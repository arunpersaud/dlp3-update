#!/usr/bin/env python3

#  dlp3.py    check packages that are being updated
#  Copyright (C) 2015-2022 Arun Persaud <arun@nubati.net>
#
#  This program is free software: you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published by
#  the Free Software Foundation, either version 3 of the License, or
#  (at your option) any later version.
#
#  This program is distributed in the hope that it will be useful,
#  but WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program.  If not, see <http://www.gnu.org/licenses/>.

"""DLP3

Usage:
dlp3.py [-n |--no-check] [-s <projectname>]

Options:
-n --no-check      Skip checking for which updates are available
-s <projectname>   Work on subproject, e.g. numeric or flask
"""

import asyncio
import cmd
import configparser
from collections import defaultdict
import concurrent.futures
from datetime import datetime
import json
import os
from pathlib import Path
import re
import subprocess
import sys
from typing import List, Dict
from aiohttp import ClientSession

import docopt
import natsort
import requests
from termcolor import colored

from dlp3.json import JsonDict, JsonList
from dlp3.create_new_spec_file import create_new_spec_file

commands = docopt.docopt(__doc__, version="dlp3.py 0.9")
subproject = commands["-s"]
if subproject is None:
    subproject = "DEFAULT"
    subproject_path = ""
else:
    subproject_path = ":" + subproject

# some tasks are done in parallel via concurrent.futures
pool = concurrent.futures.ThreadPoolExecutor(max_workers=7)

# read path information from config file
config = configparser.ConfigParser()
conf_file = config.read(["dlp3.conf", Path("~/.config/dlp3/dlp3.conf").expanduser()])

if not conf_file:
    print("Couldn't find dlp3.conf")
    print("Please create a dlp3.conf file with the following information")
    print("------")
    print("[DEFAULT]")
    print("dlp3 = <full path to your clone of dlp3>")
    print("branch = <full path to your branch location of dlp3>")
    print("------")
    sys.exit(1)

try:
    dlp3_path = Path(config[subproject]["dlp3"])
    dlp3_branch_path = Path(config[subproject]["branch"])
    dlp3_web_branch = config[subproject]["webbranch"]
    bindir = Path(__file__).resolve().parent
    logfile = JsonDict(bindir / "package-changelog-data.json")
    skipfile = JsonDict(bindir / "package-skip-data.json")
    blacklistfile = JsonList(bindir / "package-blacklist.json", name="blacklist")
    whitelistfile = JsonList(bindir / "package-whitelist.json", name="whitelist")
except (TypeError, KeyError):
    print(f"ERROR: Path for dlp3 and branch not found in {subproject} section")
    sys.exit(2)

assert dlp3_path.is_dir(), "Path to dlp3 in config file is not a directory"
assert dlp3_branch_path.is_dir(), "Path to branch in config file is not a directory"


def is_singlespec(name: str) -> bool:
    """We only want to work on singlespec, test this by looking for 'python_module()'"""
    specs = (dlp3_path / name).glob("*spec")
    out = []
    for spec in specs:
        with open(spec) as f:
            lines = f.readlines()
            singlespec = False
            for l in lines:
                if l.startswith("#"):
                    continue
                elif "python_module()" in l:
                    singlespec = True
                elif "%description" in l:
                    break
            out.append(singlespec)
    return all(out)


def get_whitelist_depends():
    """Find all dependencies for packages that are whitelisted"""
    orig = whitelistfile.load()
    depend = set()
    to_check = orig
    # get list that we need to check
    while to_check:
        p = to_check.pop()
        specs = dlp3_path.glob(f"*{p}/*spec")
        for spec in specs:
            with open(spec) as f:
                lines = f.readlines()
                singlespec = False
                for l in lines:
                    if l.startswith("#"):
                        continue
                    if "python_module()" in l:
                        singlespec = True
                    if "Requires:" in l:
                        if "oldpython" in l:
                            continue
                        if "python" in l:
                            name = l.split()
                            if "%{python_module" in name[1]:
                                name = "python-" + name[2]
                                if name.endswith("}"):
                                    name = name[:-1]
                            else:
                                name = name[1]
                            name = name.strip()
                            if "python3" in name:
                                name = name.replace("python3", "python")
                            if "python2" in name:
                                name = name.replace("python2", "python")
                            if name not in depend:
                                to_check.append(name)
                                depend.add(name)
                    if "%description" in l:
                        break

    return depend


def print_list(l: List[str], title: str = "packages:", links: bool = False):
    """print a list of packages"""
    if not l:
        print("list is empty")
    else:
        print(title)
        width = max([len(p) for p in l])
        for p in sorted(l):
            if links:
                print("  {0:<{width}} {1}".format(p, dlp3_web_branch + p, width=width))
            else:
                print("  ", p)


def my_submit(package: str):
    if not (myCMD.dir / package).is_dir():
        print(f"     WARNING: package {package} doesn't exist! skipping...")
        return None

    print("    ", package)
    worked = None
    output = subprocess.check_output(
        f"cd {myCMD.dir / package}"
        + ' && osc submitrequest --yes -m "update to latest version"',
        shell=True,
    )
    output = output.decode("utf8")
    for line in output.split("\n"):
        print(line)
        if line.startswith("created request id"):
            worked = package
            myid = line.split()[-1]
            link = "https://build.opensuse.org/request/show/" + str(myid)
            print("   link: ", link)

    return worked


def my_cleanup(package: Path) -> None:
    print("updating dlp3 checkout for", package.name)
    orig = dlp3_path / package.name
    try:
        output = subprocess.check_output(
            f"cd {orig} && osc up",
            stderr=subprocess.DEVNULL,
            shell=True,
        )
    except:
        # package didn't exist yet, create a new checkout
        output = subprocess.check_output(
            f"cd {orig} && osc co {package.name}", shell=True
        )
    print(output.decode("utf8"))


def file_search_and_replace(filename: Path, search: str, replace: str) -> None:
    """Search and replace inside a text file.

    Used, for example, to fix errors from spec-cleaner
    """
    if not filename.is_file():
        return

    text = filename.read_text()
    text = text.replace(search, replace)
    filename.write_text(text)


def my_update(package, d):
    """Branch and checkout a package, download newer version and update spec/changes file"""

    old = d[0]
    new = d[1]
    url = d[3]
    specfile = d[4]

    if str(dlp3_branch_path) in str(specfile):
        print("updating already branched package")
        os.system(f"cd {dlp3_branch_path/ package} && osc up")
    else:
        print("branching ", package)

        # cd into dpl3 package, clone and checkout
        # os.chdir is not threadsafe, so don't use it
        os.system(f"cd {dlp3_path / package} && osc branch")
        os.system(f"cd {dlp3_branch_path} && osc co {package}")

    branchdir = dlp3_branch_path / package

    # download new source
    print("downloading")
    oldpackage = url.replace("%{version}", old).split("/")[-1]

    url = url.replace("%{version}", new)
    # add new package, remove old one
    newpackage = url.split("/")[-1]

    try:
        r = requests.get(url, verify=True)
        # use absolut url to make it thread-safe
        with (branchdir / newpackage).open("wb") as f:
            f.write(r.content)
        print("download successful")
    except:
        print(f"couldn't download {package} at url {url}")
        return

    print(oldpackage, "=>", newpackage)
    os.system(f"cd {branchdir} && rm {oldpackage}")
    os.system(f"cd {branchdir} && osc addremove")

    # update version in spec file
    changelog = ""
    spec = specfile.name
    files = [spec, spec.replace(".spec", "-doc.spec")]
    for file in files:
        try:
            with (branchdir / file).open("r+") as myinput:
                content = myinput.readlines()
                myinput.seek(0)
                for line in content:
                    if "Version" in line and old in line:
                        line = line.replace(old, new)
                        # add changelog entry
                        changelog += f"- update to version {new}:"
                        changelog += "\n\n"
                    # update copyright in spec and changes files
                    if "# Copyright (c)" in line:
                        year = datetime.now().year
                        if str(year) not in line:
                            line = re.sub(
                                r"\(c\) [0-9]{4} SUSE", f"(c) {year} SUSE", line
                            )
                            # add changelog entry
                            changelog += "- specfile:\n"
                            changelog += "  * update copyright year\n\n"
                    if line.startswith("Source"):
                        if "https://pypi.python.org/packages/" in line:
                            tmp = line.split("/")
                            source = line.split(":")[0]
                            if len(tmp[4]) == 2:
                                # using direct download link, need to update it
                                line = f"{source}:         {url}"
                    myinput.write(line)
        except FileNotFoundError:
            pass
    # write changelog entries if we have any
    if changelog != "":
        # there might be two changelog files for -doc packages, just try both
        files = [
            spec.replace(".spec", ".changes"),
            spec.replace(".spec", "-doc.changes"),
        ]
        for file in files:
            try:
                with (branchdir / file).open("r+") as changes:
                    content = changes.readlines()
                    changes.seek(0)
                    changes.write(changelog)
                    for l in content:
                        changes.write(l)
            except FileNotFoundError:
                pass


def auto_complete_package_names(text: str, line: str) -> List[str]:
    """Autocomplete package names.

    readline likes to split things up when it hits a '-'
    so we need to check the last word on the line
    since 'text' could just be the text after a '-'.
    """
    lastword = line.split()[-1]
    if text:
        lastword = lastword[: -len(text)]
    # skip the beginning if we already have it on the line
    l = len(lastword)
    packages = [
        str(p.name)[l:]
        for p in myCMD.dir.iterdir()
        if p.is_dir() and p.name != ".osc" and str(p.name).startswith(lastword + text)
    ]
    return packages


class myCMD(cmd.Cmd):
    prompt = "Monitor> "
    dir = dlp3_branch_path

    def __init__(self):
        super().__init__()
        self.packages = []
        self.good_packages = []
        self.bad_packages = []
        self.dev_packages = []
        self.need_update = {}
        self.good = 0
        self.bad = 0
        self.building = 0
        self.good_total = 0
        self.bad_total = 0
        self.good_lasttotal = 0
        self.bad_lasttotal = 0
        self.longestname = 0
        self.pending_requests = []
        self.depends = []

    def do_quit(self, arg) -> bool:
        self.save("silent")
        print("Good Bye!")
        return True

    def do_bye(self, arg) -> bool:
        self.save("silent")
        print("Good Bye!")
        return True

    def do_exit(self, arg) -> bool:
        self.save("silent")
        print("Good Bye!")
        return True

    def do_add(self, arg):
        """Start monitoring the build status for the given package(s)."""
        if arg == "all":
            packages = [
                p.name for p in myCMD.dir.iterdir() if p.is_dir() and p.name != ".osc"
            ]
        else:
            packages = arg.split()
        for p in packages:
            if (myCMD.dir / p).is_dir():
                print("adding ", p)
                # do some error checking
                devel_and_noarch = 0
                subpackages = 0
                files_section = 0
                for s in (dlp3_branch_path / p).glob("*spec"):
                    with open(s) as f:
                        lines = f.readlines()
                        devel = 0
                        noarch = 0
                        for l in lines:
                            if (
                                "noarch" in l.lower()
                                and "buildarch:" in l.lower()
                                and not subpackages
                            ):
                                noarch = 1
                            if l.lower().startswith("%package"):
                                subpackages = 1
                            if "%{python_module devel}" in l.lower():
                                devel = 1
                            if (
                                "%{python_sitelib}/*" in l.lower()
                                and "dir" not in l.lower()
                            ):
                                files_section += 1
                        if noarch and devel:
                            devel_and_noarch += 1
                if devel_and_noarch or files_section:
                    if devel_and_noarch:
                        print("need to remove devel... not adding package")
                    if files_section:
                        print("need to fix %files... not adding package")
                    return
                if p not in self.packages:
                    self.packages.append(p)
                    for specname in [f"{p}.spec", f"{p}-doc.spec"]:
                        specpath = myCMD.dir / p / specname
                        if specpath.is_file():
                            output = subprocess.check_output(
                                f"cd {dlp3_branch_path/ p} && spec-cleaner -i {specname}",
                                shell=True,
                            )
                            # fix some problems with spec-cleaner
                            file_search_and_replace(
                                specpath,
                                search="%{python_libalternatives_reset_alternative}",
                                replace="%python_libalternatives_reset_alternative",
                            )
                            file_search_and_replace(
                                specpath,
                                search="%{python}-base",
                                replace="%python-base",
                            )
                    try:
                        output = subprocess.check_output(
                            f"cd {dlp3_branch_path/ p} && osc ci -n", shell=True
                        )
                    except subprocess.CalledProcessError:
                        print(f"Error: can't submit {p}")
                else:
                    print("already in list")
            else:
                print("can't find ", p)
        self.save("silent")

    def complete_add(self, text: str, line: str, begidx: int, endidx: int) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_update(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_remove(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_submit(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_addlog(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_listlog(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_ignore(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def complete_removeignore(self, text: str, line: str, begidx, endidx) -> List[str]:
        return auto_complete_package_names(text, line)

    def do_blacklist(self, arg):
        """print the blacklisted packages or adds a package to the list"""
        if arg == "":
            blacklistfile.print()
        else:
            name = arg.strip()
            print(f"Added {name} to the blacklist.")
            blacklistfile.append(name)
            blacklistfile.save()

    def do_whitelist(self, arg):
        """print the whitelisted packages or adds a package to the list"""
        if arg == "":
            whitelistfile.print()
        else:
            name = arg.strip()
            print(f"Added {name} to the whitelist.")
            whitelistfile.append(name)
            whitelistfile.save()

    def do_depend(self, arg):
        """print packages the whitelisted packages depend on"""
        depends = get_whitelist_depends()
        self.depends = depends
        if arg != "silent":
            if not depends:
                print("Currently no dependencies.")
            else:
                print("Currently whitelisted package depend on the following packages:")
                non_single = []
                for p in sorted(depends):
                    if is_singlespec(p):
                        print(f"  {p}")
                    else:
                        non_single.append(p)
                for p in non_single:
                    print(f"non-singlespec:   {p}")

    def do_ignore(self, arg):
        """print the ignore list or adds a package to the list"""
        skip = skipfile.load()
        if arg == "":
            if not skip:
                print("Currently not ignoring any packages.")
            else:
                print("Currently ignoring the following packages")
                for p in skip:
                    print(f"  {p} {skip[p]}")
        elif " " not in arg:
            if arg in skip:
                print(f"  {arg} {skip[arg]}")
            else:
                print(
                    f"Currently not ignoring {arg}. If you want to add it, "
                    + "provide a version number or '-' (for all versions)."
                )
        else:
            try:
                name, version = arg.split(" ", maxsplit=1)
                name = name.strip()
                version = version.strip()
                skipfile[name] = version
                print(f"Added {name} {version} to ignore list.")
                skipfile.save()
            except:
                print(
                    "you need to supply a package name and a version number,"
                    + " use '-' for all versions."
                )

    def do_removeignore(self, arg):
        """remove a package from the ignore list"""
        packages = arg.split()
        try:
            for p in packages:
                result = skipfile.pop(p, None)
                if result:
                    print(f"removed {p} from ignore list")
            skipfile.save()
        except:
            print("you need to supply a package name or list of package names.")

    def do_addlog(self, arg: str):
        """Save the location of a changelog file. The first argument should be
        the package name, the rest will be saved in a json file.

        """
        try:
            name, url = arg.split(" ", maxsplit=1)
            name = name.strip()
            url = url.strip(" '\"")
            logfile[name] = url
            print(f"Added log file for {name}.")
            logfile.save()
        except:
            print("you need to supply a package name and a url or string")

    def do_listlog(self, arg):
        """List the location of changelog files for the given
        packages. Without an argument show the information for all
        branched packages

        """
        logs = logfile.load()
        packages = []
        if arg == "":
            packages = [
                p.name for p in myCMD.dir.iterdir() if p.is_dir() and p.name != ".osc"
            ]
        elif arg == "all":
            packages = [p for p in logs]
        else:
            packages = arg.split()

        l = max([len(p) for p in packages])

        for i in sorted(logs):
            if i in packages:
                print("{:<{length}} {}".format(i, logs[i], length=l + 2))

    def do_cleanup(self, arg):
        """Remove package that have been updated.

        Check for packages that are still in the local branch, but
        not in the online one anymore.  These are normally the
        packages for which a SR got accepted. Update those package
        in the local checkout of dlp3 and print a command to remove
        the local copy.

        """
        try:
            output = subprocess.check_output(["osc", "list", myCMD.dir.name])
            existing = output.decode("utf8").split("\n")
        except:
            # if no packages the home-branch on osc than the command will fail
            # in this case we can remove all
            existing = []

        packages = [
            p
            for p in myCMD.dir.iterdir()
            if p.is_dir() and p.name != ".osc" and p.name not in existing
        ]

        if not packages:
            print("Nothing to clean up")
            return

        print("―" * 27)
        fut = [pool.submit(my_cleanup, p) for p in packages]
        concurrent.futures.wait(fut)
        print("―" * 27)

        for p in packages:
            name = p.name
            print("rm -rf", p)
            for package_list in [self.packages, self.good_packages, self.bad_packages]:
                if name in package_list:
                    package_list.remove(name)

        self.save("silent")

    def do_list(self, arg):
        """List all packages"""
        print_list(self.good_packages + self.packages + self.bad_packages)

    def do_links(self, arg):
        """Print links to the build server for all packages"""
        print_list(self.good_packages + self.packages + self.bad_packages, links=True)

    def do_listdev(self, arg):
        print_list(self.dev_packages, title="dev:")

    def do_good(self, arg):
        """List packages that build correclty"""
        print_list(self.good_packages, title="good:")

    def do_bad(self, arg):
        """List packages that have build problems"""
        print_list(self.bad_packages, title="bad:")

    def do_pending(self, arg):
        """List pending packages"""
        print_list(self.pending_requests, title="pending:")

    def check_package(self, p):
        cmd_dlp = f"osc results devel:languages:python{subproject_path} {p}"
        cmd_update = f"cd {myCMD.dir/ p} && osc results"

        skip_status = defaultdict(lambda: defaultdict(list))
        dlp_status = {"good": 0, "bad": 0, "building": 0, "pending": 0, "broken": 0}
        update_status = {"good": 0, "bad": 0, "building": 0, "pending": 0, "broken": 0}

        for cmd in [cmd_dlp, cmd_update]:
            # get information from orig package build status
            try:
                output = subprocess.check_output(
                    cmd, stderr=subprocess.STDOUT, shell=True
                )
                output = output.decode("utf8")
                for line in output.split("\n"):
                    try:
                        out = line.split()
                        if len(out) == 4:
                            distro, system, pname, status = out
                        else:
                            distro, system, status = out
                    except:
                        continue

                    if cmd == cmd_update:
                        # skip if original build is disabled or excluded
                        if p in skip_status:
                            if distro in skip_status[p]:
                                if system in skip_status[p][distro]:
                                    continue

                    # unify output a bit
                    if status.endswith("*"):
                        status = status[:-1]

                    # do some counting
                    # the '' exists for example when there is a problem with
                    # OBS and items don't get scheduled
                    count = None
                    if status in ["failed"]:
                        count = "bad"
                    elif status in ["broken"]:
                        count = "broken"
                    elif status in ["succeeded"]:
                        count = "good"
                    elif status in ["building", "finished", "signing"]:
                        count = "building"
                    elif status in ["scheduled", "blocked", ""]:
                        count = "pending"
                    elif status in ["excluded", "disabled", "unresolvable"]:
                        if cmd == cmd_dlp:
                            skip_status[p][distro].append(system)
                    else:
                        print(colored("unknown status", "red"), status, p)
                    if count is not None:
                        if cmd == cmd_dlp:
                            dlp_status[count] += 1
                        else:
                            update_status[count] += 1
            except subprocess.CalledProcessError as e:
                output = e.output.decode("utf8")
                if cmd == cmd_update:
                    return p, update_status, dlp_status
                else:
                    if "HTTP Error 404: unknown package" in output:
                        for k in dlp_status:
                            dlp_status[k] = -1

        if update_status["building"] + update_status["pending"] == 0:
            if update_status["bad"] == 0 and update_status["good"] > 0:
                self.good += 1
                self.good_packages.append(p)
            elif update_status["bad"] > 0:
                self.bad += 1
                self.bad_packages.append(p)
        else:
            self.building += 1

        return p, update_status, dlp_status

    def do_status(self, arg):
        """Print the build status of all packages added by the 'add' command or by 'load'."""
        self.good = 0
        self.bad = 0
        self.building = 0
        self.good_lasttotal = self.good_total
        self.bad_lasttotal = self.bad_total
        self.good_total = 0
        self.bad_total = 0
        tocheck = self.good_packages + self.bad_packages + self.packages
        tocheck = [i for i in tocheck if i not in self.pending_requests]
        self.good_packages = []
        self.bad_packages = []
        self.longestname = max([len(p) for p in tocheck]) if tocheck else 0

        # parallel check
        # local packages
        fut = [pool.submit(self.check_package, p) for p in tocheck]
        concurrent.futures.wait(fut)

        if self.longestname > 0:
            print(
                "{:^{length}}   good     bad   building  pending   broken".format(
                    "name", length=self.longestname + 2
                )
            )

        result = []
        for f in fut:
            result.append(f.result())

        for p, update_status, dlp_status in sorted(result):
            # add link in case something went wrong
            link = dlp3_web_branch + p
            # colored messes up the alignment, so we do this by hand over here
            if update_status["bad"] < 10:
                bad_out = " "
            else:
                bad_out = ""
            if update_status["bad"] > dlp_status["bad"]:
                bad_out += colored(update_status["bad"], "red")
            else:
                bad_out += str(update_status["bad"])

            if update_status["good"] < 10 and update_status["good"] > 0:
                good_out = " "
            else:
                good_out = ""
            if update_status["good"] > dlp_status["good"]:
                good_out += colored(update_status["good"], "green")
            else:
                good_out += str(update_status["good"])

            if update_status["broken"] < 10 and update_status["broken"] > 0:
                broken_out = " "
            else:
                broken_out = ""
            if update_status["broken"] > dlp_status["broken"]:
                broken_out += colored(update_status["broken"], "green")
            else:
                broken_out += str(update_status["broken"])

            if dlp_status["good"] < 0:
                print(
                    "{:<{length}}    {: >2}(--)   {}(--)   {: >2}(--)    {: >2}(--) {: >2}(--)    {link}".format(
                        p,
                        update_status["good"],
                        update_status["bad"],
                        update_status["building"],
                        update_status["pending"],
                        update_status["broken"],
                        length=self.longestname,
                        link=link,
                    )
                )
            else:
                print(
                    "{:<{length}}    {: >2}({: >2})  {}({: >2})   {: >2}({: >2})    {: >2}({: >2})   {: >2}({: >2})    {link}".format(
                        p,
                        update_status["good"],
                        dlp_status["good"],
                        bad_out,
                        dlp_status["bad"],
                        update_status["building"],
                        dlp_status["building"],
                        update_status["pending"],
                        dlp_status["pending"],
                        update_status["broken"],
                        dlp_status["broken"],
                        length=self.longestname,
                        link=link,
                    )
                )

            self.good_total += update_status["good"]
            self.bad_total += update_status["bad"]
        print("―" * (self.longestname + 47))
        print(
            "{:<{length}}    {: >+2}      {: >+2}".format(
                "ΔΣ",
                self.good_total - self.good_lasttotal,
                self.bad_total - self.bad_lasttotal,
                length=self.longestname,
            )
        )

        if self.bad or self.good or self.building:
            myCMD.prompt = "Monitor({},{},{})> ".format(
                colored(str(self.good), "green"),
                colored(str(self.bad), "red"),
                colored(str(self.building), "yellow"),
            )
        else:
            print('Nothing to check. Please, use "add" to add package to the list.')
            myCMD.prompt = "Monitor> "
        # recreate list
        self.packages = [
            p
            for p in tocheck
            if p not in self.good_packages and p not in self.bad_packages
        ]

    def emptyline(self):
        """Give status information on enter"""
        self.do_status("")

    def do_check_pending(self, arg=None):
        """Look up pending SR"""
        print("Checking my pending SR")
        try:
            output = subprocess.check_output("osc my", shell=True)
        except subprocess.CalledProcessError:
            return
        output = output.decode("utf8")

        self.pending_requests = []
        for line in output.split("\n\n"):
            if "add_role" in line:
                continue
            if "This is a humble request" in line:
                continue
            if "State:new" in line:
                tmp = line.split()
                nr = tmp[0]
                author = tmp[2][3:]  # remove 'By:'
                fromrepo = tmp[5]
                fromrepo, package = fromrepo.split("/")
                package = package.split("@")[0]
                torepo = tmp[7]
                if torepo == "devel:languages:python":
                    print(f"{nr} {package:<25} {author:<20} {fromrepo}")
                    self.pending_requests.append(package)

        # check for pending request in dlp
        print("Checking pending SR in dlp")
        try:
            output = subprocess.check_output(
                f'osc request list -s "new,review" devel:languages:python{subproject_path}',
                shell=True,
            )
        except subprocess.CalledProcessError:
            return
        output = output.decode("utf8")

        for line in output.split("\n\n"):
            if "add_role" in line:
                continue
            # skip request that e.g. go to Factory
            if "-> devel:languages:python" not in line:
                continue
            if "submit:" in line:
                tmp = line.split()
                nr = tmp[0]
                author = tmp[2][3:]  # remove 'By:'
                fromrepo = tmp[5]
                fromrepo, package = fromrepo.split("/")
                package = package.split("@")[0]
                torepo = tmp[7]
                if torepo == "devel:languages:python":
                    print(f"{nr} {package:<25} {author:<20} {fromrepo}")
                    self.pending_requests.append(package)

    def do_update(self, arg: str):
        """checkout these package to local branch, download new tar-ball,
        update changes and spec file
        """

        if arg != "":
            packages = arg.split()
            packages = [p for p in packages if p in self.need_update]
        else:
            packages = self.need_update.keys()

        if not packages:
            print(
                "Can't find any packages that need updates. Did you run 'check' first?"
            )

        fut = [pool.submit(my_update, p, self.need_update[p]) for p in packages]
        concurrent.futures.wait(fut)

        print("")
        print("Links to the changelog files:")
        self.do_listlog(arg)

    def do_check(self, arg):
        """Check for new packages on pypi. Without any arguments checks for
        all packages in dlp3, otherwise only check for the given
        packages.

        """
        logs = logfile.load()

        # list of packages to check
        show_all = False  # only show whitelisted and their dependencies
        if arg.strip() == "all":
            show_all = True
            arg = ""

        if arg != "":
            packages = arg.split()
        else:
            packages = [p.name for p in dlp3_path.glob("python*")]

        # skip pending SR
        if not self.pending_requests:
            self.do_check_pending()

        # skip packages that have a pending request
        packages = [p for p in packages if p not in self.pending_requests]

        # there are too many packages in dlp, have an option to skip patterns
        packages = [p for p in packages if p not in blacklistfile]

        # packages I'm already preparing an update for
        PENDING = [
            i.name for i in dlp3_branch_path.glob("*") if not i.name.startswith(".")
        ]

        self.dev_packages = []

        specfiles = []
        patchfiles = []
        for p in packages:
            path = dlp3_branch_path if p in PENDING else dlp3_path
            try:
                s = list((path / p).glob("*spec"))
                s = [x for x in s if not x.name.endswith("-doc.spec")][0]
                ptch = list((path / p).glob("*patch"))
                specfiles.append(s)
                patchfiles.append(ptch)
            except:
                print("Error with package", path, p)
                print("  perhaps this has been removed from dlp?")
                print("  (in which case you need to update the local copy)")
                # to keep items in sync, we still need to add something to the lists
                specfiles.append(None)
                patchfiles.append(None)

        print("checking packages:")

        # create a list of name, version, url, package that we can iterate over later
        name_version = []
        for s, p in zip(specfiles, packages):
            name, version, url = None, None, None
            # get some information out of the specfiles
            if s is None:
                name_version.append([None, None, None, None])
                continue
            with s.open() as f:
                singlespec = False
                for l in f:
                    if not version and l.startswith("Version:"):
                        version = l.split(":")[1].strip()
                    if not url and l.startswith("Source") and "version" in l:
                        url = l.split(":", maxsplit=1)[1].strip()
                        parts = l.split("/")
                        if len(parts) > 6 and (
                            parts[2] == "pypi.python.org"
                            or parts[2] == "files.pythonhosted.org"
                        ):
                            name = parts[6]
                    if not singlespec and "python_module" in l:
                        singlespec = True
                    if version and name and singlespec:
                        break
            # fix the names for some packages that are not on pypi
            if name is None:
                if len(p.split("-")) > 1:
                    name = p.split("-", maxsplit=1)[1]
            # some packages have special names
            specialnames = {"usb": "pyusb", "xdg": "pyxdg"}
            if name in specialnames:
                name = specialnames[name]
            if singlespec:
                name_version.append([name, version, url, p])
            else:
                name_version.append([None, None, None, None])

        # check version of packages in parallel
        async def fetch(url, session):
            async with session.get(url) as response:
                return (
                    await response.read(),
                    url,
                )  # response.json gave errors for some packages

        results = {}

        async def run():
            url = "https://pypi.org/pypi/{}/json"
            tasks = []
            # Fetch all responses within one Client session,
            # keep connection alive for all requests.
            async with ClientSession() as session:
                for n, v, u, p in name_version:
                    if n:
                        f = asyncio.ensure_future(fetch(url.format(n), session))
                        tasks.append(f)

                responses = await asyncio.gather(*tasks)
                for r, u in responses:
                    try:
                        r = json.loads(r)
                        if "message" in r:
                            if r["message"] == "Not Found":
                                print("Could not get response for", u)
                                continue
                        package = r["info"]["name"]
                        package_releases = list(r["releases"].keys())
                        # sort out alpha releases, etc.
                        package_releases = [p for p in package_releases if "a" not in p]
                        package_releases = [p for p in package_releases if "b" not in p]
                        package_releases = [p for p in package_releases if "c" not in p]
                        package_releases = [
                            p for p in package_releases if "dev" not in p
                        ]
                        # get latest release
                        if len(package_releases):
                            results[package] = natsort.natsorted(package_releases)[-1]
                    except json.decoder.JSONDecodeError:
                        pass

        asyncio.run(run())

        # sort list in same order as in name_version
        results_list = [
            results[n] if n in results else None for n, v, u, p in name_version
        ]

        # package everything a bit nicer
        data = [
            [v, r, n, u, s]
            for [n, v, u, p], s, r in zip(name_version, specfiles, results_list)
        ]

        good = 0
        dev = 0
        need = 0
        neednopatch = 0
        whitelist = whitelistfile.load()
        whiteout = []
        dependout = []
        self.need_update = {}
        for d, p, patch in sorted(zip(data, packages, patchfiles), key=lambda x: x[1]):
            old = d[0]
            new = d[1]
            if old is None:
                continue
            # if git or hg in old, just check for version updates
            if "+hg" in old or "+git" in old:
                old = old.split("+")[0]
            if old == new:
                good += 1
            if new is not None and (
                "dev" in new or "rc" in new or "post" in new or "git" in new
            ):
                dev += 1
                self.dev_packages.append(p + " " + new)
                continue
            ALPHA = (f"a{i}" for i in range(30))
            BETA = (f"b{i}" for i in range(30))
            GAMMA = (f"c{i}" for i in range(30))
            RELEASE_CANDIDATE = tuple(ALPHA) + tuple(BETA) + tuple(GAMMA)
            if new is not None and new.endswith(RELEASE_CANDIDATE):
                dev += 1
                self.dev_packages.append(p + " " + new)
                continue

            # check if this package is in the skip list
            skip = skipfile.load()
            extra = ""
            if p in skip:
                skipversion = skip[p]
                if skipversion == "-":
                    continue
                # if version is newer, remove from skip otherwise skip
                if new is not None and (
                    skipversion == natsort.natsorted([new, skipversion])[0]
                    and new != skipversion
                ):
                    result = skipfile.pop(p, None)
                    if result:
                        extra = " (removed from ignore list)"
                        skipfile.save()
                else:
                    continue
            if p in PENDING:
                extra = " (already branched)"

            if (
                old != new
                and new is not None
                and old == natsort.natsorted([old, new])[0]
            ):
                need += 1
                for i, c in enumerate(old):
                    if i == len(new) or new[i] != c:
                        break
                if not patch:
                    neednopatch += 1
                patchstr = colored("nopatch", "green") if not patch else "patch"
                if p in logs:
                    if logs[p].startswith("http"):
                        changelog = "h  "
                    elif logs[p].startswith("git clone"):
                        changelog = "g  "
                    else:
                        changelog = "t  "
                else:
                    changelog = "   "  # no changelog link available
                # length formatting doesn't work with color-escape
                # characters in the string, so we do it by hand
                str_out = "{}{:40}  {}{}{}{}{}".format(
                    changelog,
                    p,
                    old[:i] + colored(old[i:], "red"),
                    " " * (12 - len(old)),
                    new[:i] + colored(new[i:], "green"),
                    " " * (12 - len(new)),
                    patchstr + extra,
                )
                if any(w in p for w in whitelist):
                    whiteout.append(str_out)
                elif p in self.depends:
                    dependout.append(str_out)
                else:
                    if show_all:
                        print(str_out)
                self.need_update[p] = d
        if dependout:
            print("")
            print(
                "Whitelisted packages dependencies that",
                "need an update: ({} packages)".format(len(dependout)),
            )
            for i in dependout:
                print(i)
            print("")
        if whiteout:
            print("")
            print("Whitelisted packages that need an update:")
            for i in whiteout:
                print(i)
            print("")
        print("checking for outdated packages in branch")
        for p in PENDING:
            s1 = list((dlp3_branch_path / p).glob("*spec"))
            if s1:
                s1 = s1[0]
            else:
                print("Can't find spec file for package", p, "...skipping")
                continue
            with s1.open() as f:
                for l in f:
                    if l.startswith("Version"):
                        version1 = l.split(":")[1].strip()
                        break
            try:
                s2 = list((dlp3_path / p).glob("*spec"))[0]
                with s2.open() as f:
                    for l in f:
                        if l.startswith("Version"):
                            version2 = l.split(":")[1].strip()
                            break
            except:
                print("Error with package", path, p)
                print(
                    "Can't open dlp version...  perhaps this has been removed from dlp?"
                )
                print("  (in which case you need to update the local copy)")
                version2 = ""
            if version1 == natsort.natsorted([version1, version2])[0]:
                print(p, "local: ", version1, "  dlp: ", version2, dlp3_web_branch + p)
        print(
            f"Found {good} up to date packages,"
            + f" {dev} with a dev release, "
            + f"and {need} packages that need an update,\n"
            + f" {neednopatch} without a patch,"
            + " {} pending SR".format(len(self.pending_requests))
        )

    def do_remove(self, args: str):
        """remove package form all the internal lists"""
        for a in args.split():
            self.good_packages = [p for p in self.good_packages if p != a]
            self.bad_packages = [p for p in self.bad_packages if p != a]
            self.packages = [p for p in self.packages if p != a]

    def do_submit(self, arg: str):
        """Create SR for all packages that build correctly."""
        to_submit = arg.split()
        print("―" * 27)
        fut = [pool.submit(my_submit, p) for p in to_submit]
        concurrent.futures.wait(fut)
        worked = [f.result() for f in fut]
        print("―" * 27)

        for w in worked:
            self.pending_requests.append(w)
        self.good_packages = [p for p in self.good_packages if p not in worked]

    def do_new(self, arg):
        """Create a new package."""
        print("Creating new package")
        print("package download url:")
        url = input()
        name = url.split("/")[7]
        tarball = name.split("#")[0]
        print("got tarball:", tarball)
        name = "-".join(tarball.split("-")[:-1])
        print("got name:", name)
        version = tarball[len(name) :].split("-")[1]
        if version.endswith(".tar.gz"):
            version = version[:-7]
            ending = ".tar.gz"
        elif version.endswith(".zip"):
            version = version[:-4]
            ending = ".zip"
        else:
            print("not supported file ending")
            return
        print("got version:", version)
        print("got ending:", ending)
        subprocess.check_output(
            f"cd {myCMD.dir}" + f" && osc mkpac python-{name}", shell=True
        )
        try:
            r = requests.get(url, verify=True)
            # use absolut url to make it thread-safe
            with (dlp3_branch_path / f"python-{name}" / tarball).open("wb") as f:
                f.write(r.content)
            print("download successful")
        except:
            print("couldn't download package; url=", url)

        create_new_spec_file(dlp3_branch_path, name, version, ending)

    def save(self, arg: str):
        """Save current packages, so that we can restart the program later."""
        with (Path("~/.config/dlp3/").expanduser() / f"current-{subproject}.json").open(
            "w"
        ) as f:
            json.dump(
                self.packages + self.good_packages + self.bad_packages,
                f,
                indent=4,
                sort_keys=True,
            )
            if arg != "silent":
                print(
                    "Saved package list for next run. Use 'load' to read the list back"
                )

    def load(self, arg):
        """Load last list of packages."""
        filename = Path("~/.config/dlp3/").expanduser() / f"current-{subproject}.json"
        if not filename.is_file():
            print("No saved list of packages")
            return

        with filename.open() as f:
            self.packages = json.load(f)

        if arg != "silent":
            print("Loaded package list")

        packs = [p.name for p in myCMD.dir.iterdir()]
        for p in self.packages:
            if p not in packs:
                print(f"Package {p} doesn't exist anymore... removing it from list.")
                self.packages.remove(p)


A = myCMD()
A.load("silent")
A.do_depend("silent")
if not commands["--no-check"]:
    A.do_check("")
A.cmdloop()
