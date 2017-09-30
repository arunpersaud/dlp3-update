#!/usr/bin/env python3

#  dlp3.py    check packages that are being updated
#  Copyright (C) 2015 Arun Persaud <arun@nubati.net>
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
dlp3.py [-n |--no-check]

Options:
-n --no-check    Skip checking for which updates are available
"""

import cmd
import configparser
from collections import defaultdict
from termcolor import colored
import os
import docopt
import subprocess
import json
import sys
import xmlrpc.client as xmlrpclib
import natsort
import glob
import requests
from datetime import datetime
import re

# some tasks are done in parallel via concurrent.futures
import concurrent.futures
pool = concurrent.futures.ThreadPoolExecutor(max_workers=7)

# read path information from config file
config = configparser.ConfigParser()
conf_file = config.read(['dlp3.conf',
                         os.path.expanduser('~/.config/dlp3/dlp3.conf')])

if len(conf_file) == 0:
    print("Couldn't find dlp3.conf")
    print("Please create a dlp3.conf file with the following information")
    print("------")
    print("[DEFAULT]")
    print("dlp3 = <full path to your clone of dlp3>")
    print("branch = <full path to your branch location of dlp3>")
    print("------")
    sys.exit(1)

try:
    dlp3_path = config['DEFAULT']['dlp3']
    dlp3_branch_path = config['DEFAULT']['branch']
    dlp3_web_branch = config['DEFAULT']['webbranch']
    bindir = os.path.dirname(os.path.realpath(__file__))
    logfile = os.path.join(bindir, 'package-changelog-data.json')
    skipfile = os.path.join(bindir, 'package-skip-data.json')
    blacklistfile = os.path.join(bindir, 'package-blacklist.json')
    whitelistfile = os.path.join(bindir, 'package-whitelist.json')
except (TypeError, KeyError):
    print("ERROR: Path for dlp3 and branch not found in DEFAULT section")
    sys.exit(2)

assert os.path.isdir(dlp3_path), "Path to dlp3 in config file is not a directory"
assert os.path.isdir(dlp3_branch_path), "Path to branch in config file is not a directory"


def get_skip():
    """return a dictionary of packages that should be skipped

    The key is the package name and the value is a specific version
    number or "-" for all version.  The version number will be
    compared with natsort.
    """

    SKIP = dict()
    with open(skipfile, 'r') as f:
        c = "".join(f.readlines())
        if len(c) > 0:
            SKIP = json.loads(c)
    return SKIP


def get_blacklist():
    """return a list of packages that should be skipped

    This is just a normal list of string, if the entry matches part of
    the name, the package will be skipped during 'check'
    """

    SKIP = []
    try:
        with open(blacklistfile, 'r') as f:
            c = "".join(f.readlines())
            if len(c) > 0:
                SKIP = json.loads(c)
    except IOError:
        pass
    return SKIP


def get_whitelist():
    """return a list of packages that will be highlighted after a 'check'

    This is just a normal list of string, if the entry matches part of
    the name of the python package
    """

    white = []
    try:
        with open(whitelistfile, 'r') as f:
            c = "".join(f.readlines())
            if len(c) > 0:
                white = json.loads(c)
    except IOError:
        pass
    return white


def get_logs():
    """return a dict with package-name->changelog location urls """
    logs = dict()
    with open(logfile, 'r') as f:
        c = "".join(f.readlines())
        if len(c) > 0:
            logs = json.loads(c)
    return logs


def print_list(l, title="packages:", links=False):
    """print a list of packages"""
    if len(l) == 0:
        print("list is empty")
    else:
        print(title)
        width = max([len(p) for p in l])
        for p in sorted(l):
            if links:
                print("  {0:<{width}} {1}".format(p, dlp3_web_branch+p,
                                                  width=width))
            else:
                print("  ", p)


def my_submit(package):
    print("    ", package)

    worked = None
    output = subprocess.check_output('cd {}'.format(os.path.join(myCMD.dir, package)) +
                                     ' && osc submitrequest --yes -m "update to latest version"',
                                     shell=True)
    output = output.decode('utf8')
    for line in output.split('\n'):
        print(line)
        if line.startswith("created request id"):
            worked = package
            id = line.split()[-1]
            link = "https://build.opensuse.org/request/show/"+str(id)
            print("   link: ", link)

    return worked


def my_cleanup(package):
    print("updating dlp3 checkout for", package)
    try:
        output = subprocess.check_output('cd {} && osc up'.
                                         format(os.path.join(dlp3_path, package)),
                                         stderr=subprocess.DEVNULL,
                                         shell=True)
    except:
        # package didn't exist yet, create a new checkout
        output = subprocess.check_output('cd {} && osc co {}'.
                                         format(dlp3_path, package),
                                         shell=True)
    print(output.decode('utf8'))


def my_update(package, d):
    """Branch and checkout a package, download newer version and update spec/changes file"""

    old = d[0]
    new = d[1]

    specfile = d[4]

    if dlp3_branch_path in specfile:
        print("updating already branched package")
        os.system("cd {} && osc up".format(os.path.join(dlp3_branch_path, package)))
    else:
        print("branching ", package)

        # cd into dpl3 package, clone and checkout
        # os.chdir is not threadsafe, so don't use it
        os.system("cd {} && osc branch".format(os.path.join(dlp3_path, package)))
        os.system("cd {} && osc co {}".format(dlp3_branch_path, package))

    branchdir = os.path.join(dlp3_branch_path, package)

    # download new source
    print("downloading")
    url = d[3].replace("%{version}", new)
    try:
        r = requests.get(url, verify=True)
        # use absolut url to make it thread-safe
        with open(os.path.join(branchdir, url.split("/")[-1]), 'wb') as f:
            f.write(r.content)
        print("download successful")
    except:
        print("couldn't download", package, "at url", url)
        return

    # add new package, remove old one
    newpackage = url.split("/")[-1]
    oldpackage = d[3].replace("%{version}", old).split("/")[-1]
    print(oldpackage, "=>", newpackage)
    os.system("cd {} && rm {}".format(branchdir, oldpackage))
    os.system("cd {} && osc addremove".format(branchdir))

    # update version in spec file
    changelog = ""
    spec = d[4].split("/")[-1]
    files = [spec, spec.replace(".spec", "-doc.spec")]
    for file in files:
        try:
            with open(os.path.join(branchdir, file), "r+") as input:
                content = input.readlines()
                input.seek(0)
                for line in content:
                    if "Version" in line and old in line:
                        line = line.replace(old, new)
                        # add changelog entry
                        changelog += "- update to version {}:".format(new)
                        changelog += "\n\n"
                    # update copyright in spec and changes files
                    if "# Copyright (c)" in line:
                        year = datetime.now().year
                        if str(year) not in line:
                            line = re.sub("\(c\) [0-9]{4} SUSE",
                                          "(c) {} SUSE".format(year),
                                          line)
                            # add changelog entry
                            changelog += "- specfile:\n"
                            changelog += "  * update copyright year\n\n"
                    if line.startswith("Source"):
                        if "https://pypi.python.org/packages/" in line:
                            tmp = line.split('/')
                            source = line.split(':')[0]
                            if len(tmp[4]) == 2:
                                # using direct download link, need to update it
                                line = "{}:         {}".format(source, url)
                    input.write(line)
        except FileNotFoundError:
            pass
    # write changelog entries if we have any
    if changelog != "":
        # there might be two changelog files for -doc packages, just try both
        files = [spec.replace(".spec", ".changes"), spec.replace(".spec", "-doc.changes")]
        for file in files:
            try:
                with open(os.path.join(branchdir, file), "r+") as changes:
                    content = changes.readlines()
                    changes.seek(0)
                    changes.write(changelog)
                    for l in content:
                        changes.write(l)
            except FileNotFoundError:
                pass


def auto_complete_package_names(text, line):
    """autocomplete package names

    readline likes to split things up when it hits a '-'
    so we need to check the last word on the line and
    since 'text' could just be the text after a '-'.
    """

    lastword = line.split()[-1]
    if len(text) > 0:
        lastword = lastword[:-len(text)]
    # skip the beginning if we already have it on the line
    l = len(lastword)
    packages = [p[l:] for p in os.listdir(myCMD.dir)
                if os.path.isdir(os.path.join(dlp3_branch_path, p)) and
                p != ".osc" and
                p.startswith(lastword+text)]
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

    def do_quit(self, arg):
        self.save('silent')
        print("Good Bye!")
        return True

    def do_bye(self, arg):
        self.save('silent')
        print("Good Bye!")
        return True

    def do_exit(self, arg):
        self.save('silent')
        print("Good Bye!")
        return True

    def do_add(self, arg):
        """Start monitoring the build status for the given package(s)."""
        if arg == "all":
            packages = os.listdir(myCMD.dir)
            packages = [p for p in packages
                        if os.path.isdir(os.path.join(myCMD.dir, p)) and
                        p != ".osc"]
        else:
            packages = arg.split()
        for p in packages:
            if os.path.isdir(os.path.join(myCMD.dir, p)):
                print("adding ", p)
                if p not in self.packages:
                    self.packages.append(p)
                    output = subprocess.check_output('cd {} && osc ci -n'.
                                                     format(os.path.join(dlp3_branch_path, p)),
                                                     shell=True)
                else:
                    print("already in list")
            else:
                print("can't find ", p)
        self.save('silent')

    def complete_add(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_update(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_remove(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_submit(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_addlog(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_listlog(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_ignore(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def complete_removeignore(self, text, line, begidx, endidx):
        return auto_complete_package_names(text, line)

    def do_blacklist(self, arg):
        """print the blacklisted packages or adds a package to the list"""
        skip = get_blacklist()
        if arg == "":
            if len(skip) == 0:
                print("Currently not blacklisting any packages.")
            else:
                print("Currently blacklisting the following packages")
                for p in skip:
                    print("  {}".format(p))
        else:
            name = arg.strip()
            print("Added {} to the blacklist.".format(name))
            if len(skip) == 0:
                skip = [name]
            else:
                skip.append(name)
            with open(blacklistfile, 'w') as f:
                json.dump(skip, f, indent=4, sort_keys=True)

    def do_whitelist(self, arg):
        """print the whitelisted packages or adds a package to the list"""
        white = get_whitelist()
        if arg == "":
            if len(white) == 0:
                print("Currently not whitelisting any packages.")
            else:
                print("Currently whitelisting the following packages")
                for p in white:
                    print("  {}".format(p))
        else:
            name = arg.strip()
            print("Added {} to the whitelist.".format(name))
            if len(white) == 0:
                white = [name]
            else:
                white.append(name)
            with open(whitelistfile, 'w') as f:
                json.dump(white, f, indent=4, sort_keys=True)

    def do_ignore(self, arg):
        """print the ignore list or adds a package to the list"""
        skip = get_skip()
        if arg == "":
            if len(skip) == 0:
                print("Currently not ignoring any packages.")
            else:
                print("Currently ignoring the following packages")
                for p in skip:
                    print("  {} {}".format(p, skip[p]))
        elif " " not in arg:
            if arg in skip:
                print("  {} {}".format(arg, skip[arg]))
            else:
                print("Currently not ignoring {}. If you want to add it, ".format(arg) +
                      "provide a version number or '-' (for all versions).")
        else:
            try:
                name, version = arg.split(" ", maxsplit=1)
                name = name.strip()
                version = version.strip()
                skip[name] = version
                print("Added {} {} to ignore list.".format(name, version))
                with open(skipfile, 'w') as f:
                    json.dump(skip, f, indent=4, sort_keys=True)
            except:
                print("you need to supply a package name and a version number," +
                      " use '-' for all versions.")

    def do_removeignore(self, arg):
        """remove a package from the ignore list"""
        skip = get_skip()
        packages = arg.split()
        try:
            for p in packages:
                result = skip.pop(p, None)
                if result:
                    print("removed {} from ignore list".format(p))
            with open(skipfile, 'w') as f:
                json.dump(skip, f, indent=4, sort_keys=True)
        except:
            print("you need to supply a package name or list of package names.")

    def do_addlog(self, arg):
        """Save the location of a changelog file. The first argument should be
           the package name, the rest will be saved in a json file.

        """
        logs = get_logs()
        try:
            name, url = arg.split(" ", maxsplit=1)
            name = name.strip()
            url = url.strip(' \'"')
            logs[name] = url
            print("Added log file for {}.".format(name))
            with open(logfile, 'w') as f:
                json.dump(logs, f, indent=4, sort_keys=True)
        except:
            print("you need to supply a package name and a url or string")

    def do_listlog(self, arg):
        """List the location of changelog files for the given
           packages. Without an argument show the information for all
           branched packages

        """
        logs = get_logs()
        packages = []
        if arg == "":
            packages = os.listdir(myCMD.dir)
            packages = [p for p in packages
                        if os.path.isdir(os.path.join(myCMD.dir, p)) and
                        p != ".osc"]
        elif arg == "all":
            packages = [p for p in logs]
        else:
            packages = arg.split()

        l = max([len(p) for p in packages])

        for i in sorted(logs):
            if i in packages:
                print("{:<{length}} {}".format(i, logs[i], length=l+2))

    def do_cleanup(self, arg):
        """Remove package that have been updated.

           Check for packages that are still in the local branch, but
           not in the online one anymore.  These are normally the
           packages for which a SR got accepted. Update those package
           in the local checkout of dlp3 and print a command to remove
           the local copy.

        """
        try:
            output = subprocess.check_output(['osc', 'list', os.path.basename(myCMD.dir)])
            existing = output.decode('utf8').split('\n')
        except:
            # if no packages the home-branch on osc than the command will fail
            # in this case we can remove all
            existing = []

        packages = os.listdir(myCMD.dir)
        packages = [p for p in packages
                    if os.path.isdir(os.path.join(dlp3_branch_path, p)) and
                    p != ".osc" and
                    p not in existing]

        if len(packages) == 0:
            print("Nothing to clean up")
            return

        print("―"*27)
        fut = [pool.submit(my_cleanup, p) for p in packages]
        concurrent.futures.wait(fut)
        print("―"*27)

        for p in packages:
            print("rm -rf", os.path.join(dlp3_branch_path, p))
            if p in self.packages:
                self.packages.remove(p)
            if p in self.good_packages:
                self.good_packages.remove(p)
            if p in self.bad_packages:
                self.bad_packages.remove(p)

        self.save('silent')

    def do_list(self, arg):
        print_list(self.good_packages+self.packages+self.bad_packages)

    def do_links(self, arg):
        print_list(self.good_packages+self.packages+self.bad_packages, links=True)

    def do_listdev(self, arg):
        print_list(self.dev_packages, title="dev:")

    def do_good(self, arg):
        print_list(self.good_packages, title="good:")

    def do_bad(self, arg):
        print_list(self.bad_packages, title="bad:")

    def do_pending(self, arg):
        print_list(self.pending_requests, title="pending:")

    def check_package(self, p):
        # get information from orig package build status
        try:
            output = subprocess.check_output("osc results devel:languages:python {}".
                                             format(p),
                                             shell=True)
            output = output.decode('utf8')
            skip_status = defaultdict(lambda: defaultdict(list))
            orig_good = 0
            orig_bad = 0
            orig_building = 0
            orig_pending = 0
            for line in output.split('\n'):
                try:
                    out = line.split()
                    if len(out) == 4:
                        distro, system, pname, status = out
                    else:
                        distro, system, status = out
                except:
                    continue

                # no python3, so we get many errors here
                if distro == "SLE_11_SP4" and status == "unresolvable":
                    continue

                # unify output a bit
                if status.endswith("*"):
                    status = status[:-1]

                # do some counting
                # the '' exists for example when there is a problem with
                # OBS and items don't get scheduled
                if status in['failed']:
                    orig_bad += 1
                elif status in ['succeeded']:
                    orig_good += 1
                elif status in ['building', 'finished', 'signing']:
                    orig_building += 1
                elif status in ['scheduled', 'blocked', '']:
                    orig_pending += 1
                elif status in ['excluded', 'disabled', 'unresolvable']:
                    skip_status[p][distro].append(system)
                else:
                    print(colored("unknown status", 'red'), status)
        except:
            skip_status = {}
            orig_good, orig_bad, orig_building, orig_pending = 0, 0, 0, 0

        try:
            output = subprocess.check_output("cd {} && osc results".
                                             format(os.path.join(myCMD.dir, p)),
                                             shell=True)
        except subprocess.CalledProcessError:
            # package doesn't exist anymore, return all zeros and remove from list in caller
            # since this is executed in a thread and won't udate the real class in the main thread
            return p, (0, 0, 0, 0), (0, 0, 0, 0)
        output = output.decode('utf8')
        good, bad, building, pending = 0, 0, 0, 0
        for line in output.split('\n'):
            try:
                out = line.split()
                if len(out) == 4:
                    distro, system, pname, status = out
                else:
                    distro, system, status = out
            except:
                continue

            # skip if original build is disabled or excluded
            if p in skip_status:
                if distro in skip_status[p]:
                    if system in skip_status[p][distro]:
                        continue

            # no python3, so we get many errors here
            if distro == "SLE_11_SP4" and status == "unresolvable":
                continue

            # unify output a bit
            if status.endswith("*"):
                status = status[:-1]

            # do some counting
            # the '' exists for example when there is a problem with
            # OBS and items don't get scheduled
            if status in['failed', 'unresolvable']:
                bad += 1
            elif status in ['succeeded']:
                good += 1
            elif status in ['building', 'finished', 'signing']:
                building += 1
            elif status in ['scheduled', 'blocked', '']:
                pending += 1
            elif status in ['excluded', 'disabled']:
                pass
            else:
                print(colored("unknown status", 'red'), status)

        if building+pending == 0:
            if bad == 0 and good > 0:
                self.good += 1
                self.good_packages.append(p)
            elif bad > 0:
                self.bad += 1
                self.bad_packages.append(p)
        else:
            self.building += 1

        return p, (good, bad, building, pending), (orig_good, orig_bad, orig_building, orig_pending)

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
        self.longestname = max([len(p) for p in tocheck]) if len(tocheck) > 0 else 0

        # parallel check
        # local packages
        fut = [pool.submit(self.check_package, p) for p in tocheck]
        concurrent.futures.wait(fut)

        if self.longestname > 0:
            print("{:^{length}}   good     bad   building  pending".
                  format("name", length=self.longestname+2))

        result = []
        for f in fut:
            result.append(f.result())

        for p, (good, bad, building, pending), (o_good, o_bad, o_building, o_pending) in sorted(result):
            if good == 0 and bad == 0 and building == 0 and pending == 0:
                print("Package {} doesn't seem to exist anymore... removed it from the list".format(p))
                if p in self.good_packages:
                    self.good_packages.remove(p)
                if p in self.bad_packages:
                    self.bad_packages.remove(p)
                if p in self.packages:
                    self.packages.remove(p)

            # add link in case something went wrong
            link = dlp3_web_branch+p if bad > 0 else ""
            # colored messes up the alignment, so we do this by hand over here
            if bad < 10:
                bad_out = " "
                bad_out += colored(bad, 'red') if bad > o_bad else str(bad)
            else:
                bad_out = colored(bad, 'red') if bad > o_bad else bad
            if good < 10 and good > 0:
                good_out = " "
                good_out += colored(good, 'green') if good >= o_good else str(good)
            else:
                good_out = colored(good, 'green') if good >= o_good else good
            print("{:<{length}}    {: >2}({: >2})  {}({: >2})   {: >2}({: >2})    {: >2}({: >2})    {link}".
                  format(p,
                         good, o_good,
                         bad_out, o_bad,
                         building, o_building,
                         pending, o_pending,
                         length=self.longestname, link=link))
            self.good_total += good
            self.bad_total += bad
        print("―"*(self.longestname+37))
        print("{:<{length}}    {: >+2}      {: >+2}".
              format("ΔΣ", self.good_total-self.good_lasttotal,
                     self.bad_total-self.bad_lasttotal, length=self.longestname))

        if self.bad or self.good or self.building:
            myCMD.prompt = "Monitor({},{},{})> ".format(colored(str(self.good), 'green'),
                                                        colored(str(self.bad), 'red'),
                                                        colored(str(self.building), 'yellow'))
        else:
            print('Nothing to check. Please, use "add" to add package to the list.')
            myCMD.prompt = "Monitor> "
        # recreate list
        self.packages = [p for p in tocheck
                         if p not in self.good_packages and
                         p not in self.bad_packages]

    def emptyline(self):
        """Give status information on enter"""
        self.do_status('')

    def do_check_pending(self, arg=None):
        print("Checking my pending SR")
        try:
            output = subprocess.check_output("osc my", shell=True)
        except subprocess.CalledProcessError:
            return
        output = output.decode('utf8')

        self.pending_requests = []
        for line in output.split('\n\n'):
            if "add_role" in line:
                continue
            if "State:new" in line:
                tmp = line.split()
                nr = tmp[0]
                author = tmp[2][3:]  # remove 'By:'
                fromrepo = tmp[5]
                fromrepo, package = fromrepo.split("/")
                package = package.split('@')[0]
                torepo = tmp[7]
                if torepo == "devel:languages:python":
                    print("{} {:<25} {:<20} {}".format(nr, package, author, fromrepo))
                    self.pending_requests.append(package)

        # check for pending request in dlp
        print("Checking pending SR in dlp")
        try:
            output = subprocess.check_output(
                'osc request list -s "new,review" devel:languages:python', shell=True)
        except subprocess.CalledProcessError:
            return
        output = output.decode('utf8')

        for line in output.split('\n\n'):
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
                package = package.split('@')[0]
                torepo = tmp[7]
                if torepo == "devel:languages:python":
                    print("{} {:<25} {:<20} {}".format(nr, package, author, fromrepo))
                    self.pending_requests.append(package)

    def do_update(self, arg):
        """checkout these package to local branch, download new tar-ball,
           update changes and spec file
        """

        if arg != "":
            packages = arg.split()
            packages = [p for p in packages if p in self.need_update]
        else:
            packages = self.need_update.keys()

        if len(packages) == 0:
            print("Can't find any packages that need updates. Did you run 'check' first?")

        fut = [pool.submit(my_update, p, self.need_update[p]) for p in packages]
        concurrent.futures.wait(fut)

        print('')
        print('Links to the changelog files:')
        self.do_listlog(arg)

    def do_check(self, arg):
        """Check for new packages on pypi. Without any arguments checks for
           all packages in dlp3, otherwise only check for the given
           packages.

        """
        logs = get_logs()

        # list of packages to check
        if arg != "":
            packages = arg.split()
        else:
            packages = glob.glob(os.path.join(dlp3_path, "python*"))
            packages = [os.path.basename(p) for p in packages]

        # skip pending SR
        if not self.pending_requests:
            self.do_check_pending()

        # skip packages that have a pending request
        packages = [p for p in packages if p not in self.pending_requests]

        # there are too many packages in dlp, have an option to skip patterns
        blacklist = get_blacklist()
        for black in blacklist:
            packages = [p for p in packages if black not in p]

        # packages I'm already preparing an update for
        PENDING = [i.split("/")[-1] for i in
                   glob.glob(dlp3_branch_path+"/*")]

        self.dev_packages = []

        specfiles = []
        patchfiles = []
        for p in packages:
            path = dlp3_branch_path if p in PENDING else dlp3_path
            try:
                s = glob.glob("{}/*spec".format(os.path.join(path, p)))
                s = [p for p in s if not p.endswith('-doc.spec')][0]
                p = glob.glob("{}/*patch".format(os.path.join(path, p)))
                specfiles.append(s)
                patchfiles.append(p)
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
            with open(s, 'r') as f:
                singlespec = False
                for l in f:
                    if not version and l.startswith("Version:"):
                        version = l.split(":")[1].strip()
                    if not url and l.startswith("Source:") and "version" in l:
                        url = l.split(":", maxsplit=1)[1].strip()
                        parts = l.split("/")
                        if len(parts) > 6 and (parts[2] == "pypi.python.org" or parts[2] == "files.pythonhosted.org"):
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
            specialnames = {'usb': 'pyusb', 'xdg': 'pyxdg'}
            if name in specialnames:
                name = specialnames[name]
            if singlespec:
                name_version.append([name, version, url, p])
            else:
                name_version.append([None, None, None, None])

        clientpool = xmlrpclib.ServerProxy('https://pypi.python.org/pypi')
        client = xmlrpclib.MultiCall(clientpool)

        # do 60 requests at once, it doesn't work with all of them in one request
        results = []
        for i, [n, v, u, p] in enumerate(name_version):
            if n is not None:
                client.package_releases(n)
            else:
                client.package_releases('nonexistingdummy')
            if not i % 60:
                results += tuple(client())
                # tried to parallize this with a concurrent pool, but they either all need
                # their own client version (e.g. tcp connection) or it wouldn't work
                # that is xmlrpclib.ServerProxy is not threadsafe
                # this way should be faster as long as we only need a handful of connections
                # currently roughly 360 packages/60 = 6
                client = xmlrpclib.MultiCall(clientpool)
        results += tuple(client())

        # we really only want the latest version
        results = [r[0] if len(r) else None for r in results]

        # package everything a bit nicer
        data = [[v, r, n, u, s] for [n, v, u, p], s, r in
                zip(name_version, specfiles, results)]

        good = 0
        dev = 0
        need = 0
        neednopatch = 0
        whitelist = get_whitelist()
        whiteout = []
        self.need_update = {}
        for d, pp, patch in sorted(zip(data, packages, patchfiles), key=lambda x: x[1]):
            p = os.path.basename(pp)
            old = d[0]
            new = d[1]
            if old is None:
                continue
            # if git or hg in old, just check for version updates
            if "+hg" in old or "+git" in old:
                old = old.split("+")[0]
            if old == new:
                good += 1
            if new is not None and ("dev" in new or
                                    "rc" in new or
                                    "post" in new or
                                    "git" in new):
                dev += 1
                self.dev_packages.append(p+" "+new)
                continue
            if new is not None and new.endswith(('a', 'a0', 'a1', 'a2', 'a3', 'a4', 'a5', 'a6',
                                                 'b', 'b0', 'b1', 'b2', 'b3', 'b4', 'b5', 'b6',
                                                 'c', 'c0', 'c1', 'c2', 'c3', 'c4', 'c5', 'c6')):
                dev += 1
                self.dev_packages.append(p+" "+new)
                continue

            # check if this package is in the skip list
            skip = get_skip()
            extra = ""
            if p in skip:
                skipversion = skip[p]
                if skipversion == '-':
                    continue
                # if version is newer, remove from skip otherwise skip
                if new is not None and (skipversion == natsort.natsorted([new, skipversion])[0] and
                                        new != skipversion):
                    result = skip.pop(p, None)
                    if result:
                        extra = " (removed from ignore list)"
                        with open(skipfile, 'w') as f:
                            json.dump(skip, f, indent=4, sort_keys=True)
                else:
                    continue
            if p in PENDING:
                extra = " (already branched)"
            if old != new and new is not None and old == natsort.natsorted([old, new])[0]:
                need += 1
                for i, c in enumerate(old):
                    if i == len(new) or new[i] != c:
                        break
                if not patch:
                    neednopatch += 1
                patchstr = colored("nopatch", 'green') if not patch else "patch"
                if p in logs:
                    if logs[p].startswith('http'):
                        changelog = "h  "
                    elif logs[p].startswith('git clone'):
                        changelog = "g  "
                    else:
                        changelog = "t  "
                else:
                    changelog = "   "  # no changelog link available
                # length formatting doesn't work with color-escape
                # characters in the string, so we do it by hand
                str_out = "{}{:40}  {}{}{}{}{}".format(changelog, p,
                                                       old[:i]+colored(old[i:], 'red'), " "*(12-len(old)),
                                                       new[:i]+colored(new[i:], 'green'), " "*(12-len(new)),
                                                       patchstr+extra)
                print(str_out)
                if p in whitelist:
                    whiteout.append(str_out)
                self.need_update[p] = d
        if whiteout:
            print("")
            print("Whitelisted packages that need an update:")
            for i in whiteout:
                print(i)
            print("")
        print("checking for outdated packages in branch")
        for p in PENDING:
            try:
                s1 = glob.glob("{}/*spec".format(os.path.join(dlp3_branch_path, p)))[0]
                s2 = glob.glob("{}/*spec".format(os.path.join(dlp3_path, p)))[0]
            except:
                print("Error with package", path, p)
                print("  perhaps this has been removed from dlp?")
                print("  (in which case you need to update the local copy)")
            with open(s1, 'r') as f:
                for l in f:
                    if l.startswith("Version"):
                        version1 = l.split(":")[1].strip()
                        break
            with open(s2, 'r') as f:
                for l in f:
                    if l.startswith("Version"):
                        version2 = l.split(":")[1].strip()
                        break
            if version1 == natsort.natsorted([version1, version2])[0]:
                print(p, "local: ", version1, "  dlp: ", version2, dlp3_web_branch+p)
        print("Found {} up to date packages,".format(good) +
              " {} with a dev release, ".format(dev) +
              "and {} packages that need an update,\n".format(need) +
              " {} without a patch,".format(neednopatch) +
              " {} pending SR".format(len(self.pending_requests)))

    def do_remove(self, args):
        """remove package form all the internal lists"""
        args = args.split()
        for a in args:
            self.good_packages = [p for p in self.good_packages if p != a]
            self.bad_packages = [p for p in self.bad_packages if p != a]
            self.packages = [p for p in self.packages if p != a]

    def do_submit(self, arg):
        """Create SR for all packages that build correctly."""
        worked = []
        if arg.startswith('-f'):
            to_submit = arg.split()[1:]
        else:
            print("submitting all the good packages")
            to_submit = self.good_packages
        print("―"*27)
        fut = [pool.submit(my_submit, p) for p in to_submit]
        concurrent.futures.wait(fut)
        worked = [f.result() for f in fut]
        print("―"*27)

        for w in worked:
            self.pending_requests.append(w)
        self.good_packages = [p for p in self.good_packages
                              if p not in worked]

    def do_new(self, arg):
        """Create a new package."""
        print("Creating new package")
        print("package download url:")
        url = input()
        name = url.split('/')[7]
        tarball = name.split('#')[0]
        print('got tarball:', tarball)
        name = '-'.join(tarball.split('-')[:-1])
        print('got name:', name)
        version = tarball[len(name):].split('-')[1]
        if version.endswith('.tar.gz'):
            version = version[:-7]
            ending = '.tar.gz'
        elif version.endswith('.zip'):
            version = version[:-4]
            ending = '.zip'
        else:
            print("not supported file ending")
            return
        print('got version:', version)
        print('got ending:', ending)
        subprocess.check_output('cd {}'.format(myCMD.dir) +
                                ' && osc mkpac python-{}'.format(name),
                                shell=True)
        try:
            r = requests.get(url, verify=True)
            # use absolut url to make it thread-safe
            with open(os.path.join(dlp3_branch_path, 'python-{}'.format(name), tarball), 'wb') as f:
                f.write(r.content)
            print("download successful")
        except:
            print("couldn't download package; url=", url)

        with open(os.path.join(dlp3_branch_path, 'python-{}'.format(name), 'python-{}.spec'.format(name)), 'w') as f:
            f.write('#\n')
            f.write('# spec file for package python-{}\n'.format(name))
            f.write('#\n')
            f.write('# Copyright (c) {} SUSE LINUX GmbH, Nuernberg, Germany.\n'.format(datetime.now().year))
            f.write('#\n')
            f.write('# All modifications and additions to the file contributed by third parties\n')
            f.write('# remain the property of their copyright owners, unless otherwise agreed\n')
            f.write('# upon. The license for this file, and modifications and additions to the\n')
            f.write('# file, is the same license as for the pristine package itself (unless the\n')
            f.write('# license for the pristine package is not an Open Source License, in which\n')
            f.write('# case the license is the MIT License). An "Open Source License" is a\n')
            f.write('# license that conforms to the Open Source Definition (Version 1.9)\n')
            f.write('# published by the Open Source Initiative.\n')
            f.write('\n')
            f.write('# Please submit bugfixes or comments via http://bugs.opensuse.org/\n')
            f.write('#\n')
            f.write('\n')
            f.write('\n')
            f.write('Name:           python3-{}\n'.format(name))
            f.write('Version:        {}\n'.format(version))
            f.write('Release:        0\n')
            f.write('Summary:        \n')
            f.write('License:        \n')
            f.write('Group:          Development/Languages/Python\n')
            f.write('Url:            \n')
            f.write(
                'Source:         https://files.pythonhosted.org/packages/source/{}/{}/{}-%{{version}}{}\n'.format(name[
                    0], name, name, ending))
            f.write('BuildRequires:  python3-devel\n')
            f.write('BuildRequires:  python3-setuptools\n')
            f.write('BuildRoot:      %{_tmppath}/%{name}-%{version}-build\n')
            f.write('BuildArch:      noarch\n')
            f.write('\n')
            f.write('%description\n')
            f.write('\n')
            f.write('\n')
            f.write('%prep\n')
            f.write('%setup -q -n {}-%{{version}}\n'.format(name))
            f.write('\n')
            f.write('%build\n')
            f.write('python3 setup.py build\n')
            f.write('\n')
            f.write('%install\n')
            f.write('python3 setup.py install --prefix=%{_prefix} --root=%{buildroot}\n')
            f.write('\n')
            f.write('%files\n')
            f.write('%defattr(-,root,root,-)\n')
            f.write('%{python3_sitelib}/*\n')
            f.write('%{{_bindir}}/{}\n'.format(name))
            f.write('%doc README.rst\n')
            f.write('\n')
            f.write('%changelog\n')

    def save(self, arg):
        """Save current packages, so that we can restart the program later."""
        with open(os.path.join(os.path.expanduser('~/.config/dlp3/'), 'current.json'), 'w') as f:
            json.dump(self.packages+self.good_packages+self.bad_packages, f, indent=4, sort_keys=True)
            if arg != 'silent':
                print("Saved package list for next run. Use 'load' to read the list back")

    def load(self, arg):
        """Load last list of packages."""
        with open(os.path.join(os.path.expanduser('~/.config/dlp3/'), 'current.json'), 'r') as f:
            c = "".join(f.readlines())
            if len(c) > 0:
                self.packages = json.loads(c)
            if arg != 'silent':
                print("Loaded package list")

            packs = os.listdir(myCMD.dir)
            for p in self.packages:
                if p not in packs:
                    print("Package {} doesn't exist anymore... remove it from list.")
                    self.packages.remove(p)


commands = docopt.docopt(__doc__, version='dlp3.py 0.9')
A = myCMD()
A.load('silent')
if not commands['--no-check']:
    A.do_check('')
A.cmdloop()
