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


import cmd
import configparser
from termcolor import colored
import os
import subprocess
import json
import sys
import xmlrpc.client as xmlrpclib
import natsort
import glob
import requests

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
    bindir = os.path.dirname(os.path.realpath(__file__))
    logfile = os.path.join(bindir, 'package-changelog-data.json')
    skipfile = os.path.join(bindir, 'package-skip-data.json')
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


def get_logs():
    """return a dict with package-name->changelog location urls """
    logs = dict()
    with open(logfile, 'r') as f:
        c = "".join(f.readlines())
        if len(c) > 0:
            logs = json.loads(c)
    return logs


def print_list(l):
    """print a list of packages"""
    if len(l) == 0:
        print("list is empty")
    else:
        print("packages:")
        for p in l:
            print("  ", p)


def my_update(package, d):
    """Branch and checkout a package, download newer version and update spec/changes file"""

    old = d[0]
    new = d[1]

    print("branching ", package)

    # cd into dpl3 package, clone and checkout
    # os.chdir is not threadsafe, so don't use it
    orig_dir = os.path.join(dlp3_path, package)
    os.system("cd {} && osc branch".format(orig_dir))
    os.system("cd {} && osc co {}".format(dlp3_branch_path, package))

    branchdir = os.path.join(dlp3_branch_path, package)

    # download new source
    print("downloading")
    url = d[3].replace("%{version}", new)
    try:
        r = requests.get(url, verify=False)
        # use absolut url to make it thread-safe
        with open(os.path.join(branchdir, url.split("/")[-1]), 'wb') as f:
            f.write(r.content)
        print("download successful")
    except:
        print("couldn't download", package)
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
    with open(os.path.join(branchdir, spec), "r+") as input:
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
                if "2015" not in line:
                    line = re.sub("\(c\) [0-9]{4} SUSE",
                                  "(c) {} SUSE".format(year),
                                  line)
                    # add changelog entry
                    changelog += "- specfile:\n"
                    changelog += "  * update copyright year\n\n"
            input.write(line)
    # write changelog entries if we have any
    if changelog != "":
        file = spec.replace(".spec", ".changes")
        with open(os.path.join(branchdir, file), "r+") as changes:
            content = changes.readlines()
            changes.seek(0)
            changes.write(changelog)
            for l in content:
                changes.write(l)


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
                if os.path.isdir(os.path.join(dlp3_branch_path, p))
                and p != ".osc"
                and p.startswith(lastword+text)]
    return packages


def gen_get_name_version(specfiles):
    """Get the package name, version. and source url from a specfile"""
    for s in specfiles:
        name, version, url = None, None, None
        with open(s, 'r') as f:
            for l in f:
                if l.startswith("Version"):
                    version = l.split(":")[1].strip()
                if l.startswith("Source"):
                    url = l.split(":", maxsplit=1)[1].strip()
                    parts = l.split("/")
                    if len(parts) > 6 and parts[2] == "pypi.python.org":
                        name = parts[6]
                if version and name:
                    break
        yield name, version, url


def gen_fix_name1(l):
    for [n, v, u], p in l:
        if n is None:
            if len(p.split("-")) > 1:
                n = p.split("-", maxsplit=1)[1]
        yield n, v, u, p


def gen_fix_name2(l):
    specialnames = {'usb': 'pyusb', 'xdg': 'pyxdg'}
    for n, v, u, p in l:
        if n in specialnames:
            n = specialnames[n]
        yield n, v, u, p


class myCMD(cmd.Cmd):
    prompt = "Monitor> "
    dir = dlp3_branch_path

    def __init__(self):
        super().__init__()
        self.packages = []
        self.good_packages = []
        self.bad_packages = []
        self.need_update = []
        self.good = 0
        self.bad = 0
        self.building = 0
        self.longestname = 0

    def do_quit(self, arg):
        print("Good Bye!")
        return True

    def do_bye(self, arg):
        print("Good Bye!")
        return True

    def do_exit(self, arg):
        print("Good Bye!")
        return True

    def do_add(self, arg):
        if arg == "all":
            packages = os.listdir(myCMD.dir)
            packages = [p for p in packages
                        if os.path.isdir(os.path.join(myCMD.dir, p))
                        and p != ".osc"]
        else:
            packages = arg.split()
        for p in packages:
            if os.path.isdir(os.path.join(myCMD.dir, p)):
                print("adding ", p)
                if p not in self.packages:
                    self.packages.append(p)
                else:
                    print("already in list")
            else:
                print("can't find ", p)

    def complete_add(self, text, line, begidx, endidx):
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
        logs = get_logs()
        packages = []
        if arg == "":
            packages = os.listdir(myCMD.dir)
            packages = [p for p in packages
                        if os.path.isdir(os.path.join(myCMD.dir, p))
                        and p != ".osc"]
        elif arg == "all":
            packages = [p for p in logs]
        else:
            packages = arg.split()

        l = max([len(p) for p in packages])

        for i in logs:
            if i in packages:
                print("{:<{length}} {}".format(i, logs[i], length=l+2))

    def do_cleanup(self, arg):
        try:
            output = subprocess.check_output(['osc', 'list', os.path.basename(myCMD.dir)])
            existing = output.decode('ascii').split('\n')
        except:
            # if no packages the home-branch on osc than the command will fail
            # in this case we can remove all
            existing = []

        packages = os.listdir(myCMD.dir)
        packages = [p for p in packages
                    if os.path.isdir(os.path.join(dlp3_branch_path, p))
                    and p != ".osc"
                    and p not in existing]

        for p in packages:
            print("---------------------------------")
            print("updating dlp3 checkout for", p)
            try:
                output = subprocess.check_output('cd {} && osc up'.
                                                 format(os.path.join(dlp3_path, p)),
                                                 shell=True)
            except:
                # package didn't exist yet, create a new checkout
                output = subprocess.check_output('cd {} && osc co {}'.
                                                 format(dlp3_path, p),
                                                 shell=True)
            print(output.decode('ascii'))
        print("---------------------------------")

        for p in packages:
            print("rm -rf", os.path.join(dlp3_branch_path, p))

        if len(packages) == 0:
            print("Nothing to clean up")

    def do_list(self, arg):
        print_list(self.packages)

    def do_good(self, arg):
        print_list(self.good_packages)

    def do_bad(self, arg):
        print_list(self.bad_packages)

    def check_package(self, p):
        output = subprocess.check_output("cd {} && osc results".
                                         format(os.path.join(myCMD.dir, p)),
                                         shell=True)
        output = output.decode('ascii')
        good, bad, building = 0, 0, 0
        for line in output.split('\n'):
            try:
                out = line.split()
                distro, system, status = out
            except:
                continue

            # unify output a bit
            if status.endswith("*"):
                status = status[:-1]

            # lots of packages seem to be unresolvable in SLE_12, just ignore it
            if distro == "SLE_12" and status == "unresolvable":
                continue

            # do some counting
            # the '' exists for example when there is a problem with OBS and items don't get scheduled
            if status in['failed', 'unresolvable']:
                bad += 1
            elif status in ['succeeded']:
                good += 1
            elif status in ['scheduled', 'building', 'blocked', 'finished', '']:
                building += 1
            elif status in ['excluded']:
                pass
            else:
                print(colored("unknown status", 'red'), status)

        if building == 0:
            if bad == 0:
                self.good += 1
                self.good_packages.append(p)
            else:
                self.bad += 1
                self.bad_packages.append(p)
        else:
            self.building += 1

        return p, good, bad, building

    def do_status(self, arg):
        self.good = 0
        self.bad = 0
        self.building = 0
        tocheck = self.good_packages + self.bad_packages + self.packages
        self.good_packages = []
        self.bad_packages = []
        self.longestname = max([len(p) for p in tocheck]) if len(tocheck) > 0 else 0

        # parallel check
        if self.longestname > 0:
            print("{:^{length}} good bad building".
                  format("name", length=self.longestname+2))
        fut = [pool.submit(self.check_package, p) for p in tocheck]
        concurrent.futures.wait(fut)

        result = []
        for f in fut:
            result.append(f.result())
        for p, good, bad, building in sorted(result):
            print("{:<{length}}    {:2}  {:2}   {:2}".
                  format(p, good,
                         " "+colored(bad, 'red') if bad > 0 else bad,
                         building,
                         length=self.longestname))

        if self.bad or self.good or self.building:
            myCMD.prompt = "Monitor({},{},{})> ".format(colored(str(self.good), 'green'),
                                                        colored(str(self.bad), 'red'),
                                                        colored(str(self.building), 'yellow'))
        else:
            print('Nothing to check. Please, use "add" to add package to the list.')
            myCMD.prompt = "Monitor> "
        # recreate list
        self.packages = [p for p in self.packages
                         if p not in self.good_packages
                         and p not in self.bad_packages]

    def do_update(self, arg):
        """checkout these package to local branch, download new tar-ball,
           update changes and spec file
        """

        if arg != "":
            packages = arg.split()
            packages = [p for p in packages if p in self.need_update]
        else:
            packages = self.need_update.keys()

        fut = [pool.submit(my_update, p, self.need_update[p]) for p in packages]
        concurrent.futures.wait(fut)

    def do_check(self, arg):
        logs = get_logs()

        # list of packages to check
        if arg != "":
            packages = arg.split()
        else:
            packages = glob.glob(os.path.join(dlp3_path, "python3*"))
            packages = [os.path.basename(p) for p in packages]

        # packages I'm already preparing an update for
        PENDING = [i.split("/")[-1] for i in
                   glob.glob(dlp3_branch_path+"/*")]

        skipped = [p for p in packages if p in PENDING]
        packages = [p for p in packages if p not in PENDING]

        if len(skipped) > 0:
            print("Skipping some packages:")
            for p in skipped:
                print("  ", p)

        specfiles = [glob.glob("{}/*spec".format(os.path.join(dlp3_path, p)))[0] for p in packages]
        patchfiles = [glob.glob("{}/*patch".format(os.path.join(dlp3_path, p))) for p in packages]

        print("checking packages:")

        name_version = gen_get_name_version(specfiles)
        fix_name_version1 = gen_fix_name1(zip(name_version, packages))
        # will use this list twice, so don't use a generator for this
        fix_name_version2 = list(gen_fix_name2(fix_name_version1))

        clientpool = xmlrpclib.ServerProxy('https://pypi.python.org/pypi')
        client = xmlrpclib.MultiCall(clientpool)

        # do 60 requests at once, it doesn't work with all of them in one request
        results = []
        for i, [n, v, u, p] in enumerate(fix_name_version2):
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
                # currently roughly 360/60 = 6
                client = xmlrpclib.MultiCall(clientpool)
        results += tuple(client())

        # we really only want the latest version
        results = [r[0] if len(r) else None for r in results]

        # package everything a bit nicer
        data = [[v, r, n, u, s] for [n, v, u, p], s, r in
                zip(fix_name_version2, specfiles, results)]

        good = 0
        dev = 0
        need = 0
        neednopatch = 0
        self.need_update = {}
        for d, pp, patch in zip(data, packages, patchfiles):
            p = os.path.basename(pp)
            old = d[0]
            new = d[1]
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
                continue
            if new is not None and new.endswith(('a', 'a1', 'a2', 'a3', 'a4', 'a5',
                                                 'b', 'b1', 'b2', 'b3', 'b4', 'b5',
                                                 'c', 'c1', 'c2', 'c3', 'c4', 'c5')):
                dev += 1
                continue

            # check if we this package is in the skip list
            skip = get_skip()
            extra = ""
            if p in skip:
                skipversion = skip[p]
                if skipversion == '-':
                    continue
                # if version is newer, remove from skip otherwise skip
                if new is not None and (skipversion == natsort.versorted([new, skipversion])[0]
                                        and new != skipversion):
                    result = skip.pop(p, None)
                    if result:
                        extra = " (removed from ignore list)"
                        with open(skipfile, 'w') as f:
                            json.dump(skip, f, indent=4, sort_keys=True)
                else:
                    continue
            if old != new and new is not None and old == natsort.versorted([old, new])[0]:
                need += 1
                for i, c in enumerate(old):
                    if i == len(new) or new[i] != c:
                        break
                if not patch:
                    neednopatch += 1
                patchstr = colored("nopatch", 'green') if not patch else "patch"
                changelog = "+  " if p in logs else "   "
                # length formatting doesn't work with color-escape
                # characters in the string, so we do it by hand
                print("{}{:40}  {}{}{}{}{}".
                      format(changelog, p,
                             old[:i]+colored(old[i:], 'red'), " "*(12-len(old)),
                             new[:i]+colored(new[i:], 'green'), " "*(12-len(new)),
                             patchstr+extra))
                self.need_update[p] = d

        print("found {} up to date packages,".format(good) +
              " {} with a dev release, ".format(dev) +
              "and {} packages that need an update,".format(need) +
              " {} without a patch".format(neednopatch))

    def do_remove(self, args):
        """remove package form all the internal lists"""
        args = args.split()
        for a in args:
            self.good_packages = [p for p in self.good_packages if p != a]
            self.bad_packages = [p for p in self.bad_packages if p != a]
            self.packages = [p for p in self.packages if p != a]

    def do_submit(self, arg):
        print("submitting all the good packages")
        worked = []
        for p in self.good_packages:
            print("---------------------------------")
            print("    ", p)
            print("")
            output = subprocess.check_output('cd {}'.format(os.path.join(myCMD.dir, p)) +
                                             ' && osc submitrequest --yes -m "update to latest version"',
                                             shell=True)
            output = output.decode('ascii')
            print(output)
            print("---------------------------------")
            for line in output.split('\n'):
                if line.startswith("created request id"):
                    worked.append(p)
        self.good_packages = [p for p in self.good_packages
                              if p not in worked]

myCMD().cmdloop()