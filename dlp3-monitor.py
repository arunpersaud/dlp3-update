#!/usr/bin/env python3

#  dlp3-monitor.py   check packages that are being updated
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
import sys

import concurrent.futures
pool = concurrent.futures.ThreadPoolExecutor(max_workers=7)

# read path information from config file
config = configparser.ConfigParser()
conf_file = config.read(['dlp3-update.conf',
                         os.path.expanduser('~/.config/dlp3-update/dlp3-update.conf')])

if len(conf_file) == 0:
    print("Couldn't find dlp3-update.conf")
    print("Please create a dlp3-update.conf file witht he following information")
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
except (TypeError, KeyError):
    print("ERROR: Path for dlp3 and branch not found in DEFAULT section")
    sys.exit(2)

assert os.path.isdir(dlp3_path), "Path to dlp3 in config file is not a directory"
assert os.path.isdir(dlp3_branch_path), "Path to branch in config file is not a directory"


def print_list(l):
    """print a list of packages"""
    if len(l) == 0:
        print("list is empty")
    else:
        print("packages:")
        for p in l:
            print("  ", p)


class myCMD(cmd.Cmd):
    prompt = "Monitor> "
    dir = dlp3_branch_path

    def __init__(self):
        super().__init__()
        self.packages = []
        self.good_packages = []
        self.bad_packages = []
        self.good = 0
        self.bad = 0
        self.building = 0

    def do_quit(self, arg):
        print("Good Bye!")
        return True

    def do_bye(self, arg):
        print("Good Bye!")
        return True

    def do_add(self, arg):
        if arg == "all":
            packages = os.listdir(myCMD.dir)
            packages = [p for p in packages if os.path.isdir(p) and p != ".osc"]
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

    def do_cleanup(self, arg):
        output = subprocess.check_output(['osc', 'list', os.path.basename(myCMD.dir)])
        existing = output.decode('ascii').split('\n')

        packages = os.listdir(myCMD.dir)
        packages = [p for p in packages
                    if os.path.isdir(os.path.join(dlp3_branch_path, p))
                    and p != ".osc"
                    and p not in existing]

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
            if status in['failed', 'unresolvable']:
                bad += 1
            elif status == 'succeeded':
                good += 1
            elif status in ['scheduled', 'building', 'blocked', 'finished']:
                building += 1
            else:
                print(colored("unknown status", 'red'), status)

        print("{:<{length}}    {:2}  {:2}   {:2}".
              format(p, good,
                     colored(bad, 'red') if bad >0 else bad,
                     building,
                     length=self.longestname))

        if building == 0:
            if bad == 0:
                self.good += 1
                self.good_packages.append(p)
            else:
                self.bad += 1
                self.bad_packages.append(p)
        else:
            self.building += 1

    def do_status(self, arg):
        self.good = 0
        self.bad = 0
        self.building = 0
        tocheck = self.good_packages + self.bad_packages + self.packages
        self.good_packages = []
        self.bad_packages = []
        self.longestname = max([len(p) for p in tocheck]) if len(tocheck) >0 else 0

        # parallel check
        if self.longestname > 0:
            print("{:^{length}} good bad building".
                  format("name", length=self.longestname+2))
        fut = [pool.submit(self.check_package, p) for p in tocheck]
        concurrent.futures.wait(fut)
        if self.bad or self.good or self.building:
            myCMD.prompt = "Monitor({},{},{})> ".format(colored(str(self.good), 'green'),
                                                        colored(str(self.bad), 'red'),
                                                        colored(str(self.building), 'yellow'))
        else:
            myCMD.prompt = "Monitor> "
        # recreate list
        self.packages = [p for p in self.packages
                         if p not in self.good_packages
                         and p not in self.bad_packages]

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
            output = subprocess.check_output('cd {} && osc submitrequest --yes -m "update to latest version"'.
                                             format(os.path.join(myCMD.dir, p)),
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
