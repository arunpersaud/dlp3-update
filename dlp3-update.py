#!/usr/bin/env python3

#  dlp3-update.py   check for packages that need updates
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

"""Usage: checkver check [--error] ([<package>]...)
          checkver update (<package>...)
          checkver addlog <package> <url>
          checkver listlog [<package>]
"""

import concurrent.futures
import configparser
from datetime import datetime
import docopt
import glob
import json
import natsort
import re
import os
import requests
import sys
from termcolor import colored
import xmlrpc.client as xmlrpclib

commands = docopt.docopt(__doc__, version="1.0")

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
    SKIP = config['DEFAULT'].get('skip')
    if SKIP is None:
        SKIP = []
    else:
        SKIP = [s.strip() for s in SKIP.split(",")]
except (TypeError, KeyError):
    print("ERROR: Path for dlp3 and branch not found in DEFAULT section")
    sys.exit(2)

assert os.path.isdir(dlp3_path), "Path to dlp3 in config file is not a directory"
assert os.path.isdir(dlp3_branch_path), "Path to branch in config file is not a directory"

# list of packages to check
if commands['<package>']:
    packages = commands['<package>']
else:
    packages = glob.glob(os.path.join(dlp3_path, "python3*"))

logs = dict()
with open(logfile, 'r') as f:
    c = "".join(f.readlines())
    if len(c) > 0:
        logs = json.loads(c)

if commands['listlog']:
    for i in logs:
        print(i, logs[i])
    sys.exit(0)

if commands['addlog']:
    p = commands['<package>'][0]
    u = commands['<url>']
    logs[p] = u
    with open(logfile, 'w') as f:
        json.dump(logs, f, indent=4)
    print("Added log file for {}.".format(p))
    sys.exit(0)

# packages I'm already preparing an update for
PENDING = [i.split("/")[-1] for i in
           glob.glob(dlp3_branch_path+"/*")]

SKIP = SKIP+PENDING

skipped = [p for p in packages if p in SKIP]
packages = [p for p in packages if p not in SKIP]

if len(skipped) > 0:
    print("Skipping some packages:")
    for p in skipped:
        print("  ", p)

specfiles = [glob.glob("{}/*spec".format(os.path.join(dlp3_path, p)))[0] for p in packages]
patchfiles = [glob.glob("{}/*patch".format(os.path.join(dlp3_path, p))) for p in packages]

specialnames = {'usb': 'pyusb', 'xdg': 'pyxdg'}

print("checking packages:")

def gen_get_name_version(specfiles):
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
    for n, v, u, p in l:
        if n in specialnames:
            n = specialnames[n]
        yield n, v, u, p

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
data = [[v, r, n, u, s] for [n, v, u, p], s, r in zip(fix_name_version2, specfiles, results)]

# got all data information
# now check local for updates or create them
# can't use processpool, since objects can't be pickled
pool = concurrent.futures.ThreadPoolExecutor(max_workers=7)

def do_update(d, p, patch):
    old = d[0]
    new = d[1]
    print("branching ", p)
    # cd into dpl3 package, clone and checkout
    # os.chdir is not threadsafe, so don't use it
    olddir = os.path.join(dlp3_path, p)
    os.system("cd {} && osc branch".format(olddir))
    os.system("cd {} && osc co {}".format(dlp3_branch_path, p))

    newdir = os.path.join(dlp3_branch_path, p)
    # download new source
    print("downloading")
    url = d[3].replace("%{version}", new)
    try:
        r = requests.get(url, verify=False)
        # use absolut url to make it thread-safe
        with open(os.path.join(newdir, url.split("/")[-1]), 'wb') as f:
            f.write(r.content)
        print("download successful")
    except:
        print("couldn't download", p)
        return
    # add new package, remove old one
    newpackage = url.split("/")[-1]
    oldpackage = d[3].replace("%{version}", old).split("/")[-1]
    print(oldpackage, "=>", newpackage)
    os.system("cd {} && rm {}".format(newdir, oldpackage))
    os.system("cd {} && osc addremove".format(newdir))
    # update version in spec file
    changelog = ""
    spec = d[4].split("/")[-1]
    with open(os.path.join(newdir, spec), "r+") as input:
        content = input.readlines()
        input.seek(0)
        for line in content:
            if "Version" in line and old in line:
                line = line.replace(old, new)
                # add changelog entry
                changelog += "- update to version {}:".format(new)
                changelog += "\n\n"
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
        with open(os.path.join(newdir, file), "r+") as changes:
            content = changes.readlines()
            changes.seek(0)
            changes.write(changelog)
            for l in content:
                changes.write(l)

good = 0
dev = 0
need = 0
neednopatch = 0
bad = 0
ready = []
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
                                         'b', 'b1', 'b2', 'b3', 'b4', 'b5')):
        dev += 1
        continue
    if not commands['--error']:
        if old != new and new is not None and old == natsort.versorted([old, new])[0]:
            need += 1
            for i, c in enumerate(old):
                if i == len(new) or new[i] != c:
                    break
            if commands['check']:
                if not patch:
                    neednopatch += 1
                patchstr = colored("nopatch", 'green') if not patch else "patch"
                changelog = "+ " if p in logs else "  "
                print("{}{:40}  {:12} {:12}   {:10}".
                      format(changelog, p,
                             old[:i]+colored(old[i:], 'red'),
                             new[:i]+colored(new[i:], 'green'),
                             patchstr))
            if commands['update']:
                pool.submit(do_update, d, p, patch)
                ready.append(p)
    else:
        if old is None or new is None:
            print(" {} {} {}".format(p, d[0], d[1]))
            bad += 1

# wait for pool to finish
pool.shutdown(wait=True)

#print out some information
print("")
if commands['--error']:
    print("found {} packages that are not on pypi".format(bad))

print("found {} up to date packages,".format(good) +
      " {} with a dev release, ".format(dev) +
      "and {} packages that need an update,".format(need) +
      " {} without a patch".format(neednopatch))

if ready != []:
    print("-----------------------------")
    print("packages ready for osc vc")
    for p in ready:
        changelog = logs[p] if p in logs else ""
        print("  * ", p, changelog)
