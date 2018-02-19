# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

import json
import os
import sys
import subprocess


def read_times():
    times = {}
    tmpdir = sys.argv[1]
    with open(os.path.join(tmpdir, 'files'), 'r') as f:
        for f in json.loads(f.read()):
            if 'deleted' in f and f['deleted']:
                continue
            if not 'exists' in f or not f['exists']:
                continue
            name = f['name']
            try:
                times[name] = os.path.getmtime(name)
            except:
                print 'No such file: ', name

    return times

t0 = read_times()
subprocess.call(sys.argv[2:])
t1 = read_times()

for k, v in t0.iteritems():
    if v != t1[k]:
        print k
