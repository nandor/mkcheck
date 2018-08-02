# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. ALl rights reserved.

import os
import subprocess
import sys


def run_proc(*args, **kwargs):
    sys.stdout.flush()

    proc = subprocess.Popen(
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        *args,
        **kwargs
    )
    stdout, stderr = proc.communicate()
    if proc.returncode != 0:
        print stdout
        print stderr
        sys.stdout.flush()
        raise Exception('Command "%s" failed: %d' % (' '.join(args[0]), proc.returncode))
