# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

import os



def read_mtimes(paths):
    mtimes = {}
    for path in paths:
        if os.path.exists(path):
            mtimes[path] = os.path.getmtime(path)
        else:
            mtimes[path] = 0
    return mtimes
