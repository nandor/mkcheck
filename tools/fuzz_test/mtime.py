# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2017 Nandor Licker. All rights reserved.

import os



def read_mtimes(files):
    mtimes = {}
    for file in files:
        mtimes[file] = os.path.getmtime(file)
    return mtimes
