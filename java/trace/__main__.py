# This file is part of the mkcheck project.
# Licensing information can be found in the LICENSE file.
# (C) 2018 Nandor Licker. All rights reserved.

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
        print(stdout)
        print(stderr)
        sys.stdout.flush()
        raise Exception('Command failed: ' + str(proc.returncode))

def list_files(path):
  src = []
  dst = []

  for root, dirs, files in os.walk(path):
    for file in files:
      path = os.path.join(root, file)
      if file.endswith('.thrift'):
        src.append(path)
      if 'generated-sources' in path:
        dst.append(path)

  return src, dst


def get_timestamps(dst):
  times = {}
  for path in dst:
    try:
      times[path] = os.path.getmtime(path)
    except:
      times[path] = 0
  return times

if __name__ == '__main__':
  run_proc(['git', 'reset', '--hard', 'HEAD'])
  run_proc(['gradle', 'build'])
  src, dst = list_files(os.getcwd())
  run_proc(['git', 'reset', '--hard', 'HEAD'])
  run_proc(['gradle', 'clean'])

  with open('/tmp/java_test', 'w') as out:
    for path in src:
      print(path)

      t0 = get_timestamps(dst)
      run_proc(['git', 'reset', '--hard', 'HEAD'])
      run_proc(['gradle', 'clean'])

      with open(path, 'w') as f:
        f.write('break;$#312312312%%$#$@$')

      subprocess.Popen(['gradle', 'build']).communicate()
      t1 = get_timestamps(dst)

      out.write(path)
      out.write(':')
      for file in dst:
        if t0[file] != t1[file]:
          out.write(file)
          out.write(',')
      out.write('\n')
      out.flush()

      run_proc(['git', 'reset', '--hard', 'HEAD'])
