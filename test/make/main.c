// This file is part of the mkcheck project.
// Licensing information can be found in the LICENSE file.
// (C) 2017 Nandor Licker. All rights reserved.

#include <stdlib.h>
#include "lib_a/lib_a.h"
#include "lib_b/lib_b.h"
#include "a.h"
#include "b.h"
#include "c.h"

int main()
{
  lib_a();
  lib_b();
  a();
  b();
  c();
  return EXIT_FAILURE;
}
