#include "closure.h"
#include <stdio.h>

value _closure_fun0(value args[], void *env){
  int x = val_to_int(args[0]);
  int y = val_to_int(args[1]);
  int _tmp0 = x + y;
  return int_to_val(_tmp0);
}

int main(void) {
// In encore:
// let add = \(x : int, y : int) -> x + y in
//   ...
//   print add(2, 3);
  struct closure *c = closure_mk(_closure_fun0);
  // ...
  value _tmp_args0[] = {int_to_val(2), int_to_val(3)};
  int _tmp1 = val_to_int(closure_call(c, _tmp_args0));
  printf("%d\n", _tmp1);
  return 0;
}