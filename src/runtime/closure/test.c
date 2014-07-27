#include "closure.h"
#include <stdio.h>
#include <stdlib.h>

struct _closure_env0{
  int z;
};

value_t _closure_fun0(value_t args[], void *env){
  int x = val_to_int(args[0]);
  int y = val_to_int(args[1]);
  int z = ((struct _closure_env0 *) env)->z;
  int _tmp0 = x + y + z;
  return int_to_val(_tmp0);
}

int main(void) {
// In encore:
// let z = 3 in
//   let add = \(x : int, y : int) -> x + y + z in
//     ...
//     print add(1, 2);
  int _tmp0 = 3;
  struct _closure_env0 *env = malloc(sizeof(struct _closure_env0));
  env->z = _tmp0;
  struct closure *_tmp1 = mk_closure(_closure_fun0, env);
  // ...
  value_t _tmp_args0[] = {int_to_val(1), int_to_val(2)};
  int _tmp3 = val_to_int(closure_call(_tmp1, _tmp_args0));
  printf("%d\n", _tmp3);
  return 0;
}