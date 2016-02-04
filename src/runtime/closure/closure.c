#include "closure.h"

pony_type_t closure_type =
  {ID_CLOSURE,
   sizeof(struct closure),
   0,
   0,
   closure_trace,
   NULL,
   NULL,
   NULL,
   NULL
  };

void closure_trace(void *p){
  closure_t *c = (closure_t *) p;
  if(c->trace != NULL){
    c->trace(c->env);
  }
}

closure_t *closure_mk(closure_fun fn, void *env, pony_trace_fn trace){
  closure_t *c = pony_alloc(sizeof(closure_t));
  c->call = fn;
  c->env = env;
  c->trace = trace;
  return c;
}

value_t closure_call(closure_t *closure, value_t args[]){
  return closure->call(args, closure->env);
}

value_t ptr_to_val(void *p){
  return (value_t) {.p = p};
}

value_t int_to_val(uint64_t n){
  return (value_t) {.i = n};
}

value_t dbl_to_val(double d){
  return (value_t) {.d = d};
}

void *val_to_ptr(value_t v){
  return v.p;
}

int val_to_int(value_t v){
  return v.i;
}

double val_to_dbl(value_t v){
  return v.d;
}
