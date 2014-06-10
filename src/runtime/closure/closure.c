#include "closure.h"
#include <stdlib.h> 

struct closure *closure_mk(closure_fun fn){
  struct closure *c = malloc(sizeof(struct closure));
  c->call = fn;
  c->env = NULL;
  return c;
}

value closure_call(struct closure *closure, value args[]){
  return closure->call(args, closure->env);
}

void closure_free(struct closure *closure){
  free(closure);
}

value ptr_to_val(void *p){
  return (value){.ptr = p};
}

value int_to_val(uint64_t n){
  return (value){.integer = n};
}

value dbl_to_val(double d){
  return (value){.dbl = d};
}

void *val_to_ptr(value v){
  return v.ptr;
}

int val_to_int(value v){
  return v.integer;
}

double val_to_dbl(value v){
  return v.dbl;
}