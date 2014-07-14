#include "closure.h"

// Control whether pony_alloc is used or not
#define USE_PONY_ALLOC

#ifdef USE_PONY_ALLOC
  #include <pony/pony.h>
#else
  #include <stdlib.h> 
#endif

struct closure{
  closure_fun call;
  void *env;
};

closure_t *mk_closure(closure_fun fn, void *env){
#ifdef USE_PONY_ALLOC
  closure_t *c = pony_alloc(sizeof(closure_t));
#else
  closure_t *c = malloc(sizeof(closure_t));
#endif
  c->call = fn;
  c->env = env;
  return c;
}

value_t closure_call(closure_t *closure, value_t args[]){
  return closure->call(args, closure->env);
}

void closure_free(closure_t *closure){
#ifdef USE_PONY_ALLOC
#else
  free(closure->env); // Leaks copied memory!
  free(closure);
#endif
}

value_t ptr_to_val(void *p){
  return (value_t) {.ptr = p};
}

value_t int_to_val(uint64_t n){
  return (value_t) {.integer = n};
}

value_t dbl_to_val(double d){
  return (value_t) {.dbl = d}; 
}

void *val_to_ptr(value_t v){
  return v.ptr;
}

int val_to_int(value_t v){
  return v.integer;
}

double val_to_dbl(value_t v){
  return v.dbl;
}

void closure_trace(closure_t* c) {
  // Not implemented yet
}
