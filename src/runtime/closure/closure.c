#include "closure.h"

// Control whether pony_alloc is used or not
#define USE_PONY_ALLOC

#ifdef USE_PONY_ALLOC
  #include <pony/pony.h>
  #define ALLOC(size) pony_alloc(size)
  #define FREE(ptr)
#else
  #include <stdlib.h> 
  #define ALLOC(size) malloc(size)
  #define FREE(ptr) free(ptr)
#endif

struct closure{
  closure_fun call;
  void *env;
};

closure_t *mk_closure(closure_fun fn, void *env){
  closure_t *c = ALLOC(sizeof(closure_t));
  c->call = fn;
  c->env = env;
  return c;
}

value_t closure_call(closure_t *closure, value_t args[]){
  return closure->call(args, closure->env);
}

void closure_free(closure_t *closure){
  FREE(closure->env); // Leaks copied memory!
  FREE(closure);
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
