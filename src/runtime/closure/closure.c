#include "closure.h"

// Control whether pony_alloc is used or not

#define USE_PONY_ALLOC

extern __thread pony_actor_t* this_actor;

#ifdef USE_PONY_ALLOC
  #include <pony/pony.h>
  #include "../src/actor/actor.h"
  // Tobias: this is a hack to make top-level functions work as they are implemented as closures right now
  // This hack creates a small memory leak which we can safely ignore until top-level functions change.
  #define ALLOC(size) (actor_current() ? pony_alloc(size) : malloc(size))
  #define FREE(ptr)
#else
  #include <stdlib.h>
  #define ALLOC(size) malloc(size)
  #define FREE(ptr) free(ptr)
#endif

struct closure{
  closure_fun call;
  void *env;
  pony_trace_fn trace;
};

pony_type_t closure_type = {CLOSURE_ZERO, sizeof(struct closure), closure_trace, NULL, NULL};

void closure_trace(void *p){
  closure_t *c = (closure_t *) p;
  if(c->trace != NULL){
    c->trace(c->env);
  }
}

closure_t *closure_mk(closure_fun fn, void *env, pony_trace_fn trace){
  closure_t *c = ALLOC(sizeof(closure_t));
  c->call = fn;
  c->env = env;
  c->trace = trace;
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
