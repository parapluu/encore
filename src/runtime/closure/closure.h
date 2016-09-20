#ifndef __closure_h__
#define __closure_h__

#include <stdint.h>
#include <pony.h>

typedef struct closure closure_t;

#include "encore.h"

typedef encore_arg_t value_t;

/*
typedef union value {
  void* p;
  uint64_t i;
  double d;
} value_t;
*/

extern pony_type_t closure_type;

void closure_trace(pony_ctx_t *ctx, void *p);

/**
 *  The body of a closure.
 */
typedef value_t (*closure_fun)(pony_ctx_t**, pony_type_t**, value_t[], void*);

/**
 *  Create a new closure.
 *
 *  Use closure_free"()" to free the allocated memory.
 *
 *  @param fn The body of the closure (see the typedef of closure_fun)
 *  @param env The environment of the closure
 *  @param trace The trace function of the environment
 *  @return A closure with body \p body and environment \p env
 */
closure_t *closure_mk(pony_ctx_t **ctx, closure_fun fn, void *env,
                      pony_trace_fn trace, pony_type_t **runtimeTypes);

/**
 *  Call a closure.
 *  @param closure The closure to be called
 *  @param args An array of values used as the arguments to the closure
 *  @return The pointer returned * by \p closure
 */
value_t closure_call(pony_ctx_t **ctx, closure_t *closure, value_t args[]);

struct closure {
  closure_fun call;
  void *env;
  pony_trace_fn trace;
  pony_type_t **runtimeTypes;
};

#endif
