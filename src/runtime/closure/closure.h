#ifndef __closure_h__
#define __closure_h__

#include <stdint.h>
#include <pony/pony.h>

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

void closure_trace(void *p);

/**
 *  The body of a closure.

 *  The function should start by reading its arguments, using
 *  closure_get_arg_ptr() et al. It should return a value using
 *  ptr_to_val() et al.
 */
typedef value_t (*closure_fun)(value_t[], void*);

/**
 *  Create a new closure.
 *
 *  Use closure_free"()" to free the allocated memory.
 *
 *  @param body The body of the closure (see the typedef of closure_fun)
 *  @param env The environment of the closure
 *  @param trace The trace function of the environment
 *  @return A closure with body \p body and environment \p env
 */
closure_t *closure_mk(closure_fun body, void *env, pony_trace_fn trace);

/**
 *  Call a closure.
 *  @param closure The closure to be called
 *  @param args An array of values used as the arguments to the closure
 *  @return The pointer returned * by \p closure
 */
value_t closure_call(closure_t *closure, value_t args[]);

/**
 *  Free a closure.
 *  @param closure a closure
 */
void closure_free(closure_t *closure);

#endif
