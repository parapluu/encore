#ifndef __closure_h__
#define __closure_h__

#include <stdint.h>
#include <pony/pony.h> // <-- Only for accessing pony_arg_t

typedef pony_arg_t value_t;
/*
typedef union value {
  void* p;
  uint64_t i;
  double d;
} value_t;
*/

typedef struct closure closure_t;

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
 *  @return A closure with body \p body and environment \p env
 */
closure_t *mk_closure(closure_fun body, void *env);

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

void closure_trace(closure_t *c);

#endif
