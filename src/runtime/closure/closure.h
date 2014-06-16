#ifndef __closure_h__
#define __closure_h__

#include <stdint.h>

// This should probably be replaced by the pony_arg_t union
// sometime, this could remove the need for of the set/get arg
// functions
typedef union value {
  void* ptr;
  uint64_t integer;
  double dbl;
} value;

struct closure;

/**
 *  The body of a closure.

 *  The function should start by reading its arguments, using
 *  closure_get_arg_ptr() et al. It should return a value using
 *  ptr_to_val() et al.
 */
typedef value (*closure_fun)(value[], void*);

struct closure{
  closure_fun call;
  void *env;
};

/**
 *  Create a new closure.
 * 
 *  Use closure_free"()" to free the allocated memory.
 * 
 *  @param body The body of the closure (see the typedef of closure_fun)
 *  @param env The environment of the closure
 *  @return A closure with body \p body and environment \p env
 */
struct closure *closure_mk(closure_fun body, void *env);

/**
 *  Call a closure. 
 *  @param closure The closure to be called
 *  @param args An array of values used as the arguments to the closure
 *  @return The pointer returned * by \p closure
 */
value closure_call(struct closure *closure, value args[]);

/**
 *  Call a closure, expecting an int as return value.
 *  @param closure The closure to be called
 *  @return The integer returned by \p closure
 */
int closure_call_int(struct closure *closure, value args[]);

/**
 *  Call a closure, expecting a double as return value.
 *  @param closure The closure to be called
 *  @return The double returned by \p closure
 */
double closure_call_dbl(struct closure *closure, value args[]);

/**
 *  Free a closure.
 *  @param closure a closure
 */
void closure_free(struct closure *closure);

/**
 *  Convert a pointer to value form.
 *  @param p a pointer
 *  @return \p p in value form
 */
value ptr_to_val(void *p);

/**
 *  Convert an integer to value form.
 *  @param n an integer
 *  @return \p n in value form
 */
value int_to_val(uint64_t n);

/**
 *  Convert a double to value form.
 *  @param d a double
 *  @return \p d in value form
 */
value dbl_to_val(double d);

/**
 *  Convert a value to a pointer.
 *  @param v a value
 *  @return \p v as a pointer
 */
void *val_to_ptr(value v);

/**
 *  Convert a value to an integer.
 *  @param v a value
 *  @return \p v as an integer
 */
int val_to_int(value v);

/**
 *  Convert a value to a double.
 *  @param v a value
 *  @return \p v as a double
 */
double val_to_dbl(value v);

void closure_trace(struct closure* c);

#endif
