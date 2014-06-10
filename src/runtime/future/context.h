/**
   @file context.h

   Pausing and resuming contexts in Encore.

   @author Stephan.Brandauer@it.uu.se

   This module gives you functions that you can skip and resume. It
   achieves this by introducing an opaque data type `ctx_t`. A
   pausable function is then a function that takes a `ctx_t` and a
   `void*` for arguments and returns `void`.

   By using `ctx_call`
 */


#ifndef __context_h__
#define __context_h__

#include <stdbool.h>

/**
   A context.
*/
typedef struct _ctx ctx_t;

/**
   A pausable function.
   
   A pausable function takes a `ctx_t` and a `void*` and returns void.
   Only functions with this interface are pausable.
*/
typedef void (*pausable_fun)(ctx_t*, void*);

/**
   Run a pausable function.
   
   Set up a `ctx_t` and use it to call the method implementation, with
   the `ctx_t` as the first argument and the `void*` as the
   second. The void pointer will be passed on to the function.
   
   @param func The function to run.
   @param d The data parameter to the function.
*/
void ctx_call(pausable_fun func, void* d);

/**
   Leave execution of the function and wait for ctx_reinstate to be
   called on the context.
   
   Called from inside a `pausable_fun`.

   Execution will resume at whichever happened last: `ctx_call` or
   `ctx_reinstate`.

   If the context is later used as an argument to `ctx_reinstate`, the
   call will be resumed as if `ctx_await` had just returned.
   
   @param ctx The context of the function to be paused.
*/
void ctx_await(ctx_t* ctx);

/**
   Reinstate/resume an awaiting context.
   
   Called from outside a `pausable_fun`.

   The calling thread will resume execution of the `pausable_fun` as
   if `ctx_await` had just returned.

   If a `ctx_await` or a `ctx_return` is encountered in the
   `pausable_fun`, it will cause this call to return.
   
   @param a the contex to be resumed
*/
void ctx_reinstate(ctx_t* ctx);

/**
   Return from a `pausable_fun` forever.
   
   Called from inside a `pausable_fun`.

   This will cause the context to be freed. Execution will resume at
   whichever happened last: `ctx_call` or `ctx_reinstate`.

   @param ctx The context.
*/
void ctx_return(ctx_t* ctx);

/**
   Prints some info on the ctx, annotated with a subject-string.
   
   @param ctx The context.
   @param str The subject string.
*/
void ctx_print(ctx_t* ctx, char* str);

#endif
