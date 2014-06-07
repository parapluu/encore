/**
   context.h -- pausing and resuming method contexts in Encore.

   Author: Stephan.Brandauer@it.uu.se

   
 */


#ifndef __context_h__
#define __context_h__

#include <stdbool.h>

typedef struct _ctx ctx_t;

// set up a context, then use it to call the method implementation,
// with the void pointer as the argument; this function should be
// called from the dispatch function
void *ctx_call(void(*)(void*), void*);

// free a context
void ctx_free(ctx_t*);

//// capture `now` into the context. After capturing a context, send 
//void ctx_capture_mthd(ctx_t*);

// prints some info on the ctx, annotated with a subject-string
void ctx_print(ctx_t*, char*);

// get the result from a context -- or switch back to the dispatch
// function if it's not ready yet. This function also resets the
// context so that it can be used for new interactions.
void *ctx_await(ctx_t*);

// return the payload
void *ctx_get_payload(ctx_t*);

// reinstate an awaiting context, setting a payload (the matching
// ctx_await_result will return the payload void*)
void ctx_reinstate(ctx_t*, void*);

#endif
