
#ifndef __context_h__
#define __context_h__

#include <stdbool.h>

const unsigned int STACKLET_SIZE;

typedef struct _ctx ctx_t;

// make an empty context that will call the passed in function and
// pass the void pointer along; to be called from the dispatch
// function
void *ctx_empty(void(*)(void), void*);

ctx_t *ctx_assemble(int i0, int i1);

// run a context that was created with ctx_empty
void ctx_run(ctx_t*);

// free a context
void ctx_free(ctx_t*);

// capture `now` into the context
void ctx_capture(ctx_t*);

void print_ctx(ctx_t*, char*);

// set the context to pending (the answer is on the way)
void ctx_set_pending(ctx_t*);

bool ctx_is_pending(ctx_t*);

// true if the passed in context is ready
bool ctx_ready(ctx_t*);

// get the result from a context -- or switch back to the dispatch
// function if it's not ready yet
void *ctx_await_result(ctx_t*);

void *ctx_get_args(ctx_t*);

// reinstate a context with the result (ctx_await_result will return)
void ctx_reinstate(ctx_t*, void*);

#endif
