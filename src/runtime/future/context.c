// stack seems to be destroyed upon calling setcontext.
// This is due to:
// 1. the method returning on the first of capture_context
// 2. the actor resuming work, using the stack
// 3. the answer coming in and re
// conjecture:
// it *should* work to:
//  - in dispatch
//   1. create a stacklet to run the call on by using `makecontext`
//   2. save the context using `getcontext`
//  - on get, save the context as per usual and switch back to the old stack

#include "context.h"
#define _XOPEN_SOURCE 800
#include <ucontext.h>
#undef _XOPEN_SOURCE

#include <stdlib.h>
#include <stdio.h>

#define NULL 0

typedef struct _ctx {
  ucontext_t mthd_ctx;   // the method's context
  ucontext_t pony_ctx;   // the dispatch function's context
  volatile bool awaiting;
  volatile bool done;    // shall be true if the context is ready to be used
  volatile void* payload;
} ctx_t;

void ctx_free(ctx_t *ctx) {
  //XXX: payload may leak
  free(ctx);
}

void ctx_print(ctx_t *ctx, char* title) {
  printf("vvvvvvv %s\n", title);
  printf("ctx @ %p:\n", ctx);
  if (ctx == NULL) {
    printf("NULL\n");
  } else {
    printf("mthd_ctx       = %p\n",ctx->mthd_ctx);
    printf("pony_ctx       = %p\n",ctx->pony_ctx);
    printf("done           = %i\n",(int)ctx->done);
    printf("payload        = %p (%i)\n",ctx->payload, (int)ctx->payload);
  }
  printf("^^^^^^^\n");
}

const unsigned int STACKLET_SIZE = 512;

void ctx_run(ctx_t *ctx) {
  swapcontext(&(ctx->pony_ctx),&(ctx->mthd_ctx));
}

union splitptr {
  struct {
    int i0;
    int i1;
  } ints;
  void *ptr;
};

// this is a workaround for makecontext's horrible interface; it takes
// four ints, assembles them into two pointer, the first of which is a
// function pointer, the second its argument; it then calls the
// function with the argument.
void reassemble_and_call(int func0, int func1, int args0, int args1) {
  union splitptr func_s;
  func_s.ints.i0 = func0;
  func_s.ints.i1 = func1;
  void (*func)(void*) = (void (*)(void*))(func_s.ptr);

  union splitptr args_s;
  args_s.ints.i0 = args0;
  args_s.ints.i1 = args1;
  
  (*func)(args_s.ptr);
}

void *ctx_call(void (*func)(void*), void *args) {
  ctx_t *ctx = calloc(1,sizeof(ctx_t));
  ctx->payload = args;

  union splitptr ctx_spl;
  ctx_spl.ptr = ctx;

  union splitptr func_spl;
  func_spl.ptr = func;

  getcontext(&(ctx->mthd_ctx));
  char* mthd_stck = malloc(sizeof(char)*SIGSTKSZ);
  ctx->mthd_ctx.uc_link  = NULL;
  ctx->mthd_ctx.uc_stack.ss_sp = mthd_stck;
  ctx->mthd_ctx.uc_stack.ss_size  = SIGSTKSZ;
  makecontext(&(ctx->mthd_ctx),
              reassemble_and_call,
              4,
              func_spl.ints.i0,
              func_spl.ints.i1,
              ctx_spl.ints.i0,
              ctx_spl.ints.i1);

  //ctx_print(ctx, "created");
  //getcontext(&(ctx->pony_ctx));
  ctx_run(ctx);
  return ctx;
}

ctx_t *ctx_assemble(int i0, int i1) {
  union splitptr ctx_spl;
  ctx_spl.ints.i0 = i0;
  ctx_spl.ints.i1 = i1;
  return ctx_spl.ptr;
}

bool ctx_done(ctx_t *ctx) {
  return ctx->done;
}

void ctx_capture_mthd(ctx_t *ctx) {
  //  getcontext(&(ctx->mthd_ctx));
  //  ctx_print(ctx, "ctx_capture_mthd");
}

void *ctx_get_payload(ctx_t *ctx) {
  return ctx->payload;
}

void skip_out(ctx_t *ctx) {
  setcontext(&(ctx->pony_ctx));
}

void *ctx_await(ctx_t *ctx) {
  getcontext(&(ctx->mthd_ctx));
  if (ctx_done(ctx)) {
    void *res = ctx->payload;
    ctx->done     = false;
    ctx->awaiting = false;
    return res;
  } else {
    ctx->awaiting = true;
    skip_out(ctx);
  }
}

void ctx_reinstate(ctx_t *ctx, void* payload) {
  ctx->payload  = payload;
  ctx->done     = true;
  //ctx_print(ctx, "ctx_reinstate");
  while (!ctx->awaiting) {} //  not sure this is 
  ctx_run(ctx);
}
