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
  ucontext_t mthd_ctx; // the method's context
  ucontext_t pony_ctx; // the dispatch function's context
  void *args;
  volatile bool done; // shall be true if the context is ready to be used
  volatile void* result;
  volatile bool pending; // shall be set to true after the request was sent
} ctx_t;

void ctx_free(ctx_t *ctx) {
  //XXX: result may leak
  free(ctx->args);
  free(ctx);
}

void print_ctx(ctx_t *ctx, char* title) {
  printf("vvvvvvv %s\n", title);
  printf("ctx @ %p:\n", ctx);
  if (ctx == NULL) {
    printf("NULL\n");
  } else {
    printf("mthd_ctx       = %p\n",ctx->mthd_ctx);
    printf("pony_ctx       = %p\n",ctx->pony_ctx);
    printf("args           = %p\n",ctx->args);
    printf("done           = %i\n",(int)ctx->done);
    printf("result         = %p (%i)\n",ctx->result, (int)ctx->result);
    printf("pending        = %i\n", ctx->pending);
  }
  printf("^^^^^^^\n");
}

const unsigned int STACKLET_SIZE = 512;

union splitptr {
  struct {
    int i0;
    int i1;
  } ints;
  ctx_t *ptr;
};

void ctx_return(ctx_t* ctx) {
  print_ctx(ctx, "ctx_return");
  setcontext(&(ctx->pony_ctx));
}

void *ctx_empty(void (*func)(void), void *args) {
  //  printf("creating empty ctx:\n");
  ctx_t *ctx = malloc(sizeof(ctx_t));
  ctx->args    = args;
  ctx->done    = false;
  ctx->result  = NULL;
  ctx->pending = false;

  union splitptr ctx_spl;
  ctx_spl.ptr = ctx;

  getcontext(&(ctx->mthd_ctx));
  char* mthd_stck = malloc(sizeof(char)*SIGSTKSZ);
  ctx->mthd_ctx.uc_link  = NULL;//dispatch_ctx.ptr;
  ctx->mthd_ctx.uc_stack.ss_sp = mthd_stck;
  ctx->mthd_ctx.uc_stack.ss_size  = SIGSTKSZ;
  //  printf("f @ %p\n",func);
  //  ((void (*)(int,int))func)(0,0);
  //  printf("ran f\n");
  makecontext(&(ctx->mthd_ctx),
              *func,
              2,
              ctx_spl.ints.i0,
              ctx_spl.ints.i1);

  print_ctx(ctx, "created");

  getcontext(&(ctx->pony_ctx));
  return ctx;
}

ctx_t *ctx_assemble(int i0, int i1) {
  union splitptr ctx_spl;
  ctx_spl.ints.i0 = i0;
  ctx_spl.ints.i1 = i1;
  return ctx_spl.ptr;
}

void ctx_run(ctx_t *ctx) {
  printf("attempting to run\n");
  setcontext(&(ctx->mthd_ctx));
  printf("why do I see this?\n");
}

bool ctx_ready(ctx_t *ctx) {
  return ctx->done;
}

void ctx_set_pending(ctx_t *ctx) {
  ctx->pending = true;
  ctx_return(ctx); // jump back to dispatch fn
}

bool ctx_is_pending(ctx_t *ctx) {
  return ctx->pending;
}

void ctx_capture(ctx_t *ctx) {
  getcontext(&(ctx->mthd_ctx));
  print_ctx(ctx, "captured");
}

void *ctx_get_args(ctx_t *ctx) {
  return ctx->args;
}

void *ctx_await_result(ctx_t *ctx) {
  getcontext(&(ctx->mthd_ctx));
  if (ctx_ready(ctx)) {
    return ctx->result;
  } else {
    setcontext(&(ctx->pony_ctx));
  }
}

void ctx_reinstate(ctx_t *ctx, void* result) {
  ctx->result  = result;
  ctx->pending = false;
  ctx->done    = true;
  print_ctx(ctx, "ctx_reinstate");
  setcontext(&(ctx->mthd_ctx));
}
