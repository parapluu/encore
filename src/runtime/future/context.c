// TODO: the stack should be packed at the end of the ctx_t to avoid the unnecessary dereferency

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
#include <pthread.h>
#include <errno.h>
#include "spinlock_compatibility.h"

#include <unistd.h>
#include <sys/syscall.h>
#include <errno.h>
#include <sys/time.h>

//#define _DBG

#ifdef _DBG
#define LOCK(ctx, str)                                                  \
  printf("attempting lock (\"%s\") at line <%i>...", str, __LINE__);    \
  pthread_spin_lock(&(ctx->ctx_lk)); printf("ok\n");                    \
  ctx->lock_info = str;
#define UNLOCK(ctx,str)                                                 \
  printf("attempting unlock (\"%s\") at line <%i>...", str, __LINE__);  \
  pthread_spin_unlock(&(ctx->ctx_lk)); printf("ok\n");                  \
  ctx->lock_info = str
#define DBG(str) printf("DBG: %s\n",str);
#else
#define LOCK(ctx,str)                                                   \
  pthread_spin_lock(&(ctx->ctx_lk))
#define UNLOCK(ctx,str)                                                 \
  pthread_spin_unlock(&(ctx->ctx_lk))
#define DBG(str) (1);
#endif // _DBG


typedef struct _annotated_ucontext_t {
  ucontext_t ucontext;
#ifdef _DBG
  char* info;
#endif // _DBG
} uctx_t;

typedef struct _ctx {
  volatile uctx_t mthd_ctx;    // the method's context
  volatile uctx_t pony_ctx;    // pony's context
#ifdef _DBG
  char * volatile lock_info;
#endif
  volatile pthread_spinlock_t ctx_lk; // this shall only be unlocked once it's
                                   // legal to jump back into the method
  volatile bool done;              // shall be true if the context is ready to be used
  volatile void * volatile  payload;
} ctx_t;

void ctx_free(ctx_t *ctx) {
  //XXX: payload may leak
  pthread_spin_destroy(&(ctx->ctx_lk));
  free(ctx);
}

#ifdef _DBG
void uctx_set(uctx_t *uctx) {
  printf("DBG: jumping to %s\n", uctx->info);
  uctx->info = NULL;
  setcontext(&(uctx->ucontext));
}
#else
void uctx_set(uctx_t *uctx) {
  setcontext(uctx);
}
#endif // _DBG

#ifdef _DBG
void uctx_annotate(uctx_t *uctx, char *info) {
  uctx->info = info;
}
#else
void uctx_annotate(uctx_t *uctx, char *info) {
}
#endif // _DBG

#ifdef _DBG
void uctx_swap(uctx_t *save, uctx_t *run, char *info) {
  save->info = info;
  printf("DBG: swapping to %s\n", run->info);
  swapcontext(&(save->ucontext), &(run->ucontext));
}
#else
void uctx_swap(uctx_t *save, uctx_t *run, char *info) {
  swapcontext(save, run);
}
#endif // _DBG

void ctx_print(ctx_t *ctx, char* title) {
#ifdef _DBG
  printf("vvvvvvv %s\n", title);
  printf("ctx @ %p:\n", ctx);
  if (ctx == NULL) {
    printf("NULL\n");
  } else {
    printf("mthd_ctx  = %p\n",ctx->mthd_ctx);
#ifdef _DBG
    printf("            (%s)\n",ctx->mthd_ctx.info);
#endif // _DBG
    printf("pony_ctx  = %p\n",ctx->pony_ctx);
#ifdef _DBG
    printf("            (%s)\n",ctx->pony_ctx.info);
#endif // _DBG
    printf("done      = %i\n",(int)ctx->done);
    printf("payload   = %p (%i)\n",ctx->payload, (int)ctx->payload);
#ifdef _DBG
    printf("lock_info = %s\n", ctx->lock_info);
#endif
  }
  printf("^^^^^^^\n");
#endif
}

const unsigned int STACKLET_SIZE = 512;

union splitptr {
  int ints[2];
  void *ptr;
};

void *assemble_ptr(int i0, int i1) {
  union splitptr spl;
  spl.ints[0] = i0;
  spl.ints[1] = i1;
  return spl.ptr;
}

// this is a workaround for makecontext's horrible interface; it takes
// four ints, assembles them into two pointer, the first of which is a
// function pointer, the second its argument; it then calls the
// function with the argument.
void reassemble_and_call(int func0, int func1, int args0, int args1) {
  void (*func)(void*) = (void (*)(ctx_t*))assemble_ptr(func0,func1);
  ctx_t *ctx = (ctx_t*)assemble_ptr(args0, args1);

  (*func)(ctx);

  DBG("method terminated");

  //resume whatever the current pony context is:
  uctx_set(&(ctx->pony_ctx));
}

void *ctx_call(void (*func)(ctx_t*), void *args) {
  DBG("ctx_call");
  ctx_t *ctx = calloc(1,sizeof(ctx_t));
  ctx->payload = args;
  pthread_spin_init(&(ctx->ctx_lk), true);

  union splitptr ctx_spl;
  ctx_spl.ptr = ctx;

  union splitptr func_spl;
  func_spl.ptr = func;

  getcontext(&(ctx->mthd_ctx));
  uctx_annotate(&(ctx->mthd_ctx), "ctx_call");

  char* mthd_stck = malloc(sizeof(char)*SIGSTKSZ);
#ifdef _DBG
  ucontext_t *muctx = &(ctx->mthd_ctx.ucontext);
#else
  ucontext_t *muctx = ctx;
#endif
  muctx->uc_link  = NULL;
  muctx->uc_stack.ss_sp = mthd_stck;
  muctx->uc_stack.ss_size  = SIGSTKSZ;
  makecontext(muctx,
              reassemble_and_call,
              4,
              func_spl.ints[0],
              func_spl.ints[1],
              ctx_spl.ints[0],
              ctx_spl.ints[1]);

  LOCK(ctx, "launching call");
  uctx_swap(&(ctx->pony_ctx),&(ctx->mthd_ctx), "dispatch");
  UNLOCK(ctx, "call is done");
  return ctx;
}

bool ctx_done(ctx_t *ctx) {
  DBG("ctx_done");
  return ctx->done;
}

volatile void * volatile ctx_get_payload(ctx_t *ctx) {
  return ctx->payload;
}

void *ctx_await(ctx_t *ctx) {
  DBG("ctx_await");
  getcontext(&(ctx->mthd_ctx.ucontext));
  uctx_annotate(&(ctx->mthd_ctx),"ctx_await");
  if (ctx_done(ctx)) {
    DBG("context done, returning result");
    void *res = ctx->payload;
    ctx->done    = false;
    ctx->payload = NULL;
    printf("payload=%p\n", res);
    return res;
  } else {
    DBG("context not done");
    uctx_set(&(ctx->pony_ctx)); //will also unlock (all swapcontexts are followed by unlocking)
  }
}

void ctx_reinstate(ctx_t *ctx, void* payload) {
  LOCK(ctx, "reinstating");
  ctx->payload  = payload;
  ctx->done     = true;
  ctx_print(ctx,"reinstate");
  uctx_swap(&(ctx->pony_ctx),&(ctx->mthd_ctx), "tell");
  UNLOCK(ctx, "reinstating done");
}
