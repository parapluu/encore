// TODO: the stack should be packed at the end of the ctx_t to avoid the unnecessary dereferency

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
  uctx_t mthd_ctx;    // the method's context
  uctx_t pony_ctx;    // pony's context
#ifdef _DBG
  char * volatile lock_info;
#endif
  pthread_spinlock_t ctx_lk; // this shall only be unlocked once it's
                             // legal to jump back into the method
  volatile bool reinstated;
  volatile bool done;
} ctx_t;

void ctx_free(ctx_t *ctx) {
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
    printf("mthd_ctx  = %p\n",&(ctx->mthd_ctx));
#ifdef _DBG
    printf("            (%s)\n",ctx->mthd_ctx.info);
#endif // _DBG
    printf("pony_ctx  = %p\n",&(ctx->pony_ctx));
#ifdef _DBG
    printf("            (%s)\n",ctx->pony_ctx.info);
#endif // _DBG
    printf("reinstated      = %i\n",(int)ctx->reinstated);
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
void reassemble_and_call(int func0, int func1, int ctx0, int ctx1, int args0, int args1) {
  DBG("reassemble_and_call");
  void (*func)(ctx_t*,void*) = (void (*)(ctx_t*, void*))assemble_ptr(func0,func1);
  ctx_t *ctx = assemble_ptr(ctx0, ctx1);
  void *args = assemble_ptr(args0, args1);

#ifdef _DBG
  printf("args=%p (reassembled)\n", args);  
#endif // _DBG

  (*func)(ctx, args);

  DBG("method terminated");

  //resume whatever the current pony context is:
  uctx_set(&(ctx->pony_ctx));
}

void ctx_call(pausable_fun func, void *args) {
  DBG("ctx_call");

  ctx_t *ctx = calloc(1,sizeof(ctx_t));
  pthread_spin_init(&(ctx->ctx_lk), true);

  union splitptr ctx_spl  = {.ptr = ctx };
  union splitptr func_spl = {.ptr = func };
  union splitptr args_spl = {.ptr = args };

  getcontext(&(ctx->mthd_ctx.ucontext));
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
              6,
              func_spl.ints[0],
              func_spl.ints[1],
              ctx_spl.ints[0],
              ctx_spl.ints[1],
              args_spl.ints[0],
              args_spl.ints[1]);

  LOCK(ctx, "launching call");
  uctx_swap(&(ctx->pony_ctx),&(ctx->mthd_ctx), "dispatch");
  UNLOCK(ctx, "call is done");
  if (ctx->done) {
    ctx_free(ctx);
  }
  return;
}

bool ctx_reinstated(ctx_t *ctx) {
  return ctx->reinstated;
}

void ctx_await(ctx_t *ctx) {
  DBG("ctx_await");
  getcontext(&(ctx->mthd_ctx.ucontext));
  uctx_annotate(&(ctx->mthd_ctx),"ctx_await");
  if (ctx_reinstated(ctx)) {
    DBG("context reinstated, returning");
    ctx->reinstated    = false;
    return;
  } else {
    DBG("context not reinstated");
    uctx_set(&(ctx->pony_ctx)); //will also unlock (all swapcontexts are followed by unlocking)
  }
}

void ctx_return(ctx_t* ctx) {
  DBG("ctx_return");
  ctx->done = true;
  ctx_await(ctx);
}

void ctx_reinstate(ctx_t *ctx) {
  LOCK(ctx, "reinstating");
  ctx->reinstated = true;
  ctx_print(ctx,"reinstate");
  uctx_swap(&(ctx->pony_ctx),&(ctx->mthd_ctx), "tell");
  UNLOCK(ctx, "reinstating done");
  if (ctx->done) {
    ctx_free(ctx);
  }
}
