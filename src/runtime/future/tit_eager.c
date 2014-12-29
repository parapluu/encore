#define _XOPEN_SOURCE 600

#include <ucontext.h>
#include <assert.h>
#include <sysexits.h>
#include <err.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tit_eager.h"

#define STACK_SIZE (8*1024*1024)
#define MAX(a,b)   ((a < b) ? b : a)

typedef enum stack_state {
  SUSPENDED,
  RUNNING,
  GARBAGE,
} stack_state;

struct eager_tit_t {
  stack_state state;
  bool is_proper;
  ucontext_t context;
};

static __thread eager_tit_t *_trampoline;
static __thread eager_tit_t *runloop;

static void trampoline_5(eager_tit_t *new, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e);
static inline void __start_trampoline(eager_tit_t *new);
static inline void __end_trampoline();
extern void return_allocated_stack_to_pool(void *stack_pointer);
extern void mk_stack(ucontext_t *uc);
extern void *get_stack(pony_actor_t *a);
extern ucontext_t *get_context(pony_actor_t *a);

void fork_eager(void(*fun_t_5)(), pony_actor_t *a, void *b, void *c, void *d, void *e) {
  //FIXME: move to init part of runtime for each thread
  if (runloop == NULL) {
    eager_t_init_current();
  }

  ucontext_t *context = get_context(a);
  getcontext(context);
  context->uc_stack.ss_sp = get_stack(a);
  context->uc_link = &runloop->context;
  makecontext(context, trampoline_5, 7, NULL, fun_t_5, a, b, c, d, e);

  if (swapcontext(&runloop->context, context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}

static inline void __start_trampoline(eager_tit_t *t) {
  _trampoline = t;
}

static inline void __end_trampoline(eager_tit_t *t) {
  t->state = GARBAGE;
}

static void trampoline_5(eager_tit_t *t, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e) {
// static void trampoline_5(fun_t_5 fun, void *a, void *b, void *c, void *d, void *e) {
  // __start_trampoline(t);
  fun(a, b, c, d, e);
  // __end_trampoline(t);
}

void suspend_eager(eager_tit_t *t) {
  runloop->state = RUNNING;
  t->state = SUSPENDED;

  if (swapcontext(&t->context, &runloop->context) != 0) {
    err(EX_OSERR, "setcontext failed");
  }
    fflush(stdout);
}

void resume_eager(eager_tit_t *t) {
  eager_tit_t *cache = _trampoline;

  cache->state = SUSPENDED;
  t->state = RUNNING;
  t->context.uc_link = &cache->context;

  printf("before resuming sp is %p\n", t->context.uc_stack.ss_sp);
  printf("before resuming context is %p\n", t->context);
  if (swapcontext(&cache->context, &t->context) != 0) {
    err(EX_OSERR, "setcontext failed");
  }

  _trampoline = cache;

  switch (t->state) {
  case GARBAGE:
    return_allocated_stack_to_pool(t->context.uc_stack.ss_sp);
    free(t);
    break;
  case SUSPENDED:
    // Do nothing
    break;
  case RUNNING:
    assert(false);
    break;
  default:
    assert(false);
    break;
  }

  /* eager_tit_t *foo = runloop; */
  /* foo->state = RUNNING; */
}

eager_tit_t *get_suspendable_tit() {
  assert(_trampoline->is_proper == false);

  return _trampoline;
}

void eager_t_init_current() {
  runloop = calloc(1, sizeof(eager_tit_t));
  runloop->is_proper = true;
  runloop->state = RUNNING;
}
