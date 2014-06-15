#define _XOPEN_SOURCE

#include <ucontext.h>
#include <assert.h>
#include <sysexits.h>
#include <err.h>
#include <stdlib.h>
#include "tit_eager.h"
#include "mpmcq.h"

struct tit_t {
  ucontext_t extra_stack;
  ucontext_t original_stack;
  void *stack; // This is used because original_stack may be moved underfoot due (e.g. 16 byte alignment on OS X)
  bool is_suspended;
};

tit_t *t_init() {
  tit_t *stack_pair = calloc(1, sizeof(struct tit_t));
  return stack_pair;
}

#define __t_fork(stack_pair)				\
  getcontext(&stack_pair->extra_stack);			\
  stack_pair->stack			   = calloc(1, stack_pair->extra_stack.uc_stack.ss_size);	\
  stack_pair->extra_stack.uc_stack.ss_sp    = stack_pair->stack;  \
  stack_pair->extra_stack.uc_stack.ss_flags = 0;		\
  stack_pair->extra_stack.uc_link           = &stack_pair->original_stack;	\

void t_fork_0(tit_t *stack_pair, void(*fun)()) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 0);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}

void t_fork_1(tit_t *stack_pair, void(*fun)(void* a), void *a) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 1, a);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}
void t_fork_2(tit_t *stack_pair, void(*fun)(void* a, void *b), void *a, void *b) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 2, a, b);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}
void t_fork_3(tit_t *stack_pair, void(*fun)(void* a, void *b, void *c), void *a, void *b, void *c) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 3, a, b, c);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}
void t_fork_4(tit_t *stack_pair, void(*fun)(void* a, void *b, void *c, void *d), void *a, void *b, void *c, void *d) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 4, a, b, c, d);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}
void t_fork_5(tit_t *stack_pair, void(*fun)(void* a, void *b, void *c, void *d, void* e), void *a, void *b, void *c, void *d, void *e) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 5, a, b, c, d, e);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  // puts("Resuming from t_fork_5");
}
void t_fork_6(tit_t *stack_pair, void(*fun)(void* a, void *b, void *c, void *d, void* e, void*f), void *a, void *b, void *c, void *d, void *e, void *f) {
  __t_fork(stack_pair);
  makecontext(&stack_pair->extra_stack, fun, 6, a, b, c, d, e, f);
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}

void t_suspend(tit_t *stack_pair) {
  stack_pair->is_suspended = true;
  if (swapcontext(&stack_pair->extra_stack, &stack_pair->original_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}

static void t_cleanup(tit_t *stack_pair) {
  free(stack_pair->stack);
  free(stack_pair);
}

void t_resume(tit_t *stack_pair) {
  assert(stack_pair->is_suspended);
  stack_pair->is_suspended = false;
  stack_pair->extra_stack.uc_link = &stack_pair->original_stack;
  if (swapcontext(&stack_pair->original_stack, &stack_pair->extra_stack) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  t_cleanup(stack_pair);
}

void t_done(tit_t *stack_pair) {
  // Nothing to do in this strategy
}

bool t_is_suspended(tit_t *stack_pair) {
  return stack_pair->is_suspended;
}

