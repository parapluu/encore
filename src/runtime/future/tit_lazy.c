#define _XOPEN_SOURCE

#include <ucontext.h>
#include <assert.h>
#include <sysexits.h>
#include <err.h>
#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "tit_lazy.h"
#include "mpmcq.h"

#define STACK_SIZE (8*1024*1024)
#define MAX(a,b)   ((a < b) ? b : a)

typedef enum stack_state {
  SUSPENDED,
  RUNNING,
  GARBAGE,
  DONE
} stack_state;

struct lazy_tit_t {
  stack_state state;
  bool is_proper;
  ucontext_t context;
};

static pthread_once_t stack_pool_is_initialized = PTHREAD_ONCE_INIT;
static mpmcq_t proper_stacks_for_reuse;
static mpmcq_t side_stacks_for_reuse;
static __thread lazy_tit_t *current;
static unsigned int cached_side_stacks;
static unsigned int side_stack_cache_size;

static void trampoline_0(lazy_tit_t *new, fun_t_0 fun);
// static void trampoline_1(lazy_tit_t *new, fun_t_1 fun, void *a);
// static void trampoline_2(lazy_tit_t *new, fun_t_2 fun, void *a, void *b);
// static void trampoline_3(lazy_tit_t *new, fun_t_3 fun, void *a, void *b, void *c);
// static void trampoline_4(lazy_tit_t *new, fun_t_4 fun, void *a, void *b, void *c, void *d);
// static void trampoline_5(lazy_tit_t *new, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e);
// static void trampoline_6(lazy_tit_t *new, fun_t_6 fun, void *a, void *b, void *c, void *d, void *e, void *f);
void return_allocated_stack_to_pool(void *stack_pointer);
static inline void __start_trampoline(lazy_tit_t *new);
static inline void __end_trampoline();

void mk_stack(ucontext_t *fork) {
  if (side_stack_cache_size > 0) {
    while (fork->uc_stack.ss_sp == NULL && cached_side_stacks > 0) {
      fork->uc_stack.ss_sp = mpmcq_pop(&side_stacks_for_reuse);
    }
    if (fork->uc_stack.ss_sp) {
      int cache_size;
      do {
        cache_size = cached_side_stacks;
      } while (!__sync_bool_compare_and_swap(&cached_side_stacks, cache_size, cache_size - 1));
    }
  }

  if (fork->uc_stack.ss_sp == NULL) {
    if (posix_memalign(&fork->uc_stack.ss_sp, 16, MAX(fork->uc_stack.ss_size,STACK_SIZE)) != false) {
      err(EX_OSERR, "Posix memalign failed");
    }
  }
}

void return_allocated_stack_to_pool(void *stack_pointer) {
  bool free_stack = true;
  int current_cache_size;
  do {
    current_cache_size = cached_side_stacks;
    if (__sync_bool_compare_and_swap(&cached_side_stacks, current_cache_size, current_cache_size + 1)) {
      mpmcq_push(&side_stacks_for_reuse, stack_pointer);
      free_stack = false;
      break;
    }
  } while (current_cache_size < side_stack_cache_size);

  if (free_stack) free(stack_pointer);
}

static void garbage_collect_stack(lazy_tit_t *to_retire) {
  if (to_retire->is_proper) {
    mpmcq_push(&proper_stacks_for_reuse, to_retire);
  } else {
    return_allocated_stack_to_pool(to_retire->context.uc_stack.ss_sp);
    free(to_retire);
  }
}

static void __init_pool() {
  mpmcq_init(&proper_stacks_for_reuse);
  mpmcq_init(&side_stacks_for_reuse);
}

void init_system(int __side_stack_cache_size) {
  pthread_once(&stack_pool_is_initialized, __init_pool);
  // Silently fail if attempt to reset
  if (!side_stack_cache_size) {
    side_stack_cache_size = __side_stack_cache_size;
  }
}

lazy_tit_t *lazy_t_init_current() {
  assert(current == NULL);
  current = lazy_t_init_new();
  // TODO: assert this has not been initialized
  current->state = RUNNING;
  current->is_proper = true;
  return current;
}

lazy_tit_t *lazy_t_init_new() {
  lazy_tit_t *stack = calloc(1, sizeof(struct lazy_tit_t));
  return stack;
}

void fork_lazy(void(*fun)()) {
  /* fprintf(stderr, "forking!!\n"); */
  lazy_tit_t *fork = calloc(1, sizeof(struct lazy_tit_t));
  lazy_tit_t *cache = current;
  getcontext(&fork->context);
  fork->context.uc_stack.ss_sp    = NULL;
  fork->context.uc_stack.ss_flags = 0;
  fork->context.uc_link           = 0;
  fork->state                     = RUNNING;
  fork->is_proper                 = false;
  cache->state                    = SUSPENDED;

  mk_stack(&fork->context);

  // fprintf(stderr, "\t\t<%p> fork: tit %p has stack: %p\n", pthread_self(), fork, fork->context.uc_stack.ss_sp);
  makecontext(&fork->context, trampoline_0, 2, fork, fun);
  if (swapcontext(&cache->context, &fork->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }

  // This is the resume point
  switch (fork->state) {
  case GARBAGE:
    fprintf(stderr, "GD'd!\n");
    garbage_collect_stack(fork);
    break;
  case SUSPENDED:
    // This should probably never happen???
    fprintf(stderr, "SUSPENDED!\n");
    break;
  case RUNNING:
    // This should probably never happen???
    fprintf(stderr, "RUNNING!\n");
    break;
  case DONE:
    // This should probably never happen???
    fprintf(stderr, "DONE!\n");
    break;
  default:
    fprintf(stderr, "NONE OF THE ABOVE!\n");
    assert(false); 
    break;
  }
}

bool done_lazy(lazy_tit_t *tit) {
  /* fprintf(stderr, "\t\t<%p> done: tit %p has stack: %p\n", pthread_self(), tit, tit->context.uc_stack.ss_sp); */
  if(!tit->is_proper) {
    lazy_tit_t *idle_thread_stack = mpmcq_pop(&proper_stacks_for_reuse);
    if (idle_thread_stack) {
      tit->state = GARBAGE;
      setcontext(&idle_thread_stack->context);
    }
  } else {
  }
  return false;
}

void resume_lazy(lazy_tit_t *tit) {
  fprintf(stderr, "\t\t<%p> resume: tit %p has stack: %p\n", pthread_self(), tit, tit->context.uc_stack.ss_sp);
  tit->state = RUNNING;
  current->state = GARBAGE;
  if (current->is_proper) {
    if (swapcontext(&current->context, &tit->context) != 0) {
      err(EX_OSERR, "swapcontext failed");
    }
  } else {
    if (setcontext(&tit->context) != 0) {
      err(EX_OSERR, "setcontext failed");
    }
  }
}

static inline void __start_trampoline(lazy_tit_t *new) {
  new->context.uc_link = NULL;
  current = new;
}

static inline void __end_trampoline(lazy_tit_t *new) {
  new->state = DONE;
}

static void trampoline_0(lazy_tit_t *new, fun_t_0 fun) {
  __start_trampoline(new);
  fun();
  __end_trampoline(new);
}

/* static void trampoline_1(lazy_tit_t *new, fun_t_1 fun, void *a) { */
/*   __start_trampoline(new); */
/*   fun(a); */
/*   __end_trampoline(new); */
/* } */

/* static void trampoline_2(lazy_tit_t *new, fun_t_2 fun, void *a, void *b) { */
/*   __start_trampoline(new); */
/*   fun(a, b); */
/*   __end_trampoline(new); */
/* } */

/* static void trampoline_3(lazy_tit_t *new, fun_t_3 fun, void *a, void *b, void *c) { */
/*   __start_trampoline(new); */
/*   fun(a, b, c); */
/*   __end_trampoline(new); */
/* } */

/* static void trampoline_4(lazy_tit_t *new, fun_t_4 fun, void *a, void *b, void *c, void *d) { */
/*   __start_trampoline(new); */
/*   fun(a, b, c, d); */
/*   __end_trampoline(new); */
/* } */

/* static void trampoline_5(lazy_tit_t *new, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e) { */
/*   __start_trampoline(new); */
/*   fun(a, b, c, d, e); */
/*   __end_trampoline(new); */
/* } */

/* static void trampoline_6(lazy_tit_t *new, fun_t_6 fun, void *a, void *b, void *c, void *d, void *e, void *f) { */
/*   __start_trampoline(new); */
/*   fun(a, b, c, d, e, f); */
/*   __end_trampoline(new); */
/* } */

void lazy_t_resume(lazy_tit_t *stack) {
  stack->state = RUNNING;
  current->state = GARBAGE;
  if (current->is_proper) {
    if (swapcontext(&current->context, &stack->context) != 0) {
      err(EX_OSERR, "setcontext failed");
    }
  } else {
    if (setcontext(&stack->context) != 0) {
      err(EX_OSERR, "setcontext failed");
    }
  }
}

bool lazy_t_done(lazy_tit_t *stack) {
  if(!stack->is_proper) {
    lazy_tit_t *idle_thread_stack = mpmcq_pop(&proper_stacks_for_reuse);
    if (idle_thread_stack) {
      stack->state = GARBAGE;
      setcontext(&idle_thread_stack->context);
    }
  } else {
  }
  return false;
}

bool lazy_t_is_suspended(lazy_tit_t *stack) {
  return stack->state == SUSPENDED;
}

lazy_tit_t *lazy_t_get_current() {
  return current;
}

lazy_tit_t *lazy_t_copy(lazy_tit_t *stack) {
  lazy_tit_t *new = lazy_t_init_new();
  *new = *stack;
  return new;
}

bool lazy_t_is_proper(lazy_tit_t *stack) {
  return stack->is_proper;
}
