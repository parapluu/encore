#define _XOPEN_SOURCE

#include <ucontext.h>
#include <assert.h>
#include <sysexits.h>
#include <err.h>
#include <pthread.h>
#include <stdlib.h>
#include "tit_lazy.h"
#include "mpmcq.h"

struct lazy_tit_t {
  ucontext_t context;
  void *stack; // This is used because a stack may be moved underfoot due (e.g. 16 byte alignment on OS X)
  bool is_suspended;
  bool is_proper;
};

static pthread_once_t stack_pool_is_initialized = PTHREAD_ONCE_INIT;
static mpmcq_t proper_stacks_for_reuse;
static mpmcq_t side_stacks_for_reuse;
static __thread lazy_tit_t *current; 
static unsigned int cached_side_stacks;
static unsigned int side_stack_cache_size;

static void trampoline_0(lazy_tit_t *new, fun_t_0 fun);
static void trampoline_1(lazy_tit_t *new, fun_t_1 fun, void *a);
static void trampoline_2(lazy_tit_t *new, fun_t_2 fun, void *a, void *b);
static void trampoline_3(lazy_tit_t *new, fun_t_3 fun, void *a, void *b, void *c);
static void trampoline_4(lazy_tit_t *new, fun_t_4 fun, void *a, void *b, void *c, void *d);
static void trampoline_5(lazy_tit_t *new, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e);
static void trampoline_6(lazy_tit_t *new, fun_t_6 fun, void *a, void *b, void *c, void *d, void *e, void *f);
static void t_resume();
static void t_cleanup(lazy_tit_t *stack);
static inline void __start_trampoline(lazy_tit_t *new);
static inline void __end_trampoline();

static void t_cleanup(lazy_tit_t *stack) {
  assert(stack->is_proper == false);
  bool free_stack = true;

  int current_cache_size;
  do {
    current_cache_size = cached_side_stacks;
    if (__sync_bool_compare_and_swap(&cached_side_stacks, current_cache_size, current_cache_size + 1)) {
      mpmcq_push(&side_stacks_for_reuse, stack->stack);
      free_stack = false;
      break;
    }
  } while (current_cache_size < side_stack_cache_size);
  
  if (free_stack) free(stack->stack);
  free(stack);
}

static void t_resume() {
  if (current->is_proper) {
    mpmcq_push(&proper_stacks_for_reuse, &current);
  } else {
    t_cleanup(current);
  }
}

static void __init_pool() {
  mpmcq_init(&proper_stacks_for_reuse);
  mpmcq_init(&side_stacks_for_reuse);
}

void init_lazy_system(int __side_stack_cache_size) {
  pthread_once(&stack_pool_is_initialized, __init_pool);
  // Silently fail if attempt to reset
  if (!side_stack_cache_size) {
    side_stack_cache_size = __side_stack_cache_size;
  }
}

lazy_tit_t *lazy_t_init_current() {
  current = lazy_t_init_new();
  // TODO: assert this has not been initialized
  current->stack        = NULL;
  current->is_suspended = false;
  current->is_proper    = true;
  return current;
}

lazy_tit_t *lazy_t_init_new() {
  lazy_tit_t *stack = calloc(1, sizeof(struct lazy_tit_t));
  return stack;
}

static inline void __t_fork(lazy_tit_t *new) {
  getcontext(&new->context); 
  new->context.uc_stack.ss_flags = 0; 
  new->context.uc_link           = 0; 
  new->is_suspended		 = false; 
  new->is_proper		 = false; 
  current->is_suspended		 = true; 

  while (new->stack == NULL && cached_side_stacks > 0) {
    new->stack = mpmcq_pop(&side_stacks_for_reuse);
  }
  if (new->stack) {
    int cache_size;
    do {
      cache_size = cached_side_stacks;
    } while (__sync_bool_compare_and_swap(&cached_side_stacks, cache_size, cache_size - 1));
  }
  if (new->stack == NULL) {
    new->stack = calloc(1, new->context.uc_stack.ss_size);
    new->context.uc_stack.ss_sp = new->stack; 
  }
}

static inline void __t_fork_end(lazy_tit_t *new) {
  t_resume();
  current = new;
  current->is_suspended = false;
}

void lazy_t_fork_0(lazy_tit_t *new, void(*fun)()) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_0, 2, new, fun);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

void lazy_t_fork_1(lazy_tit_t *new, void(*fun)(void* a), void *a) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_1, 3, new, fun, a);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

void lazy_t_fork_2(lazy_tit_t *new, void(*fun)(void* a, void *b), void *a, void *b) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_2, 4, new, fun, a, b);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

void lazy_t_fork_3(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c), void *a, void *b, void *c) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_3, 5, new, fun, a, b, c);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

void lazy_t_fork_4(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c, void *d), void *a, void *b, void *c, void *d) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_4, 6, new, fun, a, b, c, d);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

void lazy_t_fork_5(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c, void *d, void* e), void *a, void *b, void *c, void *d, void *e) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_5, 7, new, fun, a, b, c, d, e);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

void lazy_t_fork_6(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c, void *d, void* e, void*f), void *a, void *b, void *c, void *d, void *e, void *f) {
  __t_fork(new);
  lazy_tit_t *cache = current;
  makecontext(&new->context, trampoline_6, 8, new, fun, a, b, c, d, e, f);
  if (swapcontext(&cache->context, &new->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
  __t_fork_end(new);
}

static inline void __start_trampoline(lazy_tit_t *new) {
  current = new; 
}

static inline void __end_trampoline() {
}

static void trampoline_0(lazy_tit_t *new, fun_t_0 fun) {
  __start_trampoline(new);
  fun();
  __end_trampoline();
}

static void trampoline_1(lazy_tit_t *new, fun_t_1 fun, void *a) {
  __start_trampoline(new);
  fun(a);
  __end_trampoline();
}

static void trampoline_2(lazy_tit_t *new, fun_t_2 fun, void *a, void *b) {
  __start_trampoline(new);
  fun(a, b);
  __end_trampoline();
}

static void trampoline_3(lazy_tit_t *new, fun_t_3 fun, void *a, void *b, void *c) {
  __start_trampoline(new);
  fun(a, b, c);
  __end_trampoline();
}

static void trampoline_4(lazy_tit_t *new, fun_t_4 fun, void *a, void *b, void *c, void *d) {
  __start_trampoline(new);
  fun(a, b, c, d);
  __end_trampoline();
}

static void trampoline_5(lazy_tit_t *new, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e) {
  __start_trampoline(new);
  fun(a, b, c, d, e);
  __end_trampoline();
}

static void trampoline_6(lazy_tit_t *new, fun_t_6 fun, void *a, void *b, void *c, void *d, void *e, void *f) {
  __start_trampoline(new);
  fun(a, b, c, d, e, f);
  __end_trampoline();
}

void lazy_t_resume(lazy_tit_t *stack) {
  lazy_tit_t *cache = current;
  stack->is_suspended = false;
  cache->is_suspended = true;
  if (swapcontext(&cache->context, &stack->context) != 0) {
    err(EX_OSERR, "swapcontext failed");
  }
}

bool lazy_t_done(lazy_tit_t *stack) {
  assert(stack->is_proper == false);

  lazy_tit_t *idle_thread_stack = mpmcq_pop(&proper_stacks_for_reuse);
  if (idle_thread_stack) {
    setcontext(&idle_thread_stack->context);
  }
  return false;
}

bool lazy_t_is_suspended(lazy_tit_t *stack) {
  return stack->is_suspended;
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

