//#define DEBUG_PRINT 1

#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <assert.h>
#include <pthread.h>
#include "future.h"
#include "future_actor.h"
#include "pony/pony.h"
#include "tit_lazy.h"
#include "tit_eager.h"

// #include "ext/pony_extensions.h"

extern void run_restart();
extern void block_actor(pony_actor_t *a);

struct resumable {
  // strategy_t strategy; // Possible future work: support multiple strategies in parallel
  union {
    lazy_tit_t *lazy;
    eager_tit_t *eager;
  };
};

extern void run_restart();

static strategy_t strategy;

static inline void suspend(resumable_t *r) {
  switch (strategy) {
  case LAZY:
    fork_lazy(run_restart);
    break;
  case EAGER:
    suspend_eager(r->eager);
    break;
  default:
    assert(false);
  }
}

static resumable_t *mk_resumeable() {
  resumable_t *r = pony_alloc(sizeof(resumable_t));
  switch (strategy) {
  case LAZY:
    r->lazy = lazy_t_get_current();
    break;
  case EAGER:
    r->eager = get_suspendable_tit();
    break;
  default:
    assert(false);
  }
  return r;
}

typedef struct chained_entry {
  pony_actor_t *actor;
  struct closure *closure;
} chained_entry;


static inline set_t *get_chained(future_t *f) {
  if (f->chained == NULL) {
    f->chained = mk_set();
  }
  return f->chained;
}

static inline set_t *get_blocked(future_t *f) {
  if (f->blocked == NULL) {
    f->blocked = mk_set();
  }
  return f->blocked;
}

static void run_chain(chained_entry *entry, void *value) {
  pony_actor_t *target = entry->actor;
  struct closure *closure = entry->closure;
  pony_arg_t argv[2];
  argv[0].p = closure;
  argv[1].p = value;
  pony_sendv(target, FUT_MSG_RUN_CLOSURE, 2, argv); // - see https://trello.com/c/kod5Ablj
}

static void resume_from_block(pony_actor_t *a) {
  pony_schedule(a);
}


void future_resume(resumable_t *r) {
    return;
  switch (strategy) {
  case LAZY:
    resume_lazy(r->lazy);
    break;
  case EAGER:
    resume_eager(r->eager);
    break;
  default:
    assert(false);
  }
}

future_t *future_mk() {
  future_t *f = pony_alloc(sizeof *f);
  pthread_mutex_init(&f->lock, NULL);
  f->fulfilled = false;
  f->value = NULL;
  f->blocked = NULL;
  f->chained = NULL;
  f->awaiting = NULL;
  return f;
}

void future_chain(future_t *f, pony_actor_t* a, struct closure *c) {
  chained_entry *new_entry = pony_alloc(sizeof(chained_entry));
  new_entry->actor = a;
  new_entry->closure = c;

  pthread_mutex_lock(&f->lock);
  set_t *chained = get_chained(f);
  set_add(chained, new_entry);

  f->has_blocking =  true; 
  pthread_mutex_unlock(&f->lock);
}

void future_block(future_t *f, pony_actor_t* a) {
  pthread_mutex_lock(&f->lock);
  if(f->fulfilled){
    return;
  } 
  pony_unschedule(a);

  f->has_blocking =  true;

  set_t *blocked = get_blocked(f);
  set_add(blocked, a);
  pthread_mutex_unlock(&f->lock);
  
  block_actor(a);
}

inline bool future_fulfilled(future_t *f) {
  return f->fulfilled;
}

inline void *future_read_value(future_t *f) {
  return f->value;
}

void future_fulfil(future_t *f, void *value) {
  pthread_mutex_lock(&f->lock);
  f->fulfilled = true;
  f->value = value;

  if (f->has_blocking) {
    set_forall(get_chained(f), (forall_fnc) run_chain, (void*) value);
    set_forall(get_blocked(f), (forall_fnc) resume_from_block, NULL );
  }
  pthread_mutex_unlock(&f->lock);
}

void init_futures(int cache_size, strategy_t s) {
  static bool init = false;
  if (init == false) {
    strategy = s;
    init_system(cache_size);
    init = true;
  }
}

void future_run_loop_start() {
  if (strategy == LAZY) {
    lazy_tit_t *current = lazy_t_get_current();
    if (current) {
      done_lazy(current);
    }
  }
}
