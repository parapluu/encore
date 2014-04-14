#include "pool.h"
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <sys/mman.h>
#include <assert.h>

#define POOL_ALIGN_MASK ((1 << 12) - 1)
#define POOL_SIZE (1 << POOL_SIZE_BITS)
#define POOL_MIN (1 << POOL_MIN_BITS)

typedef struct pool_local_t
{
  void* next;
} pool_local_t;

typedef struct pool_central_t
{
  void* next;
  uintptr_t len;
  struct pool_central_t* central;
} pool_central_t;

typedef struct pool_cmp_t
{
  union
  {
    struct
    {
      uint64_t aba;
      pool_central_t* node;
    };

    pool_aba_t dw;
  };
} pool_cmp_t;

pool_global_t pool_global[POOL_COUNT] =
{
  {POOL_MIN << 0, POOL_SIZE / (POOL_MIN << 0), 0},
  {POOL_MIN << 1, POOL_SIZE / (POOL_MIN << 1), 0},
  {POOL_MIN << 2, POOL_SIZE / (POOL_MIN << 2), 0},
  {POOL_MIN << 3, POOL_SIZE / (POOL_MIN << 3), 0},
  {POOL_MIN << 4, POOL_SIZE / (POOL_MIN << 4), 0},
  {POOL_MIN << 5, POOL_SIZE / (POOL_MIN << 5), 0},
  {POOL_MIN << 6, POOL_SIZE / (POOL_MIN << 6), 0},
  {POOL_MIN << 7, POOL_SIZE / (POOL_MIN << 7), 0},
  {POOL_MIN << 8, POOL_SIZE / (POOL_MIN << 8), 0},
  {POOL_MIN << 9, POOL_SIZE / (POOL_MIN << 9), 0},
};

__thread pool_thread_t pool_thread[POOL_COUNT];

static void pool_push(pool_thread_t* thread, pool_global_t* global)
{
  pool_cmp_t cmp, xchg;
  pool_central_t* p = thread->pool;
  p->len = thread->length;

  do
  {
    cmp.dw = global->central;
    p->central = cmp.node;
    xchg.node = p;
    xchg.aba = cmp.aba + 1;
  } while(!__sync_bool_compare_and_swap(&global->central, cmp.dw, xchg.dw));

  thread->pool = NULL;
  thread->length = 0;
}

static bool pool_pull(pool_thread_t* thread, pool_global_t* global)
{
  pool_cmp_t cmp, xchg;
  pool_central_t* next;

  do
  {
    cmp.dw = global->central;
    next = cmp.node;

    if(next == NULL)
    {
      return false;
    }

    xchg.node = cmp.node->central;
    xchg.aba = cmp.aba + 1;
  } while(!__sync_bool_compare_and_swap(&global->central, cmp.dw, xchg.dw));

  thread->pool = next;
  thread->length = next->len;

  return true;
}

static void* pool_get(pool_thread_t* thread, pool_global_t* global)
{
  pool_local_t* p = thread->pool;

  if(p != NULL)
  {
    thread->pool = p->next;
    thread->length--;
  } else if(thread->start < thread->end) {
    p = thread->start;
    thread->start += global->size;
  } else if(pool_pull(thread, global)) {
    return pool_get(thread, global);
  }

  return p;
}

static void* pool_pages(pool_thread_t* thread, pool_global_t* global)
{
  pool_local_t* p = mmap(
    0,
    POOL_SIZE,
    PROT_READ | PROT_WRITE,
    MAP_PRIVATE | MAP_ANON,
    -1,
    0
    );

  if(p == MAP_FAILED) { abort(); }

  assert(((uintptr_t)p & POOL_ALIGN_MASK) == 0);

  thread->start = ((void*)p) + global->size;
  thread->end = ((void*)p) + POOL_SIZE;

  return p;
}

void* pool_alloc(pool_thread_t* thread, pool_global_t* global)
{
  pool_local_t* t = pool_get(thread, global);

  if(t == NULL)
  {
    t = pool_pages(thread, global);
  }

  return t;
}

void pool_free(void* p, pool_thread_t* thread, pool_global_t* global)
{
  if(thread->length >= global->count)
  {
    pool_push(thread, global);
  }

  pool_local_t* lp = p;
  lp->next = thread->pool;
  thread->pool = p;
  thread->length++;
}
