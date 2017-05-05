#define PONY_WANT_ATOMIC_DEFS

#include "messageq.h"
#include "../mem/pool.h"
#include "ponyassert.h"
#include <string.h>

#ifdef USE_VALGRIND
#include <valgrind/helgrind.h>
#endif

#ifndef NDEBUG

static size_t messageq_size_debug(messageq_t* q)
{
  pony_msg_t* tail = q->tail;
  size_t count = 0;

  while(atomic_load_explicit(&tail->next, memory_order_relaxed) != NULL)
  {
    count++;
    tail = atomic_load_explicit(&tail->next, memory_order_relaxed);
  }

  return count;
}

#endif

void ponyint_messageq_init(messageq_t* q)
{
  pony_msg_t* stub = POOL_ALLOC(pony_msg_t);
  stub->index = POOL_INDEX(sizeof(pony_msg_t));
  atomic_store_explicit(&stub->next, NULL, memory_order_relaxed);

  atomic_store_explicit(&q->head, (pony_msg_t*)((uintptr_t)stub | 1),
    memory_order_relaxed);
  q->tail = stub;

#ifndef NDEBUG
  messageq_size_debug(q);
#endif
}

void ponyint_messageq_destroy(messageq_t* q)
{
  pony_msg_t* tail = q->tail;
  pony_assert((((uintptr_t)atomic_load_explicit(&q->head, memory_order_acquire) &
    ~(uintptr_t)1)) == (uintptr_t)tail);
#ifdef USE_VALGRIND
#  ifdef NDEBUG
  ANNOTATE_HAPPENS_AFTER(&q->head);
#  endif
  ANNOTATE_HAPPENS_BEFORE_FORGET_ALL(tail);
#endif

  ponyint_pool_free(tail->index, tail);
  atomic_store_explicit(&q->head, NULL, memory_order_relaxed);
  q->tail = NULL;
}

bool ponyint_messageq_push(messageq_t* q, pony_msg_t* m)
{
  atomic_store_explicit(&m->next, NULL, memory_order_relaxed);

  pony_msg_t* prev = atomic_exchange_explicit(&q->head, m,
    memory_order_relaxed);

  bool was_empty = ((uintptr_t)prev & 1) != 0;
  prev = (pony_msg_t*)((uintptr_t)prev & ~(uintptr_t)1);

#ifdef USE_VALGRIND
  ANNOTATE_HAPPENS_BEFORE(&prev->next);
#endif
  atomic_store_explicit(&prev->next, m, memory_order_release);

  return was_empty;
}

pony_msg_t* ponyint_messageq_pop(messageq_t* q)
{
  pony_msg_t* tail = q->tail;
  pony_msg_t* next = atomic_load_explicit(&tail->next, memory_order_relaxed);

  if(next != NULL)
  {
    q->tail = next;
    atomic_thread_fence(memory_order_acquire);
#ifdef USE_VALGRIND
    ANNOTATE_HAPPENS_AFTER(&tail->next);
    ANNOTATE_HAPPENS_BEFORE_FORGET_ALL(tail);
#endif
    ponyint_pool_free(tail->index, tail);
  }

  return next;
}

bool ponyint_messageq_markempty(messageq_t* q)
{
  pony_msg_t* tail = q->tail;
  pony_msg_t* head = atomic_load_explicit(&q->head, memory_order_relaxed);

  if(((uintptr_t)head & 1) != 0)
    return true;

  if(head != tail)
    return false;

  head = (pony_msg_t*)((uintptr_t)head | 1);

#ifdef USE_VALGRIND
  ANNOTATE_HAPPENS_BEFORE(&q->head);
#endif
  return atomic_compare_exchange_strong_explicit(&q->head, &tail, head,
    memory_order_release, memory_order_relaxed);
}
