#include "messageq.h"
#include "../mem/pool.h"
#include <string.h>
#include <assert.h>

void messageq_init(messageq_t* q)
{
  message_t* stub = POOL_ALLOC(message_t);
  memset(stub, 0, sizeof(message_t));

  q->head = (message_t*)((uintptr_t)stub | 1);
  q->tail = stub;
}

void messageq_destroy(messageq_t* q)
{
  assert(((uintptr_t)q->head & ~(uintptr_t)1) == (uintptr_t)q->tail);

  POOL_FREE(message_t, q->tail);
  q->head = NULL;
  q->tail = NULL;
}

bool messageq_push(messageq_t* q, uint64_t id, int argc, pony_arg_t* argv)
{
  message_t* msg = POOL_ALLOC(message_t);
  msg->id = id;
  memcpy(msg->argv, argv, argc * sizeof(pony_arg_t));
  msg->next = NULL;

  message_t* prev = __atomic_exchange_n(&q->head, msg, __ATOMIC_RELAXED);
  bool was_empty = ((uintptr_t)prev & 1) != 0;
  prev = (message_t*)((uintptr_t)prev & ~(uintptr_t)1);
  __atomic_store_n(&prev->next, msg, __ATOMIC_RELEASE);

  return was_empty;
}

message_t* messageq_pop(messageq_t* q)
{
  message_t* tail = q->tail;
  message_t* next = __atomic_load_n(&tail->next, __ATOMIC_ACQUIRE);

  if(next != NULL)
  {
    q->tail = next;
    POOL_FREE(message_t, tail);
  }

  return next;
}

bool messageq_markempty(messageq_t* q)
{
  message_t* tail = q->tail;
  message_t* head = __atomic_load_n(&q->head, __ATOMIC_RELAXED);

  if(((uintptr_t)head & 1) != 0)
    return true;

  if(head != tail)
    return false;

  head = (message_t*)((uintptr_t)head | 1);
  return __atomic_compare_exchange_n(&q->head, &tail, head, false,
    __ATOMIC_RELAXED, __ATOMIC_RELAXED);
}

// Find in messageq. No need of lock free algorithm.
bool messageq_find(messageq_t* q, void* e)
{
  message_t* m = q->head;
  bool found = false;
  while(m!=NULL){
    if(m->argv[0].p == e){
      found = true;
    }
    m = m->next;
  }
  return found;
}
