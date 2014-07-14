#include "mpmcq.h"
#include "../mem/pool.h"

struct mpmcq_node_t
{
  struct mpmcq_node_t* next;
  void* data;
};

void mpmcq_init( mpmcq_t* q )
{
  mpmcq_node_t* node = POOL_ALLOC(mpmcq_node_t);
  node->data = NULL;
  node->next = NULL;

  q->head = node;
  q->tail.node = node;
}

void mpmcq_destroy(mpmcq_t* q)
{
  POOL_FREE(mpmcq_node_t, q->tail.node);
  q->head = NULL;
  q->tail.node = NULL;
}

void mpmcq_push(mpmcq_t* q, void* data)
{
  mpmcq_node_t* node = POOL_ALLOC(mpmcq_node_t);
  node->data = data;
  node->next = NULL;

  mpmcq_node_t* prev = __atomic_exchange_n(&q->head, node, __ATOMIC_RELAXED);
  __atomic_store_n(&prev->next, node, __ATOMIC_RELEASE);
}

void* mpmcq_pop(mpmcq_t* q)
{
  mpmcq_dwcas_t cmp, xchg;
  mpmcq_node_t* next;
  void* data;

  cmp.dw = q->tail.dw;

  do
  {
    next = __atomic_load_n(&cmp.node->next, __ATOMIC_ACQUIRE);

    if(next == NULL)
      return NULL;

    data = next->data;
    xchg.node = next;
    xchg.aba = cmp.aba + 1;
  } while(!__atomic_compare_exchange_n(&q->tail.dw, &cmp.dw, xchg.dw, false,
    __ATOMIC_RELAXED, __ATOMIC_RELAXED));

  POOL_FREE(mpmcq_node_t, cmp.node);
  return data;
}
