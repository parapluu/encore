#define _XOPEN_SOURCE 800

#include "shared_object.h"
#include "mem/pool.h"
#include "sched/scheduler.h"
#include "actor/actor.h"
#include <assert.h>
#include <stdio.h>
#include <stdint.h>

typedef struct queue_node_t
{
  void* data;
  queue_node_t* next;
} queue_node_t;

typedef struct duration_t {
  to_trace_t *head;
  uint32_t entry;
  uint32_t dependency;
  union {
    struct {
      uint32_t closed;
      uint32_t exit;
    };
    uint64_t closed_and_exit;
  };
  int collector;
  int collectable;
} duration_t;

#define _atomic_sub(PTR, VAL) \
  (__atomic_fetch_sub(PTR, VAL, __ATOMIC_RELEASE))

typedef struct trace_address_list {
  void *address;
  struct trace_address_list *next;
} trace_address_list;

typedef struct to_trace_t {
  duration_t *duration;
  trace_address_list *address;
  int head_candidate;
  bool exited;
} to_trace_t;

static void mpscq_init(mpscq_t* q)
{
  queue_node_t* node = POOL_ALLOC(queue_node_t);
  node->data = NULL;
  node->next = NULL;

  q->tail = q->head = node;
}

static void mpscq_destroy(mpscq_t* q)
{
  POOL_FREE(queue_node_t, q->tail);
  q->head = NULL;
  q->tail= NULL;
}

static queue_node_t* mpscq_push(mpscq_t *q, void *data)
{

  queue_node_t* node = POOL_ALLOC(queue_node_t);
  node->data = data;
  node->next = NULL;

  assert(q->head);
  queue_node_t* prev = (queue_node_t*)_atomic_exchange(&q->head, node);
  _atomic_store(&prev->next, node);
  return node;
}

static void mpscq_push_single(mpscq_t *q, void *data)
{
  queue_node_t* node = POOL_ALLOC(queue_node_t);
  node->data = data;
  node->next = NULL;

  assert(q->head);
  queue_node_t* prev = q->head;
  q->head = node;
  prev->next = node;
}

static void *mpscq_pop(mpscq_t *q)
{
  assert(q->head);

  queue_node_t *tail = q->tail;
  queue_node_t *next = _atomic_load(&tail->next);

  if (next == NULL) {
    return NULL;
  }
  void *data = next->data;
  q->tail = next;
  POOL_FREE(queue_node_t, tail);
  return data;
}

static void *mpscq_peek(mpscq_t *q)
{
  assert(q->head);
  queue_node_t *tail = _atomic_load(&q->tail);

  queue_node_t *next = _atomic_load(&tail->next);
  if (next == NULL) {
    return NULL;
  }
  return next->data;
}

static queue_node_t *next_node_of_not_exit_item(queue_node_t *node)
{
  to_trace_t *item;
  while ((node = _atomic_load(&node->next))) {
    item = node->data;
    if (!_atomic_load(&item->exited)) {
      return node;
    }
  }
  return NULL;
}

static void clean_one(to_trace_t *item)
{
  pony_ctx_t *ctx = pony_ctx();
  {
    trace_address_list *cur = item->address;
    trace_address_list *pre;
    while (true) {
      if (!cur) {
        break;
      }
      pony_traceobject(ctx, cur->address, NULL);
      pre = cur;
      cur = cur->next;
      POOL_FREE(trace_address_list, pre);
    }
  }
  POOL_FREE(to_trace_t, item);
}

static void collect(encore_so_t *this)
{
  do {
    duration_t *d = mpscq_pop(&this->so_gc.duration_q);
    assert(d->collectable == 1);
    for (size_t i = 0; i < d->exit; ++i) {
      clean_one(mpscq_pop(&this->so_gc.in_out_q));
    }
    POOL_FREE(duration_t, d);
    d = mpscq_peek(&this->so_gc.duration_q);
    if (d == NULL || _atomic_load(&d->collectable) != 1) {
      return;
    }
    int old_collector = 0;
    if (_atomic_cas(&d->collector, &old_collector, 1) || old_collector == 3) {
      continue;
    } else {
      return;
    }
  } while (true);
}

encore_so_t *encore_create_so(pony_ctx_t *ctx, pony_type_t *type)
{
  encore_so_t *this = (encore_so_t*) encore_create(ctx, type);
  this->so_gc.cas.current = (void*)-1;
  this->so_gc.node_of_head = NULL;
  mpscq_init(&this->so_gc.in_out_q);
  mpscq_init(&this->so_gc.duration_q);
  return this;
}

to_trace_t *so_to_trace_new(encore_so_t *this)
{
  to_trace_t *ret = POOL_ALLOC(to_trace_t);
  ret->head_candidate = 0;
  ret->address = NULL;
  ret->exited = false;
  return ret;
}

void so_to_trace(to_trace_t *item, void *p)
{
  trace_address_list *new = POOL_ALLOC(trace_address_list);
  new->address = p;
  new->next = item->address;
  item->address = new;
}

static void set_collectable(encore_so_t *this, duration_t *d)
{
  if (!_atomic_load(&d->closed)) {
    return;
  }
  if (_atomic_load(&d->collectable) == 1) {
    return;
  }
  int old_collectable = 0;
  // maybe the second load could be eliminated, because ...
  if (_atomic_load(&d->exit) + _atomic_load(&d->dependency) ==
      _atomic_load(&d->entry)) {
    if (_atomic_cas(&d->collectable, &old_collectable, 1)) {
      if (mpscq_peek(&this->so_gc.in_out_q) == d->head) {
        int old_collector = 0;
        if (_atomic_cas(&d->collector, &old_collector, 2)) {
          collect(this);
        }
      } else {
        _atomic_store(&d->collector, 3);
      }
    }
  }
}

static duration_t *duration_new(to_trace_t *head)
{
  duration_t *new = POOL_ALLOC(duration_t);
  new->head = head;
  new->entry = 0;
  new->dependency = 0;
  new->closed = false;
  new->exit = 0;
  new->collector = 0;
  new->collectable = 0;
  return new;
}

static duration_t* set_new_head(so_gc_t *so_gc)
{
  dwcas_t cmp, xchg;
  cmp.current = _atomic_load(&so_gc->cas.current);
  cmp.aba = _atomic_load(&so_gc->cas.aba);
  if (cmp.current != NULL) {
    return cmp.current;
  }

  assert(cmp.current == NULL);

  queue_node_t *next = next_node_of_not_exit_item(
      _atomic_load(&so_gc->node_of_head)
      );
  xchg.aba = cmp.aba + 1;
  if (next) {
    to_trace_t *item = next->data;
    int old_head_candidate = 0;
    assert(item);
    if (!_atomic_cas(&item->head_candidate, &old_head_candidate, 1)) {
      return NULL;
    }
    xchg.current = duration_new(next->data);
    if (_atomic_dwcas(&so_gc->cas.dw, &cmp.dw, xchg.dw)) {
      _atomic_store(&so_gc->node_of_head, next);
      return xchg.current;
    } else {
      POOL_FREE(duration_t, xchg.current);
      return cmp.current;
    }
  }

  xchg.current = (void*)-1;
  if (_atomic_dwcas(&so_gc->cas.dw, &cmp.dw, xchg.dw)) {
    _atomic_store(&so_gc->node_of_head, NULL);
    return xchg.current;
  } else {
    return cmp.current;
  }
}

static duration_t* helper_prev_head_exit(encore_so_t *this, so_gc_t *so_gc)
{
  dwcas_t cmp;
  cmp.current = _atomic_load(&so_gc->cas.current);
  while (cmp.current == NULL) {
    queue_node_t *node = _atomic_load(&so_gc->node_of_head);
    cmp.current = _atomic_load(&so_gc->cas.current);
    duration_t *prev_d;
    if (cmp.current != NULL) {
      break;
    }
    assert(node);
    prev_d = node->data;
    int old_collectable = 0;
    if (_atomic_cas(&prev_d->collectable, &old_collectable, 2)) {
      cmp.current = set_new_head(so_gc);
      _atomic_store(&prev_d->collectable, 0);
      set_collectable(this, prev_d);
    } else {
      cmp.current = _atomic_load(&so_gc->cas.current);
      // relax
#if defined(PLATFORM_IS_X86) && !defined(PLATFORM_IS_VISUAL_STUDIO)
      asm volatile("pause" ::: "memory");
#endif
    }
  }
  return cmp.current;
}

void so_lockfree_on_entry(encore_so_t *this, to_trace_t *item)
{
  dwcas_t cmp, xchg;
  so_gc_t *so_gc = &this->so_gc;
  assert(so_gc->in_out_q.head);
  do {
    cmp.current = helper_prev_head_exit(this, so_gc);
    assert(cmp.current != NULL);
    if (cmp.current == (void*)-1) {
      cmp.aba = _atomic_load(&so_gc->cas.aba);
      duration_t *new = duration_new(item);
      xchg.current = new;
      xchg.aba = cmp.aba + 1;
      while (!_atomic_dwcas(&so_gc->cas.dw, &cmp.dw, xchg.dw)) {
        if (cmp.current != (void*)-1) {
          POOL_FREE(duration_t, new);
          break;
        }
        xchg.aba = cmp.aba + 1;
      }
      cmp.current = so_gc->cas.current;
    }
    _atomic_add(&cmp.current->entry, 1);
    if (_atomic_load(&cmp.current->closed)) {
      _atomic_sub(&cmp.current->entry, 1);
      set_collectable(this, cmp.current);
      continue;
    } else {
      item->duration = cmp.current;
      if (cmp.current->head == item) {
        assert(_atomic_load(&so_gc->node_of_head) == NULL);
        _atomic_store(&so_gc->node_of_head,
            mpscq_push(&this->so_gc.in_out_q, item)
            );
      } else {
        mpscq_push(&this->so_gc.in_out_q, item);
      }
      return;
    }
  } while (true);
}

static void exit_as_head(encore_so_t *this, dwcas_t cmp, to_trace_t *item)
{
  so_gc_t *so_gc = &this->so_gc;
  dwcas_t xchg;
  _atomic_store(&cmp.current->closed, true);
  mpscq_push_single(&so_gc->duration_q, cmp.current);
  xchg.aba = cmp.aba + 1;
  xchg.current = NULL;
  _atomic_store(&so_gc->cas.dw, xchg.dw);
  set_new_head(so_gc);
  if (item->duration == cmp.current) {
    _atomic_add(&cmp.current->exit, 1);
  } else {
    assert(item->duration->closed == 1);
    _atomic_add(&item->duration->dependency, 1);
    set_collectable(this, item->duration);
  }
  set_collectable(this, cmp.current);
}

static void exit_as_not_head(encore_so_t *this, to_trace_t *item)
{
  uint64_t old_closed_and_exit = _atomic_load(&item->duration->closed_and_exit);
  do {
    if (old_closed_and_exit >> 32 == 1) {
      _atomic_add(&item->duration->dependency, 1);
      set_collectable(this, item->duration);
      return;
    }
    assert(old_closed_and_exit >> 32 == 0);
    uint64_t new_closed_and_exit = old_closed_and_exit + 1;
    if (_atomic_cas(&item->duration->closed_and_exit, &old_closed_and_exit,
          new_closed_and_exit)) {
        set_collectable(this, item->duration);
        return;
    }
  } while (true);
}

void so_lockfree_on_exit(encore_so_t *this, to_trace_t *item)
{
  dwcas_t cmp, xchg;
  so_gc_t *so_gc = &this->so_gc;
  cmp.current = _atomic_load(&so_gc->cas.current);
  cmp.aba = _atomic_load(&so_gc->cas.aba);
  _atomic_store(&item->exited, true);
  int old_head_candidate = 0;
  if (!_atomic_cas(&item->head_candidate, &old_head_candidate, 2)) {
    cmp.current = _atomic_load(&so_gc->cas.current);
    if (!cmp.current) {
      xchg.aba = cmp.aba + 1;
      xchg.current = NULL;
      if (_atomic_dwcas(&so_gc->cas.dw, &cmp.dw, xchg.dw)) {
        exit_as_not_head(this, item);
        return;
      }
    }
    assert(_atomic_load(&cmp.current->head) == item);
    exit_as_head(this, cmp, item);
  } else {
    if (cmp.current && _atomic_load(&cmp.current->head) == item) {
      exit_as_head(this, cmp, item);
    } else {
      exit_as_not_head(this, item);
    }
  }
}

void encore_so_finalinzer(void *p)
{
  printf("finalizer %p\n", p);
  assert(p);
  encore_so_t *this = p;
  assert(mpscq_pop(&this->so_gc.in_out_q) == NULL);
  assert(mpscq_pop(&this->so_gc.duration_q) == NULL);
  mpscq_destroy(&this->so_gc.in_out_q);
  mpscq_destroy(&this->so_gc.duration_q);
}

void so_lockfree_send(pony_ctx_t *ctx)
{
  void *p;
  while(ctx->lf_tmp_stack != NULL) {
    ctx->lf_tmp_stack = gcstack_pop(ctx->lf_tmp_stack, &p);
    gc_double_inc_send(ctx, p);
  }
  gc_double_inc_send_done(ctx);
}

void so_lockfree_unsend(pony_ctx_t *ctx)
{
  void *p;
  while(ctx->lf_tmp_stack != NULL) {
    ctx->lf_tmp_stack = gcstack_pop(ctx->lf_tmp_stack, &p);
  }
}

void mv_tmp_to_acc(pony_ctx_t *ctx)
{
  void *p;
  while(ctx->lf_tmp_stack != NULL) {
    ctx->lf_tmp_stack = gcstack_pop(ctx->lf_tmp_stack, &p);
    ctx->lf_acc_stack = gcstack_push(ctx->lf_acc_stack, p);
  }
}

void so_lockfree_acc_recv(pony_ctx_t *ctx, to_trace_t *item)
{
  void *p;
  while(ctx->lf_acc_stack != NULL) {
    ctx->lf_acc_stack = gcstack_pop(ctx->lf_acc_stack, &p);
    so_to_trace(item, p);
    gc_recv_address(ctx, p);
  }
  gc_recv_address_done(ctx);
}

void so_lockfree_set_trace_boundary(pony_ctx_t *ctx, void *p)
{
  ctx->boundary = p;
}
