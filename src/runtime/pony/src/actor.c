#include "actor.h"
#include "actorq.h"
#include "heap.h"
#include "pool.h"
#include "map.h"
#include "pagemap.h"
#include "scheduler.h"
#include "cycle.h"
#include <assert.h>

struct pony_actor_t
{
  void* p;
  uint32_t rc;
  uint32_t thread;
  bool blocked;
  bool mark;
  bool refchanged;

  heap_t heap;
  map_t* foreign_ref;
  map_t* local_ref;

  // keep things accessed by other actors on a separate cache line
  actorq_t q __attribute__ ((aligned (64)));
  pony_actor_type_t* actor_type;
};

typedef enum
{
  TRACE_MARK,
  TRACE_SEND,
  TRACE_RECEIVE,
} trace_t;

static __thread pony_actor_t* this;
static __thread pony_actor_t* dest;
static __thread map_t* to_map;
static __thread trace_t trace;
static bool disable_cd;

static void trace_mark(pony_actor_t* actor, void* p, pony_trace_fn f,
  pony_mode_t mode)
{
  if(p == NULL) return;

  switch(mode)
  {
    case PONY_ACTOR:
      map_markactor(&actor->foreign_ref, &to_map, p);
      break;

    case PONY_MUTABLE:
    {
      chunk_t* chunk = pagemap_get(p);
      pony_actor_t* owner = heap_owner(chunk);
      bool traverse;

      if(owner == actor)
      {
        traverse = heap_mark(chunk, p);
      } else {
        traverse = map_markobject(&actor->foreign_ref, &to_map, owner, p);
      }

      if(traverse && (f != NULL)) f(p);
      break;
    }

    case PONY_IMMUTABLE:
    case PONY_OPAQUE:
    {
      /* FIX: opaque mark
      don't traverse, someone else may have a mutable pointer to
        the same object. but if we run into it again in a non-opaque context,
        then DO traverse (ignoring the mark!)
      */

      chunk_t* chunk = pagemap_get(p);
      pony_actor_t* owner = heap_owner(chunk);

      if(owner == actor)
      {
        if(heap_mark(chunk, p) && (f != NULL)) f(p);
      } else {
        map_markobject(&actor->foreign_ref, &to_map, owner, p);
      }
      break;
    }

    default: break;
  }
}

static void trace_send(pony_actor_t* actor, pony_actor_t* to, void* p,
  pony_trace_fn f, pony_mode_t mode)
{
  if(p == NULL) return;

  switch(mode)
  {
    case PONY_ACTOR:
      if(p == actor)
      {
        if(!actor->mark)
        {
          actor->rc++;
          actor->mark = true;
        }
      } else if(p != to) {
        map_sendactor(&actor->foreign_ref, &to_map, p);
      }
      break;

    case PONY_MUTABLE:
    {
      pony_actor_t* owner = heap_owner(pagemap_get(p));
      bool traverse;

      if(owner == actor)
      {
        traverse = map_sendlocal(&actor->local_ref, p, f);
      } else if(owner == to) {
        // we don't reduce our rc or send an rcdec for this object
        // don't want the rcdec to arrive before the object does
        traverse = map_markobject(&actor->foreign_ref, &to_map, owner, p);
      } else {
        traverse = map_sendobject(&actor->foreign_ref, &to_map, owner, p);
      }

      if(traverse && (f != NULL)) f(p);
      break;
    }

    case PONY_IMMUTABLE:
    case PONY_OPAQUE:
    {
      // FIX: argh opaque
      pony_actor_t* owner = heap_owner(pagemap_get(p));

      if(owner == actor)
      {
        map_sendlocal(&actor->local_ref, p, f);
      } else if(owner != to) {
        map_sendobject(&actor->foreign_ref, &to_map, owner, p);
      }
      break;
    }

    default: break;
  }
}

static void trace_receive(pony_actor_t* actor, void* p, pony_trace_fn f,
  pony_mode_t mode)
{
  if(p == NULL) return;

  switch(mode)
  {
    case PONY_ACTOR:
      if(p != actor)
      {
        // FIX: only refchanged if p wasn't in foreign_ref
        map_receiveactor(&actor->foreign_ref, p);
        actor->refchanged = true;
      }
      break;

    case PONY_MUTABLE:
    {
      pony_actor_t* owner = heap_owner(pagemap_get(p));
      bool traverse;

      if(owner == actor)
      {
        // only traverse if it's the first time we've seen it in this message
        traverse = map_receivelocal(&actor->local_ref, p);
      } else {
        traverse = map_receiveobject(&actor->foreign_ref, owner, p);
      }

      if(traverse && (f != NULL)) f(p);
      break;
    }

    case PONY_IMMUTABLE:
    case PONY_OPAQUE:
    {
      // FIX: argh opaque
      pony_actor_t* owner = heap_owner(pagemap_get(p));

      if(owner != actor)
      {
        map_receiveobject(&actor->foreign_ref, owner, p);
      }
      break;
    }

    default: break;
  }
}

static void push_message(pony_actor_t* to, uint64_t id, int argc, pony_arg_t* argv)
{
  if(actorq_push(&to->q, id, argc, argv)) scheduler_add(to, to->thread);
}

static bool handle_message(pony_actor_t* actor, message_t* msg,
  uint64_t* app_msgs, uint64_t* rc_msgs)
{
  switch(msg->id)
  {
    case ACTORMSG_RC:
      (*rc_msgs)++;
      actor->rc += msg->argv[0].i;
      map_receiverc(&actor->local_ref, msg->argv[1].p);

      if(actor->blocked && (msg->argv[0].i != 0))
      {
        // if we are blocked and our RC changes, inform the CD
        cycle_updaterc(actor, actor->rc);
      }
      return false;

    case ACTORMSG_CONF:
      (*rc_msgs)++;
      cycle_ack(msg->argv[0].i);
      return false;

    case PONY_MAIN:
      // this message is only delivered when the program starts
      assert(!actor->blocked);
      assert(actor->rc == 0);

      (*app_msgs)++;
      actor->actor_type->dispatch(actor, actor->p, msg->id, 2, msg->argv);
      return true;

    default:
    {
      (*app_msgs)++;

      if(actor->blocked)
      {
        // if we are blocked and we get an APP message, inform the CD
        cycle_unblock(actor);
        actor->blocked = false;
      }

      pony_msg_t* mtype = actor->actor_type->msg(msg->id);
      assert(mtype->argc <= PONY_MAX_ARG);

      if(mtype->argc > 0)
      {
        trace = TRACE_RECEIVE;

        for(int i = 0; i < mtype->argc; i++)
        {
          pony_trace(&msg->argv[i].p, mtype->type[i].trace, mtype->type[i].size,
            mtype->type[i].mode);
        }

        map_clearmarks(actor->foreign_ref);
        map_clearmarks(actor->local_ref);
      }

      actor->actor_type->dispatch(actor, actor->p, msg->id,
        mtype->argc, msg->argv);

      return true;
    }
  }
}

bool actor_run(pony_actor_t* actor, uint32_t thread,
  uint64_t* app_msgs, uint64_t* rc_msgs)
{
  this = actor;
  this->thread = thread;
  message_t* msg;

  while((msg = actorq_pop(&actor->q)) != NULL)
  {
    if(handle_message(actor, msg, app_msgs, rc_msgs)) { return true; }
  }

  if(actor->rc == 0)
  {
    actor_destroy(actor, NULL);
    return false;
  }

  if(heap_startgc(&actor->heap))
  {
    map_clearmarks(actor->foreign_ref);
    map_clearmarks(actor->local_ref);

    trace = TRACE_MARK;
    pony_trace(&actor->p, actor->actor_type->type.trace, 0, PONY_MUTABLE);

    map_gc(actor->local_ref);

    if(map_sweep(actor->foreign_ref))
      actor->refchanged = true;

    map_sendrc(&to_map);
    heap_endgc(&actor->heap);
  }

  if(!disable_cd && !actor->blocked)
  {
    // we are not blocked and have no messages
    if(actor->refchanged)
    {
      actor->refchanged = false;
      map_t* ref = map_clonerefs(actor->foreign_ref);

      if(ref != NULL)
      {
        // we reference other actors so can complete a cycle, inform the CD
        cycle_block(actor, actor->rc, ref);
        actor->blocked = true;
      }
    } else {
      if((actor->foreign_ref != NULL) && (map_size(actor->foreign_ref) > 0))
      {
        // our referenced actors haven't changed, but we have some
        cycle_block(actor, actor->rc, NULL);
        actor->blocked = true;
      }
    }
  }

  bool empty = actorq_markempty(&actor->q);
  this = NULL;

  return !empty;
}

void actor_destroy(pony_actor_t* actor, map_t* cycle)
{
  map_removecycle(actor->foreign_ref, cycle);
  map_clearmarks(actor->foreign_ref);
  map_sweep(actor->foreign_ref);

  while(actorq_pop(&actor->q) != NULL);
  actorq_destroy(&actor->q);
  heap_destroy(&actor->heap);

  map_free(actor->foreign_ref, true);
  map_free(actor->local_ref, false);

  POOL_FREE(pony_actor_t, actor);
}

void actor_sendv(pony_actor_t* to, uint64_t id, int argc, pony_arg_t* argv)
{
  push_message(to, id, argc, argv);
}

pony_actor_t* actor_create(pony_actor_type_t* type)
{
  assert(type != NULL);

  pony_actor_t* actor = POOL_ALLOC(pony_actor_t);
  actor->actor_type = type;
  actor->p = NULL;
  actor->rc = 256;
  actor->thread = 0;
  actor->blocked = false;
  actor->mark = false;
  actor->refchanged = false;

  actorq_init(&actor->q);
  heap_init(&actor->heap);

  actor->foreign_ref = NULL;
  actor->local_ref = NULL;

  return actor;
}

void actor_disablecd(bool state)
{
  disable_cd = state;
}

pony_actor_t* pony_create(pony_actor_type_t* type)
{
  pony_actor_t* actor = actor_create(type);

  if(this != NULL)
  {
    // actors begin unblocked and referenced by the creating actor
    map_createactor(&this->foreign_ref, actor);
    this->refchanged = true;
  } else {
    // the initial actor isn't referenced by anything
    actor->rc = 0;
  }

  return actor;
}

void pony_set(void* p)
{
  this->p = p;
}

void pony_send(pony_actor_t* to, uint64_t id)
{
  push_message(to, id, 0, NULL);
}

void pony_sendv(pony_actor_t* to, uint64_t id, int argc, pony_arg_t* argv)
{
  pony_msg_t* mtype = to->actor_type->msg(id);
  assert(argc <= PONY_MAX_ARG);
  assert(mtype->argc == argc);

  /* Tobias commented this out in waiting for Syvan's fixes
  if(argc > 0)
  {
    trace = TRACE_SEND;
    dest = to;

    for(int i = 0; i < argc; i++)
    {
      pony_trace(&argv[i].p, mtype->type[i].trace, mtype->type[i].size,
        mtype->type[i].mode);
    }

    map_sendrc(&to_map);
    map_clearmarks(this->foreign_ref);
    map_clearmarks(this->local_ref);
    this->mark = false;
  }
  */

  push_message(to, id, argc, argv);
}

void pony_sendp(pony_actor_t* to, uint64_t id, void* p)
{
  pony_arg_t arg = {.p = p};
  pony_sendv(to, id, 1, &arg);
}

void pony_sendi(pony_actor_t* to, uint64_t id, intptr_t i)
{
  pony_arg_t arg = {.i = i};
  push_message(to, id, 1, &arg);
}

void pony_sendd(pony_actor_t* to, uint64_t id, double d)
{
  pony_arg_t arg = {.d = d};
  push_message(to, id, 1, &arg);
}

void* pony_alloc(size_t size)
{
  return heap_alloc(this, &this->heap, size);
}

void pony_triggergc()
{
  this->heap.next_gc = 0;
}

void pony_trace(void* p, pony_trace_fn f, size_t size, pony_mode_t mode)
{
  /*
  Can p be an internal pointer?

  Arrays: pony arrays are arrays of primitives or pointers, not of contiguous
  objects. If a slice keeps a pointer to the original array, no need to trace
  internal pointers.

  Strings: strings are immutable and of counted length. A substring can be
  expressed as a pointer into the parent string and a length. But a pointer to
  the parent may as well be kept, since there is extra space on the cache line.

  Buffers: like strings. Again, extra space on the cache line.
  */
  switch(trace)
  {
    case TRACE_MARK: trace_mark(this, *(void**)p, f, mode); break;
    case TRACE_SEND: trace_send(this, dest, *(void**)p, f, mode); break;
    case TRACE_RECEIVE: trace_receive(this, *(void**)p, f, mode); break;
  }
}

void pony_tracearray(void* p, size_t asize, pony_mode_t amode,
  pony_trace_fn f, size_t size, pony_mode_t mode)
{
  // FIX: is this the right approach across a network?
  // should we separate trace and (de)serialise?
  pony_trace(p, NULL, asize * sizeof(void*), amode);

  void* o = *(void**)p;
  void* end = o + (asize * sizeof(void*));

  while(o < end)
  {
    pony_trace(o, f, size, mode);
    o += sizeof(void*);
  }
}

void pony_traceactor(pony_actor_t** p)
{
  pony_trace(p, NULL, sizeof(pony_actor_t*), PONY_ACTOR);
}

void pony_trace8(void* p)
{
  // FIX: only do something on serialise or deserialise
}

void pony_trace16(void* p)
{
  // FIX: only do something on serialise or deserialise
}

void pony_trace32(void* p)
{
  // FIX: only do something on serialise or deserialise
}

void pony_trace64(void* p)
{
  // FIX: only do something on serialise or deserialise
}
