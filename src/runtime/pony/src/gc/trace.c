#include "trace.h"
#include "gc.h"
#include "../actor/actor.h"

static __thread void (*trace_object)(pony_actor_t* current, heap_t* heap,
  gc_t* gc, void* p, pony_trace_fn f);

static __thread void (*trace_actor)(pony_actor_t* current, gc_t* gc,
  pony_actor_t* actor);

void trace_send()
{
  trace_object = gc_sendobject;
  trace_actor = gc_sendactor;
}

void trace_recv()
{
  trace_object = gc_recvobject;
  trace_actor = gc_recvactor;
}

void trace_mark()
{
  trace_object = gc_markobject;
  trace_actor = gc_markactor;
}

void pony_trace(void* p)
{
  if(p != NULL)
  {
    pony_actor_t* actor = actor_current();
    trace_object(actor, actor_heap(actor), actor_gc(actor), p, NULL);
  }
}

void pony_traceactor(pony_actor_t* p)
{
  if(p != NULL)
  {
    pony_actor_t* actor = actor_current();
    trace_actor(actor, actor_gc(actor), p);
  }
}

void pony_traceobject(void* p, pony_trace_fn f)
{
  if(p != NULL)
  {
    pony_actor_t* actor = actor_current();
    trace_object(actor, actor_heap(actor), actor_gc(actor), p, f);
  }
}
