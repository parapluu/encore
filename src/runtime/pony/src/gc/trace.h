#ifndef gc_trace_h
#define gc_trace_h

#include <pony/pony.h>

void trace_send();

void trace_recv();

void trace_mark();

void pony_trace(void* p);

void pony_traceactor(pony_actor_t* p);

void pony_traceobject(void* p, pony_trace_fn f);

#endif
