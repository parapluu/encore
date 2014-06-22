#ifndef scheduler_h
#define scheduler_h

#include "actor.h"

typedef struct scheduler_t scheduler_t;

void scheduler_add(pony_actor_t* actor, uint32_t thread);

void scheduler_terminate();

#endif
