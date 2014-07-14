#ifndef ARRAY_ACTOR_H
#define ARRAY_ACTOR_H

#include <pony/pony.h>
#include <stdint.h>

typedef struct array_actor_t
{
  uint64_t count;
  pony_actor_t** array;
} array_actor_t;

extern pony_type_t array_actor_type;
void array_actor_trace(void* p);

#endif
