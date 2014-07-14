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

typedef struct array_uint64_t
{
  uint64_t count;
  uint64_t* array;
} array_uint64_t;

extern pony_type_t array_uint64_type;
void array_uint64_trace(void* p);

#endif
