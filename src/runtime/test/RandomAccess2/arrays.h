#ifndef ARRAY_ACTOR_H
#define ARRAY_ACTOR_H

typedef struct array_actor_t
{
  uint64_t count;
  pony_actor_t** array;
} array_actor_t;

void array_actor_trace(void* p);

typedef struct array_uint64_t
{
  uint64_t count;
  uint64_t* array;
} array_uint64_t;

void array_uint64_trace(void* p);

#endif
