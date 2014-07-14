#include "arrays.h"

pony_type_t array_actor_type =
{
  sizeof(array_actor_t),
  array_actor_trace,
  NULL,
  NULL
};

void array_actor_trace(void* p)
{
  array_actor_t* this = p;
  pony_trace(this->array);

  for(uint64_t i = 0; i < this->count; i++)
  {
    pony_traceactor(this->array[i]);
  }
}
