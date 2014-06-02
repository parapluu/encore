#include "arrays.h"

void array_actor_trace(void* p)
{
  array_actor_t* this = p;

  // pony_trace64(&this->count);
  pony_trace(&this->array, NULL, this->count * sizeof(pony_actor_t*), PONY_MUTABLE);

  for(uint64_t i = 0; i < this->count; i++)
  {
    pony_traceactor(&this->array[i]);
  }
}
