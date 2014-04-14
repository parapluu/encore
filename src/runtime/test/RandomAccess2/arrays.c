#include <pony/pony.h>
#include "arrays.h"
#include <stdint.h>

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

void array_uint64_trace(void* p)
{
  array_uint64_t* this = p;

  // pony_trace64(&this->count);
  pony_trace(&this->array, NULL, this->count * sizeof(uint64_t), PONY_MUTABLE);

  // for(uint64_t i = 0; i < this->count; i++)
  // {
  //   pony_trace64(&this->array[i]);
  // }
}
