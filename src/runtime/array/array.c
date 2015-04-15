#include "array.h"
#include "encore.h"

struct array_t
{
  size_t size;
  pony_type_t *type;
  encore_arg_t *elements;
};

pony_type_t array_type = 
  {
    ID_ARRAY, 
    sizeof(struct array_t), 
    array_trace, 
    NULL, 
    NULL,
    NULL,
    NULL
  };

void array_trace(void *p) 
{
  struct array_t *array = p;
  // pony_trace(array->elements);
  if(array->type == ENCORE_ACTIVE)
    {
      for(int i = 0; i < array->size; i++)
	{
	  pony_traceactor(array->elements[i].p);
	}
    } 
  else if(array->type != ENCORE_PRIMITIVE) 
    {
      for(int i = 0; i < array->size; i++)
	{
	  pony_traceobject(array->elements[i].p, array->type->trace);
	}
    }
}

array_t *array_mk(size_t size, pony_type_t *type)
{
  struct array_t *array = encore_alloc(sizeof(struct array_t) + sizeof(encore_arg_t) * size);
  array->size = size;
  array->type = type;
  array->elements = ((void *)array) + sizeof(struct array_t);
  return array;
}

inline size_t array_size(array_t *a)
{
  return ((struct array_t *)a)->size;
}

inline encore_arg_t array_get(array_t *a, size_t i)
{
  return ((struct array_t *)a)->elements[i];
}

inline void array_set(array_t *a, size_t i, encore_arg_t element)
{
  ((struct array_t *)a)->elements[i] = element;
}
