#include "array.h"
#include "encore.h"

struct array_t
{
  size_t size;
  pony_type_t *type;
  encore_arg_t elements[];
};

pony_type_t array_type =
  {
    ID_ARRAY,
    sizeof(struct array_t),
    0,
    0,
    array_trace,
    NULL,
    NULL,
    NULL,
    NULL
  };

void array_trace(void *p)
{
  struct array_t *array = p;
  if (array->type == ENCORE_ACTIVE) {
    for(size_t i = 0; i < array->size; i++) {
      pony_traceactor(array->elements[i].p);
    }
  } else if (array->type != ENCORE_PRIMITIVE) {
    for(size_t i = 0; i < array->size; i++) {
      pony_traceobject(array->elements[i].p, array->type->trace);
    }
  }
}

array_t *array_mk(size_t size, pony_type_t *type)
{
  struct array_t *array = encore_alloc(
      sizeof(struct array_t) + sizeof(encore_arg_t) * size);
  array->size = size;
  array->type = type;
  return array;
}

array_t *array_from_array(size_t size, pony_type_t *type, encore_arg_t arr[])
{
  struct array_t *array = array_mk(size, type);
  for(int i = 0; i < size; i++) {
    array_set(array, i, arr[i]);
  }
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
