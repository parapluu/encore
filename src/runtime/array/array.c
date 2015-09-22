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

/* int int_cmp(const void *a, const void *b) {  */
/*   const intptr_t *ia = (const intptr_t*) __atomic_load_n(&a, __ATOMIC_SEQ_CST);  */
/*   const intptr_t *ib = (const intptr_t*) __atomic_load_n(&b, __ATOMIC_SEQ_CST);  */
/*   return *ia - *ib; */
/* } */

int int_cmp(const void *a, const void *b) { 
  const intptr_t *ia = (const intptr_t*) a;
  const intptr_t *ib = (const intptr_t*) b;
  return *ia - *ib;
}

/// Only works on arrays of integers, only callable through embed at the present
void array_qsort(array_t *a, int64_t start, int64_t end) 
{
  struct array_t *p = a;
  qsort(p->elements + start, end, sizeof(encore_arg_t*), int_cmp); 
}

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

/* inline encore_arg_t array_get(array_t *a, size_t i) */
/* { */
/*   encore_arg_t result; */
/*   __atomic_load(&((struct array_t *)a)->elements[i], &result, __ATOMIC_SEQ_CST); */
/*   return result; */
/* } */

/* inline void array_set(array_t *a, size_t i, encore_arg_t element) */
/* { */
/*   __atomic_store(&((struct array_t *)a)->elements[i], &element, __ATOMIC_SEQ_CST); */
/* } */

inline encore_arg_t array_get(array_t *a, size_t i)
{
  return ((struct array_t *)a)->elements[i];
}

inline void array_set(array_t *a, size_t i, encore_arg_t element)
{
  ((struct array_t *)a)->elements[i] = element;
}

