#include "array.h"
#include "encore.h"
#include <stdio.h>
#include <assert.h>

struct array_t
{
  size_t size;
  pony_type_t *type;
  encore_arg_t elements[];
};

pony_type_t array_type =
  {
    .id = ID_ARRAY,
    .size = sizeof(struct array_t),
    .trace = array_trace,
  };

static int int_cmp(const void *a, const void *b) {
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

void array_trace(pony_ctx_t* ctx, void *p)
{
  assert(p);
  struct array_t *array = p;
  if (array->type == ENCORE_ACTIVE) {
    for(size_t i = 0; i < array->size; i++) {
      encore_trace_actor(ctx, array->elements[i].p);
    }
  } else if (array->type != ENCORE_PRIMITIVE) {
    for(size_t i = 0; i < array->size; i++) {
      encore_trace_object(ctx, array->elements[i].p, array->type->trace);
    }
  }
}

array_t *array_mk(pony_ctx_t **ctx, size_t size, pony_type_t *type)
{
  struct array_t *array = encore_alloc(*ctx,
      sizeof(struct array_t) + sizeof(encore_arg_t) * size);
  array->size = size;
  array->type = type;
  return array;
}

array_t *array_from_array(pony_ctx_t **ctx, size_t size, pony_type_t *type, encore_arg_t arr[])
{
  struct array_t *array = array_mk(ctx, size, type);
  for(size_t i = 0; i < size; i++) {
    array_set(array, i, arr[i]);
  }
  return array;
}

array_t *array_get_chunk(pony_ctx_t **ctx, size_t start,
                         size_t end, array_t* const a){
  assert(start < end);
  assert(end <= array_size(a));
  assert(array_size(a) > start);

  array_t* const new_array = array_mk(ctx, end-start, array_get_type(a));
  for(size_t index = start; index < end; ++index){
    array_set(new_array, index, array_get(a, index));
  }
  return new_array;
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
  assert(i < ((struct array_t *)a)->size);
  return ((struct array_t *)a)->elements[i];
}

pony_type_t* array_get_type(array_t *a){
  return ((struct array_t *)a)->type;
}

inline void array_set(array_t *a, size_t i, encore_arg_t element)
{
  ((struct array_t *)a)->elements[i] = element;
}
