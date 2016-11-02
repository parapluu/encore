#include "array.h"
#include "encore.h"
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


array_t *array_get_chunk(pony_ctx_t **ctx, size_t start, size_t end, array_t* a){
  assert(start <= end);

  struct array_t* arr = a;

  // total size of the original array
  size_t total_size = array_size(arr);

  // size of the new chunk
  size_t local_size = end - start + 1;

  if((start < total_size) && ((end - 1) <= total_size)){
    array_t* new_array = array_mk(ctx, local_size, arr->type);
    size_t pivot = total_size - start;
    value_t* elements = arr->elements;

    for(; start<=end; start++){
      size_t index = pivot + start - total_size;
      array_set(new_array, index, elements[start]);
    }
    return new_array;
  }
  exit(-1);
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
