#include "array.h"
#include "encore.h"
#include <assert.h>
#include <stdarg.h>

struct array_t
{
  size_t total_size;
  size_t num_dims;
  size_t *dim_sizes;
  pony_type_t *type;
  encore_arg_t elements[];
};

pony_type_t array_type =
  {
    .id = ID_ARRAY,
    .size = sizeof(struct array_t),
    .trace = array_trace,
  };

void array_trace(pony_ctx_t* ctx, void *p)
{
  assert(p);
  struct array_t *array = p;
  pony_trace(ctx, array->dim_sizes);
  if (array->type == ENCORE_ACTIVE) {
    for(size_t i = 0; i < array_size(array); i++) {
      pony_traceactor(ctx, array->elements[i].p);
    }
  } else if (array->type != ENCORE_PRIMITIVE) {
    for(size_t i = 0; i < array_size(array); i++) {
      pony_traceobject(ctx, array->elements[i].p, array->type->trace);
    }
  }
}

struct array_t *array_mk_helper(pony_ctx_t* ctx, pony_type_t *type, size_t num_dims, va_list dims)
{
  ctx = pony_ctx();

  size_t *dim_sizes = encore_alloc(ctx, sizeof(size_t) * num_dims);
  size_t total_size = 0;

  for(size_t i = 0; i < num_dims; ++i) {
    size_t this_size = va_arg(dims, size_t);
    dim_sizes[i] = this_size;
    total_size += this_size;
   }

  struct array_t *array = encore_alloc(ctx,
         sizeof(struct array_t) + sizeof(encore_arg_t) * total_size);
  array->total_size = total_size;
  array->num_dims = num_dims;
  array->dim_sizes = dim_sizes;
  array->type = type;
  
  return array;
}

array_t *array_mk(pony_ctx_t* ctx, pony_type_t *type, size_t num_dims, ...)
{
  va_list dims;
  va_start(dims, num_dims);
  struct array_t *array = array_mk_helper(ctx, type, num_dims, dims);
  va_end(dims);
  
  return array;
}

array_t *array_from_array(pony_ctx_t* ctx, pony_type_t *type, encore_arg_t arr[], size_t num_dims, ...)
{
  va_list dims;
  va_start(dims, num_dims);
  struct array_t *array = array_mk_helper(ctx, type, num_dims, dims);
  va_end(dims);
  
  for(size_t i = 0; i < array_size(array); i++) {
    array_set(array, i, arr[i]);
  }
  return array;
}

// Only works for 1-dimensional arrays
array_t *array_get_chunk(pony_ctx_t *ctx, size_t start, size_t end, array_t* a){
  assert(start <= end);

  struct array_t* arr = a;

  // total size of the original array
  size_t total_size = array_size(arr);

  // size of the new chunk
  size_t local_size = end - start + 1;

  if((start < total_size) && ((end - 1) <= total_size)){
    array_t* new_array = array_mk(ctx, arr->type, 1, local_size);
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
  return ((struct array_t*)a)->total_size;
}

inline size_t array_dim_size(array_t *a, size_t dim)
{
  return ((struct array_t*)a)->dim_sizes[dim];
}

inline encore_arg_t array_get(array_t *a, size_t i)
{
  return ((struct array_t *)a)->elements[i];
}

pony_type_t* array_get_type(array_t *a){
  return ((struct array_t *)a)->type;
}

inline void array_set(array_t *a, size_t i, encore_arg_t element)
{
  ((struct array_t *)a)->elements[i] = element;
}
