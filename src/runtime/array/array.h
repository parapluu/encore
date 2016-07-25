#ifndef __array_h__
#define __array_h__

#include <pony.h>
#include <encore.h>

typedef void array_t;

extern pony_type_t array_type;

void array_trace(pony_ctx_t*, void *);

array_t *array_mk(pony_ctx_t* ctx, pony_type_t *type, size_t num_dims, ...);

array_t *array_from_array(pony_ctx_t* ctx, pony_type_t *type, encore_arg_t arr[], size_t num_dims, ...);

size_t array_size(array_t *a);

size_t array_dim_size(array_t *a, size_t dim);

encore_arg_t array_get(array_t *a, size_t i);

pony_type_t* array_get_type(array_t *a);

void array_set(array_t *a, size_t i, encore_arg_t element);

array_t* array_get_chunk(pony_ctx_t *ctx, size_t start, size_t end, array_t* a);

#endif
