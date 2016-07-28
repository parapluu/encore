#ifndef __tuple_h__
#define __tuple_h__

#include <pony.h>
#include <encore.h>

typedef void tuple_t;

extern pony_type_t tuple_type;

void tuple_trace(pony_ctx_t*, void *);

tuple_t *tuple_mk(pony_ctx_t** ctx, size_t size);

void tuple_set_type(tuple_t *t, size_t i, pony_type_t *type);

tuple_t *tuple_from_tuple(pony_ctx_t** ctx, size_t size, pony_type_t **type, encore_arg_t arr[]);

size_t tuple_size(tuple_t *t);

encore_arg_t tuple_get(tuple_t *t, size_t i);

void tuple_set(tuple_t *t, size_t i, encore_arg_t element);

#endif
