#ifndef __tuple_h__
#define __tuple_h__

#include <pony.h>
#include <encore.h>

typedef struct tuple tuple_t;

extern pony_type_t tuple_type;

void tuple_trace(pony_ctx_t *ctx, void *tuple);

tuple_t *tuple_mk(pony_ctx_t **ctx, size_t arity);

void tuple_set_type(tuple_t *t, size_t arity, pony_type_t *types);

tuple_t *tuple_from_tuple(pony_ctx_t **ctx, size_t arity, pony_type_t **types, encore_arg_t elements[]);

size_t tuple_size(tuple_t *t);

encore_arg_t tuple_get(tuple_t *t, size_t i);

void tuple_set(tuple_t *t, size_t i, encore_arg_t element);

#endif
