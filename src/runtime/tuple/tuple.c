#include "tuple.h"
#include "encore.h"
#include <assert.h>

struct tuple
{
  size_t arity;
  encore_arg_t *elements;
  pony_type_t **types;
};

pony_type_t tuple_type =
  {
    .id = ID_TUPLE,
    .size=sizeof(tuple_t),
    .trace=tuple_trace
  };

void tuple_trace(pony_ctx_t *ctx, void *p)
{
  tuple_t *tuple = p;

  for(size_t i = 0; i < tuple->arity; ++i)
    {
      if (tuple->types[i] == ENCORE_ACTIVE)
        {
          encore_trace_actor(ctx, tuple->elements[i].p);
        }
      else
      if (tuple->types[i] != ENCORE_PRIMITIVE)
        {
          encore_trace_object(ctx, tuple->elements[i].p, tuple->types[i]->trace);
        }
    }
}

tuple_t *tuple_mk(pony_ctx_t **ctx, size_t arity)
{
  size_t tuple_size    = sizeof(tuple_t);
  size_t elements_size = arity * sizeof(encore_arg_t);
  size_t types_size    = arity * sizeof(pony_type_t *);

  tuple_t *tuple = encore_alloc(*ctx, tuple_size + elements_size + types_size);

  *tuple = (tuple_t) { .arity    = arity,
                       .elements = ((void *)tuple) + tuple_size,
                       .types    = ((void *)tuple) + tuple_size + elements_size };

  return tuple;
}

inline void tuple_set_type(tuple_t *t, size_t i, const pony_type_t *type)
{
  t->types[i] = type;
}


tuple_t *tuple_from_tuple(pony_ctx_t **ctx, size_t arity, const pony_type_t *types[], encore_arg_t elems[])
{
  tuple_t *tuple = tuple_mk(ctx, arity);

  for(size_t i = 0; i < arity; ++i)
    {
      tuple_set(tuple, i, elems[i]);
      tuple_set_type(tuple, i, types[i]);
    }

  return tuple;
}

inline size_t tuple_size(tuple_t *t)
{
  return t->arity;
}

inline encore_arg_t tuple_get(tuple_t *t, size_t i)
{
  return t->elements[i];
}

inline void tuple_set(tuple_t *t, size_t i, const encore_arg_t element)
{
  t->elements[i] = element;
}
