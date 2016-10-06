#include "tuple.h"
#include "encore.h"
#include <assert.h>

struct tuple
{
  size_t size;
  encore_arg_t *elements;
  pony_type_t **types;
};

pony_type_t tuple_type =
  {
    .id = ID_TUPLE,
    .size=sizeof(struct tuple),
    .trace=tuple_trace
  };

void tuple_trace(pony_ctx_t* ctx, void *p)
{
  struct tuple *tuple = p;

  pony_trace(ctx, tuple->types);

  for(size_t i = 0; i < tuple->size; ++i)
    {
      if (tuple->types[i] == ENCORE_ACTIVE)
        {
          pony_traceactor(ctx, tuple->elements[i].p);
          continue;
        }
      if (tuple->types[i] != ENCORE_PRIMITIVE)
        {
          pony_traceobject(ctx, tuple->elements[i].p, tuple->types[i]->trace);
        }
    }
}

tuple_t *tuple_mk(pony_ctx_t** ctx, size_t size)
{
  size_t tuple_size    = sizeof(struct tuple);
  size_t elements_size = size * sizeof(encore_arg_t);
  size_t types_size    = size * sizeof(pony_type_t *);
  
  struct tuple *tuple = encore_alloc(*ctx, tuple_size + elements_size + types_size);

  tuple->size     = size;
  tuple->elements = ((void *)tuple) + tuple_size;
  tuple->types    = ((void *)tuple) + tuple_size + elements_size;

  return tuple;
}

inline void tuple_set_type(tuple_t *t, size_t i, const pony_type_t *type)
{
  t->types[i] = type;
}


tuple_t *tuple_from_tuple(pony_ctx_t** ctx, size_t size, const pony_type_t *types[], encore_arg_t elems[])
{
  struct tuple *tuple = tuple_mk(ctx, size);
  
  for(size_t i = 0; i < size; ++i)
    {
      tuple_set(tuple, i, elems[i]);
      tuple_set_type(tuple, i, types[i]);
    }
  
  return tuple;
}

inline size_t tuple_size(tuple_t *t)
{
  return t->size;
}

inline encore_arg_t tuple_get(tuple_t *t, size_t i)
{
  return t->elements[i];
}

inline void tuple_set(tuple_t *t, size_t i, const encore_arg_t element)
{
  t->elements[i] = element;
}
