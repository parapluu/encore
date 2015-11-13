#include "tuple.h"
#include "encore.h"

struct tuple_t
{
  size_t size;
  pony_type_t **types;
  encore_arg_t elements[];
};

pony_type_t tuple_type =
  {
    .id = ID_TUPLE,
    .size=sizeof(struct tuple_t),
    .trace=tuple_trace
  };

void tuple_trace(void *p)
{
  struct tuple_t *tuple = p;
  for(size_t i = 0; i < tuple->size; i++) {
    if (tuple->types[i] == ENCORE_ACTIVE) {
      pony_traceactor(tuple->elements[i].p);
    } else if (tuple->types[i] != ENCORE_PRIMITIVE) {
      pony_traceobject(tuple->elements[i].p, tuple->types[i]->trace);
    }
  }
}

tuple_t *tuple_mk(size_t size)
{
  struct tuple_t *tuple = encore_alloc(
      sizeof(struct tuple_t) + sizeof(encore_arg_t) * size);
  tuple->size = size;
  tuple->types = encore_alloc(sizeof(pony_type_t*) * size);
  return tuple;
}

inline void tuple_set_type(tuple_t *t, size_t i, const pony_type_t *type)
{
  ((struct tuple_t *)t)->types[i] = type;
}


tuple_t *tuple_from_tuple(size_t size, const pony_type_t *types[], encore_arg_t elems[])
{
  struct tuple_t *tuple = tuple_mk(size);
  for(size_t i = 0; i < size; i++) {
    tuple_set(tuple, i, elems[i]);
    tuple_set_type(tuple, i, types[i]);
  }
  return tuple;
}

inline size_t tuple_size(tuple_t *t)
{
  return ((struct tuple_t *)t)->size;
}

inline encore_arg_t tuple_get(tuple_t *t, size_t i)
{
  return ((struct tuple_t *)t)->elements[i];
}

inline void tuple_set(tuple_t *t, size_t i, const encore_arg_t element)
{
  ((struct tuple_t *)t)->elements[i] = element;
}

