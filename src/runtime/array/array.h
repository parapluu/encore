#ifndef __array_h__
#define __array_h__

#include <pony/pony.h>
#include <encore.h>

typedef void array_t;

extern pony_type_t array_type;

void array_trace(void *);

array_t *array_mk(size_t size, pony_type_t *type);

size_t array_size(array_t *a);

encore_arg_t array_get(array_t *a, size_t i);

void array_set(array_t *a, size_t i, encore_arg_t element);

#endif