#ifndef __array_h__
#define __array_h__

#include <pony.h>
#include <encore.h>

typedef void array_t;

extern pony_type_t array_type;

void array_trace(void *);

array_t *array_mk(size_t size, pony_type_t *type);

array_t *array_from_array(size_t size, pony_type_t *type, encore_arg_t arr[]);

size_t array_size(array_t *a);

encore_arg_t array_get(array_t *a, size_t i);

void array_set(array_t *a, size_t i, encore_arg_t element);

void array_qsort(array_t *a, int64_t start, int64_t end);

#endif
