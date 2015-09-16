#ifndef __range_h__
#define __range_h__

#include <pony.h>
#include <encore.h>

typedef struct range_t range_t;

void range_trace(void *);

range_t *range_mk(size_t start, size_t stop, size_t step);

void range_assert_step(size_t step);

size_t range_start (range_t *a);
size_t range_stop  (range_t *a);
size_t range_step  (range_t *a);

pony_type_t range_type;
#endif
