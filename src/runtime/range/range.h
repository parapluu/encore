#ifndef __range_h__
#define __range_h__

#include <pony.h>
#include <encore.h>

typedef struct range_t range_t;

extern pony_type_t range_type;

void range_trace(pony_ctx_t*, void *);

range_t *range_mk(pony_ctx_t **ctx, int64_t start, int64_t stop, int64_t step);

void range_assert_step(int64_t step);

int64_t range_start (range_t *a);
int64_t range_stop  (range_t *a);
int64_t range_step  (range_t *a);

extern pony_type_t range_type;
#endif
