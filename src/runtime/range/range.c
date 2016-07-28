#include <stdio.h>
#include <stdlib.h>
#include "range.h"
#include "encore.h"

struct range_t
{
  int64_t start;
  int64_t stop;
  int64_t step;
};

pony_type_t range_type =
  {
    .id = ID_RANGE,
    .size = sizeof(struct range_t),
    .trace = range_trace,
  };

void range_trace(pony_ctx_t* ctx, __attribute__((unused)) void *p)
{
  (void)ctx;
  // No pointers
}

struct range_t *range_mk(pony_ctx_t **ctx, int64_t start,
                         int64_t stop, int64_t step)
{
  range_assert_step(step);
  struct range_t *r = encore_alloc(*ctx, sizeof(struct range_t));
  *r = (range_t) { .start = start, .stop = stop, .step = step };
  return r;
}

void range_assert_step(int64_t step)
{
  if(step == 0)
    {
      puts("*** Runtime error: step length cannot be 0 ***");
      exit(1);
    }
}

int64_t  range_start (struct range_t *a) { return a->start; }
int64_t  range_stop  (struct range_t *a) { return a->stop;  }
int64_t range_step  (struct range_t *a) { return a->step;  }
