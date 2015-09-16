#include <stdio.h>
#include <stdlib.h>
#include "range.h"
#include "encore.h"

struct range_t
{
  size_t start;
  size_t stop;
  size_t step;
};

pony_type_t range_type =
  {
    ID_RANGE,
    sizeof(struct range_t),
    0,
    0,
    range_trace,
    NULL,
    NULL,
    NULL,
    NULL
  };

void range_trace(void *p)
{
  // No pointers
}

struct range_t *range_mk(size_t start, size_t stop, size_t step)
{
  range_assert_step(step);
  struct range_t *r = encore_alloc(sizeof(struct range_t));
  *r = (range_t) { .start = start, .stop = stop, .step = step };
  return r;
}

void range_assert_step(size_t step)
{
  if(step == 0)
    {
      puts("*** Runtime error: step length cannot be 0 ***");
      exit(1);
    }
}

size_t range_start (struct range_t *a) { return a->start; }
size_t range_stop  (struct range_t *a) { return a->stop;  }
size_t range_step  (struct range_t *a) { return a->step;  }
