#include "header.h"

static void Bar_trace(void* p)
{
  pony_traceactor((*((Bar_data*) p)).f);
}


void* Bar_bar(Bar_data* this, pony_actor_t* f)
{
  ;
  (*this).f = f;;;
  return UNIT;;
}
