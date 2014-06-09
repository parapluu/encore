#include "ccontext.h"

#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <stdio.h>

// Debug
void print_threadid() {
  fprintf(stderr, "{{{ current thread: %p }}}\n", pthread_self());
}

void t_resume(stacklet_t *context) {
  ucontext_t here; 
  context->context.uc_link = &here;
  swapcontext(&here, &context->context);
}

stacklet_t *t_make_stacklet() {
  return malloc(sizeof(stacklet_t));
}

void t_bail(stacklet_t *markPoint) {
  markPoint->resumed = true;
  setcontext(&markPoint->old);
}

void t_checkpoint(stacklet_t *markPoint, dispatch_t dispatch, void* this, void* p, long id, int argc, void* argv) {
  markPoint->resumed = false;
  getcontext(&markPoint->context);

  if (markPoint->resumed == false) {
    markPoint->context.uc_link = &markPoint->old;
    markPoint->context.uc_stack.ss_sp = malloc(1024*1024);
    markPoint->context.uc_stack.ss_size = 1024*1024;
    markPoint->context.uc_stack.ss_flags = 0;        

    makecontext(&markPoint->context, dispatch, 5, this, p, id, argc, argv);
    swapcontext(&markPoint->old, &markPoint->context);
  }
}
