#include "ccontext.h"

#include <stddef.h>
#include <stdint.h>
#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <stdio.h>
#include <setjmp.h>

#define STACK_SIZE (8 * 1024 * 1024)

jmp_buf env;

void t_resume(stacklet_t *context) {
  //setcontext(&context->context);
  longjmp(env,1);
}

stacklet_t *t_init() {
  return malloc(sizeof(stacklet_t));
}

// TODO: need to rewrite strategy to get back into scheduler
// - When a thread A goes back into the scheduler, how does its new stack get deleted?
// - Ideally, A should be the one to resume an interruption and at this point release 
//   all resources for the temporary parallel stack
extern void run_thread_in_same_scheduler_as_actor(void* p);

typedef struct hack_for_now
{
  void* p;
  uint32_t rc;
  uint32_t thread;
  void *resume;
} hack_for_now; 

void t_restart(stacklet_t *context, void *this) {
  getcontext(&context->old);
  // TODO: have a pool of stacks
  // TODO: if current stack height is very small, use existing stack rather than calloc
  char *stack = calloc(1, STACK_SIZE+16);
  // Align stack on a 16 byte boundary to please OS X
  context->old.uc_stack.ss_sp    = stack + ((long) stack) % 16;
  context->old.uc_stack.ss_size  = STACK_SIZE;
  context->old.uc_stack.ss_flags = 0;        
  context->old.uc_link           = &context->old;

  ((hack_for_now*)this)->resume = true;

  if (setjmp(env) == false) {
    makecontext(&context->old, run_thread_in_same_scheduler_as_actor, 1, this);
    swapcontext(&context->context, &context->old);
  }
    
  free(context->old.uc_stack.ss_sp);
}
