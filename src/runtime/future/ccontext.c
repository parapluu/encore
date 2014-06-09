#include "ccontext.h"

#include <string.h>
#include <stdio.h>
#include <pthread.h>
#include <stdio.h>

#define STACK_SIZE (8 * 1024 * 1024)

void t_resume(stacklet_t *context) {
  setcontext(&context->context);
}

stacklet_t *t_init() {
  return malloc(sizeof(stacklet_t));
}

// TODO: need to rewrite strategy to get back into scheduler
// - When a thread A goes back into the scheduler, how does its new stack get deleted?
// - Ideally, A should be the one to resume an interruption and at this point release 
//   all resources for the temporary parallel stack
extern __thread void *t_hacky_scheduler;
extern void* run_thread(void* p);

static void __t_restart(void *this) {
  run_thread(t_hacky_scheduler);
}

void t_restart(stacklet_t *context, void *this) {
  getcontext(&context->old);
  // TODO: have a pool of stacks
  char *stack = malloc(STACK_SIZE+16);
  // Align stack on a 16 byte boundary to please OS X
  context->old.uc_stack.ss_sp    = stack + ((long) stack) % 16;
  context->old.uc_stack.ss_size  = STACK_SIZE;
  context->old.uc_stack.ss_flags = 0;        
  context->old.uc_link           = 0;
  
  makecontext(&context->old, __t_restart, 1, this);
  swapcontext(&context->context, &context->old);
}
