#include "ccontext.h"

#include <stddef.h>
#include <stdint.h>
#include "actor_def.h"

#define STACK_SIZE (8 * 1024 * 1024)

void t_resume(stacklet_t *s) {
  longjmp(s->env, 1);
}

stacklet_t *t_init() {
  return calloc(1, sizeof(stacklet_t));
}

extern void run_thread_in_same_scheduler_as_actor(void* p);

void t_restart(stacklet_t *s, pony_actor_t *this) {
  // TODO: quite possibly we can move everything but env from stacklet into stack vars here
  getcontext(&s->old);
  // TODO: have a pool of stacks
  // TODO: if current stack height is very small, use existing stack rather than calloc
  char *stack = calloc(1, STACK_SIZE+16);
  // Align stack on a 16 byte boundary to please OS X
  s->old.uc_stack.ss_sp    = stack + ((long) stack) % 16;
  s->old.uc_stack.ss_size  = STACK_SIZE;
  s->old.uc_stack.ss_flags = 0;        
  s->old.uc_link           = &s->old;

  this->blocking_on_a_future = true;

  if (setjmp(s->env) == false) {
    makecontext(&s->old, run_thread_in_same_scheduler_as_actor, 1, this);
    swapcontext(&s->context, &s->old);
  }
    
  free(s->old.uc_stack.ss_sp);
}
