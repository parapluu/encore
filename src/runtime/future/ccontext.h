#ifndef __ccontext_h__
#define __ccontext_h__

#define _XOPEN_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <ucontext.h>

typedef struct stacklet_t stacklet_t;

// TODO: make this opaque
struct stacklet_t {
  volatile bool resumed;
  ucontext_t context;
  ucontext_t old;
  void *t_hacky_q; 
};

void t_resume(stacklet_t *context);
stacklet_t *t_init();
void t_restart(stacklet_t *context, void* actor);
void print_threadid();

#endif
