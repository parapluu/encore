#ifndef __ccontext_h__
#define __ccontext_h__

#define _XOPEN_SOURCE

#include <stdbool.h>
#include <stdlib.h>
#include <ucontext.h>

typedef struct stacklet_t stacklet_t;
struct stacklet_t {
  volatile bool resumed;
  ucontext_t context;
  ucontext_t old;
};
typedef void(*dispatch_t)(void* this, void* p, long id, int argc, void* argv);

// TODO: Free resources on resume
#define t_save(stacklet) \
  stacklet->resumed = false; \
  getcontext(&stacklet->context); \

void print_threadid();
void t_resume(stacklet_t *stacklet);
void t_checkpoint(stacklet_t *stacklet, dispatch_t dispatch, void* this, void* p, long id, int argc, void* argv);
stacklet_t *t_make_stacklet();
void t_bail(stacklet_t *stacklet);

#endif
