#ifndef __ccontext_h__
#define __ccontext_h__

#define _XOPEN_SOURCE

#include <setjmp.h>
#include <stdbool.h>
#include <stdlib.h>
#include <ucontext.h>
#include <pony/pony.h>

typedef struct stacklet_t stacklet_t;

// TODO: make this opaque
struct stacklet_t {
  ucontext_t context;
  ucontext_t old;
  jmp_buf env;
};

stacklet_t *t_init();
void t_resume(stacklet_t *s);
void t_restart(stacklet_t *s, pony_actor_t* a);

#endif
