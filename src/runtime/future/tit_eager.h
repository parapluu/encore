#ifndef __tit_eager_h__
#define __tit_eager_h__

#include <stdbool.h>
#include "tit_common.h"

typedef struct tit_t tit_t;

// =============================================================================
// Setting up and tearing down a stack
// =============================================================================

tit_t *t_init();
void t_done(tit_t *s); 

// =============================================================================
// Calling a new function in its own stack
// =============================================================================

void t_fork_0(tit_t *s, fun_t_0 fun);
void t_fork_1(tit_t *s, fun_t_1 fun, void *a);
void t_fork_2(tit_t *s, fun_t_2 fun, void *a, void *b);
void t_fork_3(tit_t *s, fun_t_3 fun, void *a, void *b, void *c);
void t_fork_4(tit_t *s, fun_t_4 fun, void *a, void *b, void *c, void *d);
void t_fork_5(tit_t *s, fun_t_5 fun, void *a, void *b, void *c, void *d, void *e);
void t_fork_6(tit_t *s, fun_t_6 fun, void *a, void *b, void *c, void *d, void *e, void *f);

// =============================================================================
// Suspending, resuming and querying the state of a stack
// =============================================================================

void t_suspend(tit_t *s);
bool t_is_suspended(tit_t *s);
void t_resume(tit_t *s);

#endif
