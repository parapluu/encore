#ifndef __tit_eager_h__
#define __tit_eager_h__

#include <stdbool.h>
#include "tit_common.h"
#include <pony/pony.h>

typedef struct eager_tit_t eager_tit_t;

// =============================================================================
// Setting up and tearing down a stack
// =============================================================================

void eager_t_init_current();

// =============================================================================
// Calling a new function in its own stack
// =============================================================================

void fork_eager(void(*fun_t_5)(), pony_actor_t *a, void *b, void *c, void *d, void *e);

// =============================================================================
// Suspending, resuming and querying the state of a stack
// =============================================================================

void suspend_eager(eager_tit_t *t);
void resume_eager(eager_tit_t *t);
eager_tit_t *get_suspendable_tit();


#endif
