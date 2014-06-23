#ifndef __tit_lazy_h__
#define __tit_lazy_h__

#include <stdbool.h>
#include "tit_common.h"

typedef struct lazy_tit_t lazy_tit_t;

// =============================================================================
// Setting up and tearing down a stack
// =============================================================================

/*
 * Sets up the library internal state. Should be called once per system.
 */
void init_system(int cache_size);

/*
 * Sets up context information for the current thread. Should be
 * called directly in each pthread after creation. 
 *
 * DANGER: Never call this unless you are positively certain that
 * you are running on a system-allocated stack. Below, we refer to
 * these as SAS-threads.
 */
lazy_tit_t *lazy_t_init_current();
lazy_tit_t *lazy_t_get_current();

/*
 * Returns a pointer to a new user-level thread that can be used
 * to call fork.
 */
lazy_tit_t *lazy_t_init_new();

// =============================================================================
// Calling a new function in its own stack
// =============================================================================

/*
 * Suspends the current execution and runs the supplied function with arguments.
 */
void lazy_t_fork_0(lazy_tit_t *new, void(*fun)());
void lazy_t_fork_1(lazy_tit_t *new, void(*fun)(void* a), void *a);
void lazy_t_fork_2(lazy_tit_t *new, void(*fun)(void* a, void *b), void *a, void *b);
void lazy_t_fork_3(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c), void *a, void *b, void *c);
void lazy_t_fork_4(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c, void *d), void *a, void *b, void *c, void *d);
void lazy_t_fork_5(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c, void *d, void* e), void *a, void *b, void *c, void *d, void *e);
void lazy_t_fork_6(lazy_tit_t *new, void(*fun)(void* a, void *b, void *c, void *d, void* e, void*f), void *a, void *b, void *c, void *d, void *e, void *f);

// =============================================================================
// Suspending, resuming and querying the state of a stack
// =============================================================================

/*
 * Resume execution of stack from the current thread. 
 */
void lazy_t_resume(lazy_tit_t *stack);

/*
 * Call this in a non SAS-thread to have the event loop picked up
 * by an avaiable SAS-thread. If there is no such thread
 * available, keep running. 
 * Returns false if no SAS-thread was available so the current thread
 * should keep going. 
 */
bool lazy_t_done(lazy_tit_t *stack);

/*
 * Returns true if stack is in a suspended state
 */
bool lazy_t_is_suspended(lazy_tit_t *stack);
bool lazy_t_is_proper(lazy_tit_t *stack);
lazy_tit_t *lazy_t_copy(lazy_tit_t *stack);

bool done_lazy(lazy_tit_t *stack);
void fork_lazy(void(*fun)());
void resume_lazy(lazy_tit_t *stack);

#endif
