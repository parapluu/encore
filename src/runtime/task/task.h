#ifndef task_h
#define task_h

#include <pony.h>

typedef struct encore_task_s encore_task_s;

#include "encore.h"
typedef encore_arg_t (*task_fn)(pony_ctx_t**, void*, void*);

extern __thread encore_actor_t* this_encore_task;

// ==================================================================
// setup runtime task type
// ==================================================================
void task_setup(pony_type_t const* const type);
pony_type_t* task_gettype();

// ==================================================================
// create, attach future, schedule and run task
// ==================================================================
encore_task_s* task_mk(pony_ctx_t* ctx, task_fn const body, void* const env,
                       void* const dependencies, pony_trace_fn trace);
void task_schedule(encore_task_s const* const t);
void task_attach_fut(encore_task_s* const t, void* const fut);  // optional
encore_arg_t task_runner(encore_task_s const* const task); // run task

// ==================================================================
// clean up
// ==================================================================
void task_trace(pony_ctx_t *ctx, void* const);
void task_free(encore_task_s* const task);

#endif
