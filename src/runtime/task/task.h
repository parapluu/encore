#ifndef task_h
#define task_h

#include <pony/pony.h>

typedef struct encore_task_s encore_task_s;

#include "encore.h"


typedef encore_arg_t (*task_fn)(void*, void*);

extern pony_type_t* encore_task_type;
extern __thread pony_actor_t* this_encore_task;

// initializes the task dependencies (mpmcq, etc)
void task_setup(pony_type_t const* const type);

// create task structure
encore_task_s* task_mk(task_fn const body, void* const env, void* const dependencies, pony_trace_fn trace);

// put the task into a scheduler
void task_schedule(encore_task_s const* const t);

// attach future to task (optional)
void task_attach_fut(encore_task_s* const t, void* const fut);

// run task
encore_arg_t task_runner(encore_task_s const* const task);

void task_trace(void* const);

void* task_getenv(encore_task_s* const task);
void* task_getdependencies(encore_task_s* const task);

// free task
void task_free(encore_task_s* const task);

#endif
