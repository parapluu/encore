#ifndef task_h
#define task_h

#include <pony/pony.h>

typedef struct encore_task_s encore_task_s;

#include "encore.h"

typedef encore_arg_t (*task_fn)(void*, void*);

// initializes the task dependencies (mpmcq, etc)
void task_setup();

void init_actor_task();

// create task structure
encore_task_s* task_mk(task_fn body, void* env, void* dependencies, pony_trace_fn trace);

// put the task into a scheduler
void task_schedule(encore_task_s* t);

// attach future to task (optional)
void task_attach_fut(encore_task_s* t, void* fut);

// run task
encore_arg_t task_runner(encore_task_s* task);

// free task
void task_free(encore_task_s* task);

#endif
