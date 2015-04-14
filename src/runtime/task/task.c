#include "task.h"
#include "mpmcq.h"
#include <pony/pony.h>
#include <assert.h>
#include <stdio.h>
#include "encore.h"

struct encore_task_s {
  task_fn run;
  void* env;
  void* dependencies;
  void* fut;
  pony_trace_fn trace;
};

typedef encore_arg_t value_t;

// TODO: Best practice: Put it into the header. => creates more dependencies...
// handle_task(): called from scheduler.c, implemented in the actor because it needs
//                to call handle_message (actor.c, to follow with the guidelines of
//                processing a message)
// return whether it did handled a task. If no tasks are handled, it means it can
//        stop the program
extern bool handle_task();


// used in actor.c to create an encore actor if there's not one
extern pony_type_t* encore_task_type; // global that 


// Global queue where actors can pick up tasks as messages
extern mpmcq_t taskq;
extern __thread pony_actor_t* this_encore_task;


/*
 * Implementation
 */


static void set_encore_task_type(pony_type_t const* const type){
    encore_task_type = type;
}

void task_setup(pony_type_t* type){
  static int n_calls = 0;
  assert(n_calls++ == 0);

  mpmcq_init(&taskq);
  set_encore_task_type(type);
}


encore_task_s* task_mk(task_fn const body, void* const env, void* const dependencies, pony_trace_fn trace){
  encore_task_s* task = malloc(sizeof(encore_task_s));

  *task = (encore_task_s){.run = body, .env = env, .dependencies = dependencies, .trace = trace};
  return task;
}


value_t task_runner(encore_task_s const* const task){
  return task->run(task->env, task->dependencies);
}


void task_free(encore_task_s* const task){
  free(task);
}


void task_attach_fut(encore_task_s* const task, void* const fut){
  task->fut = fut;
}


inline static encore_task_msg_s* const task_mk_msg(encore_task_s* const task){
  encore_task_msg_s* const msg = (encore_task_msg_s* const) pony_alloc_msg(0, _ENC__MSG_TASK);
  msg->_fut = task->fut;
  msg->_task = task;
  return msg;
}

inline static void encore_send_task(encore_task_msg_s* const msg){
  // send message to global queue of actors
  mpmcq_push(&taskq, msg);
}

void task_schedule(encore_task_s const* const task){
  encore_task_msg_s* const msg = task_mk_msg((encore_task_s* const) task);
  encore_send_task(msg);
}
