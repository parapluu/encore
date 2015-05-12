#include "task.h"
#include "sched/mpmcq.h"
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


pony_type_t* encore_task_type;
extern mpmcq_t taskq;
mpmcq_t taskq;
uint32_t remaining_tasks;


/*
 * Implementation
 */


static void set_encore_task_type(pony_type_t const* const type){
    encore_task_type = type;
}

void task_setup(pony_type_t* type){
  static int n_calls = 0;
  assert(n_calls++ == 0);

  __pony_atomic_store_n(&remaining_tasks, 0, PONY_ATOMIC_RELAXED, uint32_t);
  mpmcq_init(&taskq);
  set_encore_task_type(type);
}


encore_task_s* task_mk(task_fn const body, void* const env, void* const dependencies, pony_trace_fn trace){
  encore_task_s* task = malloc(sizeof(encore_task_s));
  __pony_atomic_fetch_add(&remaining_tasks, 1, PONY_ATOMIC_RELAXED, uint32_t);
  *task = (encore_task_s){.run = body, .env = env, .dependencies = dependencies, .trace = trace};
  return task;
}

void task_trace(void* const p){
  struct encore_task_s* t = (struct encore_task_s*)p;
  if(t->trace != NULL){
    t->trace(t->env);
  }
  // future is traced via generated code
  // dependencies are null at the moment
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


void* task_getenv(encore_task_s* const task){
  return task->env;
}


void* task_getdependencies(encore_task_s* const task){
  return task->dependencies;
}


inline static encore_task_msg_s* const task_mk_msg(encore_task_s* const task){
  encore_task_msg_s* const msg = (encore_task_msg_s* const) pony_alloc_msg(0, _ENC__MSG_TASK);
  msg->_fut = task->fut;
  msg->_task = task;
  return msg;
}

inline static void encore_send_task(encore_task_msg_s* const msg){
  // send message to global queue of actors
  /* puts("send message to taskq"); */
  mpmcq_push(&taskq, msg);
}

void task_schedule(encore_task_s const* const task){
  encore_task_msg_s* const msg = task_mk_msg((encore_task_s* const) task);
  encore_send_task(msg);
}
