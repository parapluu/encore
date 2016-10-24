#include "task.h"
#include "sched/mpmcq.h"
#include "sched/scheduler.h"
#include <pony.h>
#include <stdlib.h>
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
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
mpmcq_t taskq;
uint32_t remaining_tasks;

pony_type_t* task_gettype(){
  assert(encore_task_type!=NULL);
  return encore_task_type;
}


static void set_encore_task_type(pony_type_t const* const type){
  encore_task_type = type;
}

void task_setup(pony_type_t const* const type){
  static int n_calls = 0;
  (void) n_calls;
  assert(n_calls++ == 0);

  __atomic_store_n(&remaining_tasks, 0, __ATOMIC_RELAXED);
  ponyint_mpmcq_init(&taskq);
  set_encore_task_type(type);
}


encore_task_s* task_mk(pony_ctx_t* ctx, task_fn const body,
                       void* const env, void* const dependencies,
                       pony_trace_fn trace){
  (void)ctx;
  encore_task_s* task = malloc(sizeof(encore_task_s));
  __atomic_fetch_add(&remaining_tasks, 1, __ATOMIC_RELAXED);
  *task = (encore_task_s){.run = body, .env = env, .dependencies = dependencies, .trace = trace};
  return task;
}

void task_trace(pony_ctx_t *ctx, void* const p)
{
  assert(p);
  struct encore_task_s* t = (struct encore_task_s*)p;
  if(t->trace != NULL){
    t->trace(ctx, t->env);
  }
  // future is traced via generated code
  // dependencies are null at the moment
}

value_t task_runner(encore_task_s const* const task){
  pony_ctx_t* ctx = pony_ctx(); // TODO: Pass ctx as argument instead!
  return task->run(&ctx, task->env, task->dependencies);
}


void task_free(encore_task_s* const task){
  free(task);
}


void task_attach_fut(encore_task_s* const task, void* const fut){
  task->fut = fut;
}


inline static encore_task_msg_s* task_mk_msg(encore_task_s* const task){
  encore_task_msg_s* const msg = (encore_task_msg_s* const) pony_alloc_msg(0, _ENC__MSG_TASK);
  msg->_fut = task->fut;
  msg->_task = task;
  return msg;
}

inline static void encore_send_task(encore_task_msg_s* const msg){
  // send message to global queue of actors
  ponyint_mpmcq_push(&taskq, msg);
}

void task_schedule(encore_task_s const* const task){
  encore_task_msg_s* const msg = task_mk_msg((encore_task_s* const) task);
  encore_send_task(msg);
}
