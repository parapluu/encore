#include "task.h"
#include "mpmcq.h"
#include <pony/pony.h>
#include <assert.h>
#include <stdio.h>
#include "encore.h"
#include "../src/actor/actor.h"

struct encore_task_s {
  task_fn run;
  void* env;
  void* dependencies;
  void* fut;
  pony_trace_fn trace;
};

typedef encore_arg_t value_t;

// TODO: Best practice: Put it into the header. => creates more dependencies...
extern void handle_task();
extern bool taskq_empty();
extern pony_type_t* encore_task_type;



// Global queue where actors can pick up tasks as messages
mpmcq_t taskq;
__thread pony_actor_t* this_encore_task;

void init_actor_task(){
  if(this_encore_task==NULL){
    this_encore_task = (pony_actor_t*) pony_create(encore_task_type);
    assert(this_encore_task!=NULL);
  }
}

static void set_encore_task_type(pony_type_t* type){
    encore_task_type = type;
}


void task_setup(pony_type_t* type){
  static int n_calls = 0;
  assert(n_calls++ == 0);

  mpmcq_init(&taskq);
  set_encore_task_type(type);
}

encore_task_s* task_mk(task_fn body, void* env, void* dependencies, pony_trace_fn trace){
  encore_task_s* task = pony_alloc(sizeof(encore_task_s)); // allocs in the heap of the running actor

  *task = (encore_task_s){.run = body, .env = env, .dependencies = dependencies, .trace = trace};
  return task;
}

value_t task_runner(encore_task_s* task){
  return task->run(task->env, task->dependencies);
}

void task_free(encore_task_s* task){
}

void task_attach_fut(encore_task_s* t, void* fut){
  t->fut = fut;  
}

// Forward declaration
static encore_task_msg_s* task_mk_msg(encore_task_s* task);
static void encore_send_task(encore_task_msg_s* msg);


// TODO: this makes the task library non-portable (depends on msg, actors, etc)
void task_schedule(encore_task_s* task){
  /* if(encore_task_type==NULL) */
  /*   set_encore_task_type(); */
  encore_task_msg_s* msg = task_mk_msg(task);
  encore_send_task(msg);
}

// create message
inline static encore_task_msg_s* task_mk_msg(encore_task_s* task){
  encore_task_msg_s* msg = (encore_task_msg_s*) pony_alloc_msg(0, _ENC__MSG_TASK);
  msg->_fut = task->fut;
  msg->_task = task;
  return msg;
}

inline static void encore_send_task(encore_task_msg_s* msg){
  // send message to global queue of actors
  mpmcq_push(&taskq, msg);
}


// TODO: query the taskq in a more efficient way
bool taskq_empty(){
  void* data = mpmcq_pop(&taskq);
  if(data==NULL){
    return true;
  }else{
    mpmcq_push(&taskq, data);
    return false;
  }
}
