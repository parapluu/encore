#ifndef __pony_extensions_h__
#define __pony_extensions_h__

#include "mpmcq.h"
#include "scheduler.h"

bool just_suspended();
void set_just_suspended(bool value);
scheduler_t *ext_run_thread_start(scheduler_t *s);
void ext_run_loop_start();
bool in_main_thread();
void run_thread_restart();
pony_actor_t* ext_next_actor(scheduler_t* s, pony_actor_t* prev);
void ext_new_thread();

#endif
