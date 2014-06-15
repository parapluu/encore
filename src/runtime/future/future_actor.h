/**
 *  @file future_actor.h
 */

#include <stdbool.h>
#include "pony/pony.h"

#ifndef __future_actor_h
#define __future_actor_h

enum
  {
    FUT_MSG_RUN_CLOSURE,
    FUT_MSG_RESUME,
    FUT_MSG_BLOCK,
    FUT_MSG_CHAIN,
    FUT_MSG_AWAIT,
    FUT_MSG_FULFIL
  };

pony_actor_t* future_create();
pony_msg_t* future_actor_message_type(uint64_t id);
void future_actor_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);
volatile void *future_actor_get_value(pony_actor_t* this);
void future_actor_set_value(pony_actor_t* this, volatile void *value);
volatile bool future_actor_get_fulfilled(pony_actor_t* this);
void future_actor_set_fulfilled(pony_actor_t* this, volatile bool fulfilled);


#endif
