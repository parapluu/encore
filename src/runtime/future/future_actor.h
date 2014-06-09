/**
 *  @file future_actor.h
 */

#include "ccontext.h"

#ifndef __future_actor_h
#define __future_actor_h

enum
  {
    FUT_MSG_RUN_CLOSURE,
    FUT_MSG_RESUME,
    FUT_MSG_BLOCK,
    FUT_MSG_CHAIN,
    FUT_MSG_YIELD,
    FUT_MSG_FULFIL
  };

pony_msg_t* future_actor_message_type(uint64_t id);
void future_actor_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);

#endif
