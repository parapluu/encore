/**
 *  @file future_actor.h
 */

#include "set.h"

#ifndef __future_actor_h
#define __future_actor_h

enum
  {
    MSG_RUN_CLOSURE,
    MSG_RESUME,
    MSG_BLOCK,
    MSG_CHAIN,
    MSG_YIELD,
    MSG_FULFIL
  };

pony_msg_t* future_actor_message_type(uint64_t id);
void future_actor_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv);


typedef struct future_actor_fields {
  Set blocked;
  Set chained;
  Set yielded;
} future_actor_fields;

pony_actor_type_t future_actor_type =
  {
    1,
    {NULL, sizeof(future_actor_fields), PONY_ACTOR},
    future_actor_message_type,
    future_actor_dispatch
  };

#endif
