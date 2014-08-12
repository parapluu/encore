#include "header.h"

pony_actor_t* create_and_send(pony_actor_type_t* type, uint64_t msg_id)
{
  pony_actor_t* ret = pony_create(type);;
  pony_send(ret, msg_id);;
  return ret;;
}

/////////////////////
// Common messages
pony_msg_t m_MSG_alloc = {0, {}};
pony_msg_t m_resume_get = {1, {PONY_NONE}};
pony_msg_t m_run_closure = {1, {PONY_NONE}};

int main(int argc, char** argv)
{
  init_futures(2, LAZY);;
  return pony_start(argc, argv, pony_create((&(Main_actor))));;
}
