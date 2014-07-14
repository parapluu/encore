#ifndef messageq_h
#define messageq_h

#include <pony/pony.h>

typedef struct message_t
{
  uint64_t id;
  pony_arg_t argv[PONY_MAX_ARG];
  struct message_t* next;
} message_t;

typedef struct messageq_t
{
  message_t* head;
  message_t* tail;
} messageq_t;

void messageq_init(messageq_t* q);

void messageq_destroy(messageq_t* q);

bool messageq_push(messageq_t* q, uint64_t id, int argc, pony_arg_t* argv);

message_t* messageq_pop(messageq_t* q);

bool messageq_markempty(messageq_t* q);

#endif
