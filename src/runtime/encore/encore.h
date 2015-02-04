typedef union
{
  void* p;
  intptr_t i;
  double d;
} encore_arg_t;

typedef enum {
  CLOSURE_ID = 0
} encore_type_id;

typedef enum {
  _ENC__MSG_RESUME_GET,
  _ENC__MSG_RESUME_SUSPEND,
  _ENC__MSG_RESUME_AWAIT,
  _ENC__MSG_RUN_CLOSURE,
  _ENC__MSG_MAIN,
} encore_msg_id;

typedef struct encore_actor encore_actor_t;

struct encore_actor
{
  pony_actor_pad_t pad;
  // Everything else that goes into an encore_actor that's not part of PonyRT
};
