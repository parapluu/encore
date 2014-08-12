#include "header.h"

struct ___Foo_data
{
  pony_actor_t* aref;
};

static void Foo_trace(void* p)
{
}

static pony_msg_t m_Foo_foo = {1, {PONY_ACTOR}};


static pony_msg_t m_Foo__one_way_foo = {0, {}};


static pony_msg_t* Foo_message_type(uint64_t id)
{
  switch (id)
  {
    case MSG_alloc:
    {
      return (&(m_MSG_alloc));
      break;
    }
    case FUT_MSG_RESUME:
    {
      return (&(m_resume_get));
      break;
    }
    case FUT_MSG_RUN_CLOSURE:
    {
      return (&(m_run_closure));
      break;
    }
    case MSG_Foo_foo:
    {
      return (&(m_Foo_foo));
      break;
    }
    case MSG_Foo__one_way_foo:
    {
      return (&(m_Foo__one_way_foo));
      break;
    }
    default:
    {
    }
  };
  return NULL;;
}


void* Foo_foo(Foo_data* this)
{
  char* _tmp0 = "Hello Ponyworld";;
  printf("%s\n", _tmp0);;;
  return UNIT;;
}


static void Foo_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  switch (id)
  {
    case MSG_alloc:
    {
      p = pony_alloc(sizeof(Foo_data));;;
      memset(p, 0, sizeof(Foo_data));;
      (*((Foo_data*) p)).aref = this;;
      pony_set(p);;
      break;
    }
    case FUT_MSG_RESUME:
    {
      resumable_t* r = (argv[0]).p;;
      future_resume(r);;
      break;
    }
    case FUT_MSG_RUN_CLOSURE:
    {
      closure_t* closure = (argv[0]).p;;
      value_t closure_arguments[] = {{.p = (argv[1]).p}};;
      closure_call(closure, closure_arguments);;
      break;
    }
    case MSG_Foo_foo:
    {
      future_t* fut = (argv[0]).p;;
      future_fulfil(fut, ((void*) Foo_foo(p)));;
      break;
    }
    case MSG_Foo__one_way_foo:
    {
      Foo_foo(p);
      break;
    }
    default:
    {
      printf("error, got invalid id: %llu", id);
    }
  }
}

pony_actor_type_t Foo_actor = {ID_Foo, {sizeof(Foo_data), Foo_trace, NULL, NULL}, Foo_message_type, Foo_dispatch};