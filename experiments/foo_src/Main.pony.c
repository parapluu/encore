#include "header.h"

struct ___Main_data
{
  pony_actor_t* aref;
};


static void Main_trace(void* p)
{
}


static pony_msg_t m_Main_main = {0, {}};


static pony_msg_t m_Main__one_way_main = {0, {}};


static pony_msg_t* Main_message_type(uint64_t id)
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
    case PONY_MAIN:
    {
      return (&(m_Main_main));
      break;
    }
    case MSG_Main__one_way_main:
    {
      return (&(m_Main__one_way_main));
      break;
    }
    default:
    {
    }
  };
  return NULL;;
}

void* Main_main(Main_data* this, int64_t argc, char** argv)
{
  pony_actor_t* _tmp0 = create_and_send((&(Foo_actor)), MSG_alloc);;
  pony_actor_t* _tmp1 = _tmp0;;
  Bar_data* _tmp2 = pony_alloc(sizeof(Bar_data));;
  memset(_tmp2, 0, sizeof(Bar_data));;;
  Bar_data* _tmp3 = _tmp2;;
  ;
  void* _tmp4 = Bar_bar(_tmp3, ({ _tmp1;}));;;
  future_t* _tmp7 = future_mk();;
  pony_arg_t (_tmp8[1]) = {{.p = _tmp7}};;
  pony_sendv(({;
               pony_actor_t* _tmp5 = (*_tmp3).f;; _tmp5;}), MSG_Foo_foo, 1, _tmp8);;;
  UNIT;;;;;
  return UNIT;;
}


static void Main_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  switch (id)
  {
    /* case MSG_alloc: */
    /* { */
    /*   p = pony_alloc(sizeof(Main_data));;; */
    /*   memset(p, 0, sizeof(Main_data));; */
    /*   (*((Main_data*) p)).aref = this;; */
    /*   pony_set(p);; */
    /*   break; */
    /* } */
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
    case PONY_MAIN:
    {
      p = pony_alloc(sizeof(Main_data));;;
      memset(p, 0, sizeof(Main_data));;
      (*((Main_data*) p)).aref = this;;
      pony_set(p);;;
      Main_main(p, (argv[0]).i, ((char**) (argv[1]).p));;
      break;
    }
    default:
    {
      printf("error, got invalid id: %llu", id);
    }
  }
}

pony_actor_type_t Main_actor = {ID_Main, {sizeof(Main_data), Main_trace, NULL, NULL}, Main_message_type, Main_dispatch};