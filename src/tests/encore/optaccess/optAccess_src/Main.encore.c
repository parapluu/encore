#include "header.h"


static void* trait_method_selector(int id)
{
  switch (id)
  {
    default:
    {
      printf("error, got invalid id: %d", id);
    }
  };
  return NULL;
}


struct _enc__class__optAccess_Main_t
{
  encore_actor_t _enc__actor;
};


void _enc__type_init__optAccess_Main(_enc__class__optAccess_Main_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__optAccess_Main(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__optAccess_Main_t* _this = p;
}


_enc__class__optAccess_Main_t* _enc__constructor__optAccess_Main(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__optAccess_Main_t* _this = ((_enc__class__optAccess_Main_t*) encore_create((*_ctx), (&(_enc__class__optAccess_Main_type))));
  return _this;
}


void* _enc__method__optAccess_Main_await(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_await(_ctx, _enc__arg_f);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  return UNIT;
}


future_t* _enc__method__optAccess_Main_await_future(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__optAccess_Main_await_t* msg = ((_enc__fut_msg__optAccess_Main_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__optAccess_Main_await_t)), _ENC__FUT_MSG__optAccess_Main_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__optAccess_Main_await_forward(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f, future_t* _fut)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__optAccess_Main_await_t* msg = ((_enc__fut_msg__optAccess_Main_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__optAccess_Main_await_t)), _ENC__FUT_MSG__optAccess_Main_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__optAccess_Main_await_one_way(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__optAccess_Main_await_t* msg = ((_enc__oneway_msg__optAccess_Main_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__optAccess_Main_await_t)), _ENC__ONEWAY_MSG__optAccess_Main_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__optAccess_Main_suspend(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  actor_suspend(_ctx);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  return UNIT;
}


future_t* _enc__method__optAccess_Main_suspend_future(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__optAccess_Main_suspend_t* msg = ((_enc__fut_msg__optAccess_Main_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__optAccess_Main_suspend_t)), _ENC__FUT_MSG__optAccess_Main_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__optAccess_Main_suspend_forward(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__optAccess_Main_suspend_t* msg = ((_enc__fut_msg__optAccess_Main_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__optAccess_Main_suspend_t)), _ENC__FUT_MSG__optAccess_Main_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__optAccess_Main_suspend_one_way(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__optAccess_Main_suspend_t* msg = ((_enc__oneway_msg__optAccess_Main_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__optAccess_Main_suspend_t)), _ENC__ONEWAY_MSG__optAccess_Main_suspend));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__optAccess_Main_main(pony_ctx_t** _ctx, _enc__class__optAccess_Main_t* _this, pony_type_t** runtimeType, array_t* _argv)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "main");
  /* val t = Just(new T(1)) */;
  /* t = Just(new T(1)) */;
  _enc__class__optAccess_T_t* _new_0 = _enc__constructor__optAccess_T(_ctx, NULL);
  int64_t _literal_1 = 1;
  pony_type_t* _tmp_2[] = {};
  _enc__type_init__optAccess_T(_new_0);
  _enc__method__optAccess_T_init(_ctx, _new_0, NULL, _literal_1);
  option_t* _option_3 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_0}), (&(_enc__class__optAccess_T_type)));
  option_t* _t_5 = _option_3;
  /* this.testFieldAccess(t) */;
  check_receiver(_this, ".", "this", "testFieldAccess", "\"optAccess.enc\" (line 83, column 5)");
  pony_type_t* _tmp_7[] = {};
  void* _sync_method_call_6 = _enc__method__optAccess_Main_testFieldAccess(_ctx, _this, NULL, _t_5);
  /* this.testMethodCall(t) */;
  check_receiver(_this, ".", "this", "testMethodCall", "\"optAccess.enc\" (line 84, column 5)");
  pony_type_t* _tmp_9[] = {};
  void* _sync_method_call_8 = _enc__method__optAccess_Main_testMethodCall(_ctx, _this, NULL, _t_5);
  /* this.testString(t) */;
  check_receiver(_this, ".", "this", "testString", "\"optAccess.enc\" (line 85, column 5)");
  pony_type_t* _tmp_11[] = {};
  void* _sync_method_call_10 = _enc__method__optAccess_Main_testString(_ctx, _this, NULL, _t_5);
  /* this.testFunctionCalls() */;
  check_receiver(_this, ".", "this", "testFunctionCalls", "\"optAccess.enc\" (line 86, column 5)");
  pony_type_t* _tmp_13[] = {};
  void* _sync_method_call_12 = _enc__method__optAccess_Main_testFunctionCalls(_ctx, _this, NULL);
  /* this.testArrayAccess() */;
  check_receiver(_this, ".", "this", "testArrayAccess", "\"optAccess.enc\" (line 87, column 5)");
  pony_type_t* _tmp_15[] = {};
  void* _sync_method_call_14 = _enc__method__optAccess_Main_testArrayAccess(_ctx, _this, NULL);
  /* this.testNothing() */;
  check_receiver(_this, ".", "this", "testNothing", "\"optAccess.enc\" (line 88, column 5)");
  pony_type_t* _tmp_17[] = {};
  void* _sync_method_call_16 = _enc__method__optAccess_Main_testNothing(_ctx, _this, NULL);
  /* this.testReturnOptAccess() */;
  check_receiver(_this, ".", "this", "testReturnOptAccess", "\"optAccess.enc\" (line 89, column 5)");
  pony_type_t* _tmp_19[] = {};
  option_t* _sync_method_call_18 = _enc__method__optAccess_Main_testReturnOptAccess(_ctx, _this, NULL);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "main");
  return UNIT;
}


future_t* _enc__method__optAccess_Main_main_future(pony_ctx_t** _ctx, _enc__class_