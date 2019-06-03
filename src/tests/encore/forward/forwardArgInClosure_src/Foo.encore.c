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


struct _enc__class__forwardArgInClosure_Foo_t
{
  encore_actor_t _enc__actor;
};


void _enc__type_init__forwardArgInClosure_Foo(_enc__class__forwardArgInClosure_Foo_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__forwardArgInClosure_Foo(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__forwardArgInClosure_Foo_t* _this = p;
}


_enc__class__forwardArgInClosure_Foo_t* _enc__constructor__forwardArgInClosure_Foo(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__forwardArgInClosure_Foo_t* _this = ((_enc__class__forwardArgInClosure_Foo_t*) encore_create((*_ctx), (&(_enc__class__forwardArgInClosure_Foo_type))));
  return _this;
}


void* _enc__method__forwardArgInClosure_Foo_await(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_await(_ctx, _enc__arg_f);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  return UNIT;
}


future_t* _enc__method__forwardArgInClosure_Foo_await_future(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardArgInClosure_Foo_await_t* msg = ((_enc__fut_msg__forwardArgInClosure_Foo_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardArgInClosure_Foo_await_t)), _ENC__FUT_MSG__forwardArgInClosure_Foo_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__forwardArgInClosure_Foo_await_forward(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f, future_t* _fut)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardArgInClosure_Foo_await_t* msg = ((_enc__fut_msg__forwardArgInClosure_Foo_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardArgInClosure_Foo_await_t)), _ENC__FUT_MSG__forwardArgInClosure_Foo_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__forwardArgInClosure_Foo_await_one_way(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__forwardArgInClosure_Foo_await_t* msg = ((_enc__oneway_msg__forwardArgInClosure_Foo_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__forwardArgInClosure_Foo_await_t)), _ENC__ONEWAY_MSG__forwardArgInClosure_Foo_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__forwardArgInClosure_Foo_suspend(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  actor_suspend(_ctx);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  return UNIT;
}


future_t* _enc__method__forwardArgInClosure_Foo_suspend_future(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardArgInClosure_Foo_suspend_t* msg = ((_enc__fut_msg__forwardArgInClosure_Foo_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardArgInClosure_Foo_suspend_t)), _ENC__FUT_MSG__forwardArgInClosure_Foo_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__forwardArgInClosure_Foo_suspend_forward(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardArgInClosure_Foo_suspend_t* msg = ((_enc__fut_msg__forwardArgInClosure_Foo_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardArgInClosure_Foo_suspend_t)), _ENC__FUT_MSG__forwardArgInClosure_Foo_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__forwardArgInClosure_Foo_suspend_one_way(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__forwardArgInClosure_Foo_suspend_t* msg = ((_enc__oneway_msg__forwardArgInClosure_Foo_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__forwardArgInClosure_Foo_suspend_t)), _ENC__ONEWAY_MSG__forwardArgInClosure_Foo_suspend));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


future_t* _enc__method__forwardArgInClosure_Foo_duplicate(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "duplicate");
  _enc__class__forwardArgInClosure_Base_t* _new_0 = _enc__constructor__forwardArgInClosure_Base(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init__forwardArgInClosure_Base(_new_0);
  _enc__method__forwardArgInClosure_Base_init_one_way(_ctx, _new_0, NULL);
  check_receiver(_new_0, " ! ", "new Base()", "base", "\"forwardArgInClosure.enc\" (line 17, column 16)");
  pony_type_t* _tmp_2[] = {};
  future_t* _fut_3 = _enc__method__forwardArgInClosure_Base_base_future(_ctx, _new_0, NULL);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "duplicate");
  return ((future_t*) _fut_3);
}


future_t* _enc__method__forwardArgInClosure_Foo_duplicate_future(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, (&(future_type)));
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardArgInClosure_Foo_duplicate_t* msg = ((_enc__fut_msg__forwardArgInClosure_Foo_duplicate_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardArgInClosure_Foo_duplicate_t)), _ENC__FUT_MSG__forwardArgInClosure_Foo_duplicate));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__forwardArgInClosure_Foo_duplicate_forward(pony_ctx_t** _ctx, _enc__class__forwardArgInClosure_Foo_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardArgInClosure_Foo_duplicate_t* msg = ((_enc__fut_msg__forwardArgInClosure_Foo_duplicate_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardArgInClosure_Foo_duplicate_t)), _ENC__FUT_MSG__forwardArgInClosure_Foo_duplicate));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__forwardArgInCl