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


struct _enc__class__forwardPreexistingFutureFunctionCall_Foo_t
{
  encore_actor_t _enc__actor;
};


void _enc__type_init__forwardPreexistingFutureFunctionCall_Foo(_enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__forwardPreexistingFutureFunctionCall_Foo(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this = p;
}


_enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _enc__constructor__forwardPreexistingFutureFunctionCall_Foo(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this = ((_enc__class__forwardPreexistingFutureFunctionCall_Foo_t*) encore_create((*_ctx), (&(_enc__class__forwardPreexistingFutureFunctionCall_Foo_type))));
  return _this;
}


void* _enc__method__forwardPreexistingFutureFunctionCall_Foo_await(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_await(_ctx, _enc__arg_f);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  return UNIT;
}


future_t* _enc__method__forwardPreexistingFutureFunctionCall_Foo_await_future(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_await_t* msg = ((_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_await_t)), _ENC__FUT_MSG__forwardPreexistingFutureFunctionCall_Foo_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__forwardPreexistingFutureFunctionCall_Foo_await_forward(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f, future_t* _fut)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_await_t* msg = ((_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_await_t)), _ENC__FUT_MSG__forwardPreexistingFutureFunctionCall_Foo_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__forwardPreexistingFutureFunctionCall_Foo_await_one_way(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__forwardPreexistingFutureFunctionCall_Foo_await_t* msg = ((_enc__oneway_msg__forwardPreexistingFutureFunctionCall_Foo_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__forwardPreexistingFutureFunctionCall_Foo_await_t)), _ENC__ONEWAY_MSG__forwardPreexistingFutureFunctionCall_Foo_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__forwardPreexistingFutureFunctionCall_Foo_suspend(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  actor_suspend(_ctx);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  return UNIT;
}


future_t* _enc__method__forwardPreexistingFutureFunctionCall_Foo_suspend_future(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t* msg = ((_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t)), _ENC__FUT_MSG__forwardPreexistingFutureFunctionCall_Foo_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__forwardPreexistingFutureFunctionCall_Foo_suspend_forward(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t* msg = ((_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t)), _ENC__FUT_MSG__forwardPreexistingFutureFunctionCall_Foo_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__forwardPreexistingFutureFunctionCall_Foo_suspend_one_way(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t* msg = ((_enc__oneway_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__forwardPreexistingFutureFunctionCall_Foo_suspend_t)), _ENC__ONEWAY_MSG__forwardPreexistingFutureFunctionCall_Foo_suspend));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


int64_t _enc__method__forwardPreexistingFutureFunctionCall_Foo_foo(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "foo");
  int64_t _literal_0 = 42;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "foo");
  return ((int64_t) _literal_0);
}


future_t* _enc__method__forwardPreexistingFutureFunctionCall_Foo_foo_future(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_foo_t* msg = ((_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_foo_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__forwardPreexistingFutureFunctionCall_Foo_foo_t)), _ENC__FUT_MSG__forwardPreexistingFutureFunctionCall_Foo_foo));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__forwardPreexistingFutureFunctionCall_Foo_foo_forward(pony_ctx_t** _ctx, _enc__class__forwardPreexistingFutureFunctionCall_Foo_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  p