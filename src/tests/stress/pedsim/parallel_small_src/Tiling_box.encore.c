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


struct _enc__class__Ped_util_Regions_Tiling_box_t
{
  encore_actor_t _enc__actor;
  int64_t _enc__field_futs_left;
  array_t* _enc__field_futures;
  array_t* _enc__field_boxes;
};


void _enc__type_init__Ped_util_Regions_Tiling_box(_enc__class__Ped_util_Regions_Tiling_box_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_Regions_Tiling_box(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_Regions_Tiling_box_t* _this = p;
  int64_t _enc__field_futs_left = _this->_enc__field_futs_left;
  /* Not tracing field '_enc__field_futs_left' */;
  array_t* _enc__field_futures = _this->_enc__field_futures;
  encore_trace_object((*_ctx), _enc__field_futures, array_trace);
  array_t* _enc__field_boxes = _this->_enc__field_boxes;
  encore_trace_object((*_ctx), _enc__field_boxes, array_trace);
}


_enc__class__Ped_util_Regions_Tiling_box_t* _enc__constructor__Ped_util_Regions_Tiling_box(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_Regions_Tiling_box_t* _this = ((_enc__class__Ped_util_Regions_Tiling_box_t*) encore_create((*_ctx), (&(_enc__class__Ped_util_Regions_Tiling_box_type))));
  return _this;
}


void* _enc__method__Ped_util_Regions_Tiling_box_await(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_await(_ctx, _enc__arg_f);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_await_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_await_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_await_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_await_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f, future_t* _fut)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_await_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_await_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Tiling_box_await_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__Ped_util_Regions_Tiling_box_suspend(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  actor_suspend(_ctx);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_suspend_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_suspend_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Tiling_box_suspend_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Tiling_box_suspend_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Tiling_box_suspend_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_suspend));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


array_t* _enc__method__Ped_util_Regions_Tiling_box_agents(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  /* val temp = new [[(int, int)]](|this.boxes|) */;
  /* temp = new [[(int, int)]](|this.boxes|) */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
  array_t* _fieldacc_1 = (*_this)._enc__field_boxes;
  int64_t _size_2 = array_size(_fieldacc_1);
  array_t* _array_0 = array_mk(_ctx, _size_2, (&(array_type)));
  array_t* _temp_4 = _array_0;
  /* for i <- [0..|this.boxes| - 1] do
  temp(i) = get(this.boxes(i)!agents())
end */;
  void* _for_5;
  /* Range not generated */;
  int64_t _literal_12 = 0;
  int64_t _binop_16 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                         array_t* _fieldacc_13 = (*_this)._enc__field_boxes;
                         int64_t _size_14 = array_size(_fieldacc_13); _size_14;}) - ({int64_t _literal_15 = 1; _literal_15;}));
  int64_t _literal_17 = 1;
  int64_t _literal_18 = 1;
  int64_t _step_10 = (_literal_18 * _literal_17);
  range_assert_step(_step_10);
  int64_t _index_6;
  if ((_step_10 > 0))
  {
    _index_6 = _literal_12;
  }
  else
  {
    _index_6 = _binop_16;
  };
  while (((_index_6 >= _literal_12) && (_index_6 <= _binop_16)))
  {
    int64_t _i_7 = _index_6;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
    array_t* _fieldacc_19 = (*_this)._enc__field_boxes;
    _enc__class__Ped_util_Regions_Box_t* _access_20 = array_get(_fieldacc_19, _i_7).p;
    check_receiver(_access_20, " ! ", "(this.boxes)(i)", "agents", "\"./Ped_util/Regions.enc\" (line 107, column 37)");
    pony_type_t* _tmp_21[] = {};
    future_t* _fut_22 = _enc__method__Ped_util_Regions_Box_agents_future(_ctx, _access_20, NULL);
    array_t* _tmp_23 = future_get_actor(_ctx, _fut_22).p;
    array_set(_temp_4, _i_7, ((encore_arg_t) {.p = _tmp_23}));
    _for_5 = UNIT;
    _index_6 = (_index_6 + _step_10);
  };
  /* var a = flatten(temp) */;
  /* a = flatten(temp) */;
  ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.flatten");
  pony_type_t* _tmp_24[] = {};
  array_t* _fun_call_25 = _enc__global_fun__Ped_util_Global_funsflatten(_ctx, NULL, _temp_4);
  array_t* _a_27 = _fun_call_25;
  /* a */;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  return ((array_t*) _a_27);
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_agents_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, (&(array_type)));
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_agents));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_agents_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_agents));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Tiling_box_agents_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Tiling_box_agents_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_agents_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Tiling_box_agents_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_agents));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__Ped_util_Regions_Tiling_box_move(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_i)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "move");
  /* for id <- [0..|this.boxes| - 1] do
  (this.futures)(id) = ((this.boxes)(id))!move()
end */;
  void* _for_0;
  /* Range not generated */;
  int64_t _literal_7 = 0;
  int64_t _binop_11 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                         array_t* _fieldacc_8 = (*_this)._enc__field_boxes;
                         int64_t _size_9 = array_size(_fieldacc_8); _size_9;}) - ({int64_t _literal_10 = 1; _literal_10;}));
  int64_t _literal_12 = 1;
  int64_t _literal_13 = 1;
  int64_t _step_5 = (_literal_13 * _literal_12);
  range_assert_step(_step_5);
  int64_t _index_1;
  if ((_step_5 > 0))
  {
    _index_1 = _literal_7;
  }
  else
  {
    _index_1 = _binop_11;
  };
  while (((_index_1 >= _literal_7) && (_index_1 <= _binop_11)))
  {
    int64_t _id_2 = _index_1;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
    array_t* _fieldacc_14 = (*_this)._enc__field_boxes;
    _enc__class__Ped_util_Regions_Box_t* _access_15 = array_get(_fieldacc_14, _id_2).p;
    check_receiver(_access_15, " ! ", "(this.boxes)(id)", "move", "\"./Ped_util/Regions.enc\" (line 92, column 47)");
    pony_type_t* _tmp_16[] = {};
    future_t* _fut_17 = _enc__method__Ped_util_Regions_Box_move_future(_ctx, _access_15, NULL);
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "futures");
    array_t* _fieldacc_18 = (*_this)._enc__field_futures;
    array_set(_fieldacc_18, _id_2, ((encore_arg_t) {.p = _fut_17}));
    _for_0 = UNIT;
    _index_1 = (_index_1 + _step_5);
  };
  /* for f <- this.futures do
  get(f)
end */;
  void* _for_19;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "futures");
  array_t* _fieldacc_26 = (*_this)._enc__field_futures;
  int64_t _start_22 = 0;
  int64_t _stop_23 = (array_size(_fieldacc_26) - 1);
  int64_t _src_step_25 = 1;
  int64_t _literal_27 = 1;
  int64_t _step_24 = (_literal_27 * _src_step_25);
  range_assert_step(_step_24);
  int64_t _index_20;
  if ((_step_24 > 0))
  {
    _index_20 = _start_22;
  }
  else
  {
    _index_20 = _stop_23;
  };
  while (((_index_20 >= _start_22) && (_index_20 <= _stop_23)))
  {
    future_t* _f_21 = array_get(_fieldacc_26, _index_20).p;
    int64_t _tmp_28 = future_get_actor(_ctx, _f_21).i;
    _for_19 = _tmp_28;
    _index_20 = (_index_20 + _step_24);
  };
  /* if i > 1 then
  this!move(i - 1)
else
  print("DONE")
end */;
  void* _ite_29;
  if (({int64_t _binop_31 = (({ _enc__arg_i;}) > ({int64_t _literal_30 = 1; _literal_30;})); _binop_31;}))
  {
    check_receiver(_this, " ! ", "this", "move", "\"./Ped_util/Regions.enc\" (line 98, column 11)");
    int64_t _binop_33 = (({ _enc__arg_i;}) - ({int64_t _literal_32 = 1; _literal_32;}));
    pony_type_t* _tmp_34[] = {};
    _enc__method__Ped_util_Regions_Tiling_box_move_one_way(_ctx, _this, NULL, _binop_33);
    _ite_29 = ((void*) UNIT);
  }
  else
  {
    char* _literal_35 = "DONE";
    fprintf(stdout, "%s", _literal_35);
    _ite_29 = ((void*) UNIT);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "move");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_move_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_i)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  /* Not tracing field '_enc__arg_i' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_move_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_move_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_move));
  msg->f1 = _enc__arg_i;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_move_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_i, future_t* _fut)
{
  pony_gc_send((*_ctx));
  /* Not tracing field '_enc__arg_i' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_move_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_move_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_move));
  msg->f1 = _enc__arg_i;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Tiling_box_move_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_i)
{
  pony_gc_send((*_ctx));
  /* Not tracing field '_enc__arg_i' */;
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_move));
  msg->f1 = _enc__arg_i;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__Ped_util_Regions_Tiling_box_init(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, array_t* _enc__arg_agents, int64_t _enc__arg_n)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.find_extreme");
  pony_type_t* _tmp_1[] = {};
  tuple_t* _fun_call_2 = _enc__global_fun__Ped_util_Global_funsfind_extreme(_ctx, NULL, _enc__arg_agents);
  void* _match_0;
  int64_t _xmax_3;
  int64_t _ymax_4;
  int64_t _xmin_5;
  int64_t _ymin_6;
  if ((({int64_t _tupleCheck_283;
         _tupleCheck_283 = 1;
         int64_t _tupleAccess_284 = tuple_get(_fun_call_2, 0).i;
         int64_t _varBinding_285;
         _xmax_3 = _tupleAccess_284;
         _varBinding_285 = 1;
         _tupleCheck_283 = (_tupleCheck_283 && _varBinding_285);
         int64_t _tupleAccess_286 = tuple_get(_fun_call_2, 1).i;
         int64_t _varBinding_287;
         _ymax_4 = _tupleAccess_286;
         _varBinding_287 = 1;
         _tupleCheck_283 = (_tupleCheck_283 && _varBinding_287);
         int64_t _tupleAccess_288 = tuple_get(_fun_call_2, 2).i;
         int64_t _varBinding_289;
         _xmin_5 = _tupleAccess_288;
         _varBinding_289 = 1;
         _tupleCheck_283 = (_tupleCheck_283 && _varBinding_289);
         int64_t _tupleAccess_290 = tuple_get(_fun_call_2, 3).i;
         int64_t _varBinding_291;
         _ymin_6 = _tupleAccess_290;
         _varBinding_291 = 1;
         _tupleCheck_283 = (_tupleCheck_283 && _varBinding_291); _tupleCheck_283;}) && ({int64_t _literal_292 = 1/*True*/; _literal_292;})))
  {
    _match_0 = ((void*) ({/* val dx = xmax - xmin / n */;
                          /* dx = xmax - xmin / n */;
                          int64_t _binop_8 = (({int64_t _binop_7 = (({ _xmax_3;}) - ({ _xmin_5;})); _binop_7;}) / ({ _enc__arg_n;}));
                          int64_t _dx_10 = _binop_8;
                          /* this.boxes = new [Box](n * n) */;
                          int64_t _binop_12 = (({ _enc__arg_n;}) * ({ _enc__arg_n;}));
                          array_t* _array_11 = array_mk(_ctx, _binop_12, ENCORE_ACTIVE);
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes"); _this;}))._enc__field_boxes = _array_11;
                          /* this.futures = new [Fut[bool]](|this.boxes|) */;
                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                          array_t* _fieldacc_14 = (*_this)._enc__field_boxes;
                          int64_t _size_15 = array_size(_fieldacc_14);
                          array_t* _array_13 = array_mk(_ctx, _size_15, (&(future_type)));
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "futures"); _this;}))._enc__field_futures = _array_13;
                          /* if n > 1 then
  var x = xmin
  var y = ymin
  var i = 0
  var win = true
  val dy = ymax - ymin / n
  for xindex <- [0..n - 1] do
    for yindex <- [0..n - 2] do
      (this.boxes)(i) = new Box((x + dx - 1, y + dy - 1), (x, y))
      if yindex != 0 then
        win = win && link((this.boxes)(i), (this.boxes)(i - 1))
      end
      if xindex != 0 then
        win = win && link((this.boxes)(i), (this.boxes)(i - n))
      end
      if xindex != 0 && yindex != 0 then
        win = win && link((this.boxes)(i), (this.boxes)(i - n + 1))
        win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
        ()
      end
      i = i + 1
      y = y + dy
    end
    (this.boxes)(i) = new Box((x + dx - 1, ymax), (x, y))
    if xindex != 0 then
      win = win && link((this.boxes)(i), (this.boxes)(i - n))
      win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
    end
    win = win && link((this.boxes)(i), (this.boxes)(i - 1))
    i = i + 1
    x = x + dx
    y = ymin
  end
  for yindex <- [0..n - 2] do
    (this.boxes)(i) = new Box((xmax, y + dy - 1), (x, y))
    if yindex != 0 then
      win = win && link((this.boxes)(i), (this.boxes)(i - 1))
      win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
    end
    win = win && link((this.boxes)(i), (this.boxes)(i - n + 1))
    win = win && link((this.boxes)(i), (this.boxes)(i - n))
    i = i + 1
    y = y + dy
  end
  (this.boxes)(i) = new Box((xmax, ymax), (x, y))
  win = win && link((this.boxes)(i), (this.boxes)(i - n))
  win = win && link((this.boxes)(i), (this.boxes)(i - 1))
  win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
else
  this.boxes(0) = new Box((xmax, ymax), (xmin, ymin))
end */;
                          void* _ite_16;
                          if (({int64_t _binop_18 = (({ _enc__arg_n;}) > ({int64_t _literal_17 = 1; _literal_17;})); _binop_18;}))
                          {
                            /* var x = xmin */;
                            /* x = xmin */;
                            int64_t _x_20 = _xmin_5;
                            /* var y = ymin */;
                            /* y = ymin */;
                            int64_t _y_22 = _ymin_6;
                            /* var i = 0 */;
                            /* i = 0 */;
                            int64_t _literal_23 = 0;
                            int64_t _i_25 = _literal_23;
                            /* var win = true */;
                            /* win = true */;
                            int64_t _literal_26 = 1/*True*/;
                            int64_t _win_28 = _literal_26;
                            /* val dy = ymax - ymin / n */;
                            /* dy = ymax - ymin / n */;
                            int64_t _binop_30 = (({int64_t _binop_29 = (({ _ymax_4;}) - ({ _ymin_6;})); _binop_29;}) / ({ _enc__arg_n;}));
                            int64_t _dy_32 = _binop_30;
                            /* for xindex <- [0..n - 1] do
  for yindex <- [0..n - 2] do
    (this.boxes)(i) = new Box((x + dx - 1, y + dy - 1), (x, y))
    if yindex != 0 then
      win = win && link((this.boxes)(i), (this.boxes)(i - 1))
    end
    if xindex != 0 then
      win = win && link((this.boxes)(i), (this.boxes)(i - n))
    end
    if xindex != 0 && yindex != 0 then
      win = win && link((this.boxes)(i), (this.boxes)(i - n + 1))
      win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
      ()
    end
    i = i + 1
    y = y + dy
  end
  (this.boxes)(i) = new Box((x + dx - 1, ymax), (x, y))
  if xindex != 0 then
    win = win && link((this.boxes)(i), (this.boxes)(i - n))
    win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
  end
  win = win && link((this.boxes)(i), (this.boxes)(i - 1))
  i = i + 1
  x = x + dx
  y = ymin
end */;
                            void* _for_33;
                            /* Range not generated */;
                            int64_t _literal_40 = 0;
                            int64_t _binop_42 = (({ _enc__arg_n;}) - ({int64_t _literal_41 = 1; _literal_41;}));
                            int64_t _literal_43 = 1;
                            int64_t _literal_44 = 1;
                            int64_t _step_38 = (_literal_44 * _literal_43);
                            range_assert_step(_step_38);
                            int64_t _index_34;
                            if ((_step_38 > 0))
                            {
                              _index_34 = _literal_40;
                            }
                            else
                            {
                              _index_34 = _binop_42;
                            };
                            while (((_index_34 >= _literal_40) && (_index_34 <= _binop_42)))
                            {
                              int64_t _xindex_35 = _index_34;
                              /* for yindex <- [0..n - 2] do
  (this.boxes)(i) = new Box((x + dx - 1, y + dy - 1), (x, y))
  if yindex != 0 then
    win = win && link((this.boxes)(i), (this.boxes)(i - 1))
  end
  if xindex != 0 then
    win = win && link((this.boxes)(i), (this.boxes)(i - n))
  end
  if xindex != 0 && yindex != 0 then
    win = win && link((this.boxes)(i), (this.boxes)(i - n + 1))
    win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
    ()
  end
  i = i + 1
  y = y + dy
end */;
                              void* _for_45;
                              /* Range not generated */;
                              int64_t _literal_52 = 0;
                              int64_t _binop_54 = (({ _enc__arg_n;}) - ({int64_t _literal_53 = 2; _literal_53;}));
                              int64_t _literal_55 = 1;
                              int64_t _literal_56 = 1;
                              int64_t _step_50 = (_literal_56 * _literal_55);
                              range_assert_step(_step_50);
                              int64_t _index_46;
                              if ((_step_50 > 0))
                              {
                                _index_46 = _literal_52;
                              }
                              else
                              {
                                _index_46 = _binop_54;
                              };
                              while (((_index_46 >= _literal_52) && (_index_46 <= _binop_54)))
                              {
                                int64_t _yindex_47 = _index_46;
                                /* (this.boxes)(i) = new Box((x + dx - 1, y + dy - 1), (x, y)) */;
                                _enc__class__Ped_util_Regions_Box_t* _new_57 = _enc__constructor__Ped_util_Regions_Box(_ctx, NULL);
                                tuple_t* _tuple_58 = tuple_mk(_ctx, 2);
                                tuple_set_type(_tuple_58, 0, ENCORE_PRIMITIVE);
                                tuple_set_type(_tuple_58, 1, ENCORE_PRIMITIVE);
                                int64_t _binop_61 = (({int64_t _binop_59 = (({ _x_20;}) + ({ _dx_10;})); _binop_59;}) - ({int64_t _literal_60 = 1; _literal_60;}));
                                int64_t _binop_64 = (({int64_t _binop_62 = (({ _y_22;}) + ({ _dy_32;})); _binop_62;}) - ({int64_t _literal_63 = 1; _literal_63;}));
                                tuple_set(_tuple_58, 0, ((encore_arg_t) {.i = _binop_61}));
                                tuple_set(_tuple_58, 1, ((encore_arg_t) {.i = _binop_64}));
                                tuple_t* _tuple_65 = tuple_mk(_ctx, 2);
                                tuple_set_type(_tuple_65, 0, ENCORE_PRIMITIVE);
                                tuple_set_type(_tuple_65, 1, ENCORE_PRIMITIVE);
                                tuple_set(_tuple_65, 0, ((encore_arg_t) {.i = _x_20}));
                                tuple_set(_tuple_65, 1, ((encore_arg_t) {.i = _y_22}));
                                pony_type_t* _tmp_66[] = {};
                                _enc__type_init__Ped_util_Regions_Box(_new_57);
                                _enc__method__Ped_util_Regions_Box_init_one_way(_ctx, _new_57, NULL, _tuple_58, _tuple_65);
                                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                array_t* _fieldacc_67 = (*_this)._enc__field_boxes;
                                array_set(_fieldacc_67, _i_25, ((encore_arg_t) {.p = _new_57}));
                                /* if yindex != 0 then
  win = win && link((this.boxes)(i), (this.boxes)(i - 1))
end */;
                                void* _ite_68;
                                if (({int64_t _binop_70 = (({ _yindex_47;}) != ({int64_t _literal_69 = 0; _literal_69;})); _binop_70;}))
                                {
                                  int64_t _binop_79 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                          array_t* _fieldacc_71 = (*_this)._enc__field_boxes;
                                                                          _enc__class__Ped_util_Regions_Box_t* _access_72 = array_get(_fieldacc_71, _i_25).p;
                                                                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                          array_t* _fieldacc_73 = (*_this)._enc__field_boxes;
                                                                          int64_t _binop_75 = (({ _i_25;}) - ({int64_t _literal_74 = 1; _literal_74;}));
                                                                          _enc__class__Ped_util_Regions_Box_t* _access_76 = array_get(_fieldacc_73, _binop_75).p;
                                                                          ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                          pony_type_t* _tmp_77[] = {};
                                                                          int64_t _fun_call_78 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_72, _access_76); _fun_call_78;}));
                                  _win_28 = _binop_79;
                                  _ite_68 = ((void*) UNIT);
                                }
                                else
                                {
                                  UNIT;
                                  _ite_68 = ((void*) UNIT);
                                };
                                /* if xindex != 0 then
  win = win && link((this.boxes)(i), (this.boxes)(i - n))
end */;
                                void* _ite_80;
                                if (({int64_t _binop_82 = (({ _xindex_35;}) != ({int64_t _literal_81 = 0; _literal_81;})); _binop_82;}))
                                {
                                  int64_t _binop_90 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                          array_t* _fieldacc_83 = (*_this)._enc__field_boxes;
                                                                          _enc__class__Ped_util_Regions_Box_t* _access_84 = array_get(_fieldacc_83, _i_25).p;
                                                                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                          array_t* _fieldacc_85 = (*_this)._enc__field_boxes;
                                                                          int64_t _binop_86 = (({ _i_25;}) - ({ _enc__arg_n;}));
                                                                          _enc__class__Ped_util_Regions_Box_t* _access_87 = array_get(_fieldacc_85, _binop_86).p;
                                                                          ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                          pony_type_t* _tmp_88[] = {};
                                                                          int64_t _fun_call_89 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_84, _access_87); _fun_call_89;}));
                                  _win_28 = _binop_90;
                                  _ite_80 = ((void*) UNIT);
                                }
                                else
                                {
                                  UNIT;
                                  _ite_80 = ((void*) UNIT);
                                };
                                /* if xindex != 0 && yindex != 0 then
  win = win && link((this.boxes)(i), (this.boxes)(i - n + 1))
  win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
  ()
end */;
                                void* _ite_91;
                                if (({int64_t _binop_96 = (({int64_t _binop_93 = (({ _xindex_35;}) != ({int64_t _literal_92 = 0; _literal_92;})); _binop_93;}) && ({int64_t _binop_95 = (({ _yindex_47;}) != ({int64_t _literal_94 = 0; _literal_94;})); _binop_95;})); _binop_96;}))
                                {
                                  /* win = win && link((this.boxes)(i), (this.boxes)(i - n + 1)) */;
                                  int64_t _binop_106 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                           array_t* _fieldacc_97 = (*_this)._enc__field_boxes;
                                                                           _enc__class__Ped_util_Regions_Box_t* _access_98 = array_get(_fieldacc_97, _i_25).p;
                                                                           ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                           array_t* _fieldacc_99 = (*_this)._enc__field_boxes;
                                                                           int64_t _binop_102 = (({int64_t _binop_100 = (({ _i_25;}) - ({ _enc__arg_n;})); _binop_100;}) + ({int64_t _literal_101 = 1; _literal_101;}));
                                                                           _enc__class__Ped_util_Regions_Box_t* _access_103 = array_get(_fieldacc_99, _binop_102).p;
                                                                           ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                           pony_type_t* _tmp_104[] = {};
                                                                           int64_t _fun_call_105 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_98, _access_103); _fun_call_105;}));
                                  _win_28 = _binop_106;
                                  /* win = win && link((this.boxes)(i), (this.boxes)(i - n - 1)) */;
                                  int64_t _binop_116 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                           array_t* _fieldacc_107 = (*_this)._enc__field_boxes;
                                                                           _enc__class__Ped_util_Regions_Box_t* _access_108 = array_get(_fieldacc_107, _i_25).p;
                                                                           ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                           array_t* _fieldacc_109 = (*_this)._enc__field_boxes;
                                                                           int64_t _binop_112 = (({int64_t _binop_110 = (({ _i_25;}) - ({ _enc__arg_n;})); _binop_110;}) - ({int64_t _literal_111 = 1; _literal_111;}));
                                                                           _enc__class__Ped_util_Regions_Box_t* _access_113 = array_get(_fieldacc_109, _binop_112).p;
                                                                           ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                           pony_type_t* _tmp_114[] = {};
                                                                           int64_t _fun_call_115 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_108, _access_113); _fun_call_115;}));
                                  _win_28 = _binop_116;
                                  /* () */;
                                  UNIT;
                                  _ite_91 = ((void*) UNIT);
                                }
                                else
                                {
                                  UNIT;
                                  _ite_91 = ((void*) UNIT);
                                };
                                /* i = i + 1 */;
                                int64_t _binop_118 = (({ _i_25;}) + ({int64_t _literal_117 = 1; _literal_117;}));
                                _i_25 = _binop_118;
                                /* y = y + dy */;
                                int64_t _binop_119 = (({ _y_22;}) + ({ _dy_32;}));
                                _y_22 = _binop_119;
                                _for_45 = UNIT;
                                _index_46 = (_index_46 + _step_50);
                              };
                              /* (this.boxes)(i) = new Box((x + dx - 1, ymax), (x, y)) */;
                              _enc__class__Ped_util_Regions_Box_t* _new_120 = _enc__constructor__Ped_util_Regions_Box(_ctx, NULL);
                              tuple_t* _tuple_121 = tuple_mk(_ctx, 2);
                              tuple_set_type(_tuple_121, 0, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_121, 1, ENCORE_PRIMITIVE);
                              int64_t _binop_124 = (({int64_t _binop_122 = (({ _x_20;}) + ({ _dx_10;})); _binop_122;}) - ({int64_t _literal_123 = 1; _literal_123;}));
                              tuple_set(_tuple_121, 0, ((encore_arg_t) {.i = _binop_124}));
                              tuple_set(_tuple_121, 1, ((encore_arg_t) {.i = _ymax_4}));
                              tuple_t* _tuple_125 = tuple_mk(_ctx, 2);
                              tuple_set_type(_tuple_125, 0, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_125, 1, ENCORE_PRIMITIVE);
                              tuple_set(_tuple_125, 0, ((encore_arg_t) {.i = _x_20}));
                              tuple_set(_tuple_125, 1, ((encore_arg_t) {.i = _y_22}));
                              pony_type_t* _tmp_126[] = {};
                              _enc__type_init__Ped_util_Regions_Box(_new_120);
                              _enc__method__Ped_util_Regions_Box_init_one_way(_ctx, _new_120, NULL, _tuple_121, _tuple_125);
                              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                              array_t* _fieldacc_127 = (*_this)._enc__field_boxes;
                              array_set(_fieldacc_127, _i_25, ((encore_arg_t) {.p = _new_120}));
                              /* if xindex != 0 then
  win = win && link((this.boxes)(i), (this.boxes)(i - n))
  win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
end */;
                              void* _ite_128;
                              if (({int64_t _binop_130 = (({ _xindex_35;}) != ({int64_t _literal_129 = 0; _literal_129;})); _binop_130;}))
                              {
                                /* win = win && link((this.boxes)(i), (this.boxes)(i - n)) */;
                                int64_t _binop_138 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_131 = (*_this)._enc__field_boxes;
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_132 = array_get(_fieldacc_131, _i_25).p;
                                                                         ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_133 = (*_this)._enc__field_boxes;
                                                                         int64_t _binop_134 = (({ _i_25;}) - ({ _enc__arg_n;}));
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_135 = array_get(_fieldacc_133, _binop_134).p;
                                                                         ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                         pony_type_t* _tmp_136[] = {};
                                                                         int64_t _fun_call_137 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_132, _access_135); _fun_call_137;}));
                                _win_28 = _binop_138;
                                /* win = win && link((this.boxes)(i), (this.boxes)(i - n - 1)) */;
                                int64_t _binop_148 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_139 = (*_this)._enc__field_boxes;
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_140 = array_get(_fieldacc_139, _i_25).p;
                                                                         ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_141 = (*_this)._enc__field_boxes;
                                                                         int64_t _binop_144 = (({int64_t _binop_142 = (({ _i_25;}) - ({ _enc__arg_n;})); _binop_142;}) - ({int64_t _literal_143 = 1; _literal_143;}));
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_145 = array_get(_fieldacc_141, _binop_144).p;
                                                                         ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                         pony_type_t* _tmp_146[] = {};
                                                                         int64_t _fun_call_147 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_140, _access_145); _fun_call_147;}));
                                _win_28 = _binop_148;
                                _ite_128 = ((void*) UNIT);
                              }
                              else
                              {
                                UNIT;
                                _ite_128 = ((void*) UNIT);
                              };
                              /* win = win && link((this.boxes)(i), (this.boxes)(i - 1)) */;
                              int64_t _binop_157 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                       array_t* _fieldacc_149 = (*_this)._enc__field_boxes;
                                                                       _enc__class__Ped_util_Regions_Box_t* _access_150 = array_get(_fieldacc_149, _i_25).p;
                                                                       ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                       array_t* _fieldacc_151 = (*_this)._enc__field_boxes;
                                                                       int64_t _binop_153 = (({ _i_25;}) - ({int64_t _literal_152 = 1; _literal_152;}));
                                                                       _enc__class__Ped_util_Regions_Box_t* _access_154 = array_get(_fieldacc_151, _binop_153).p;
                                                                       ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                       pony_type_t* _tmp_155[] = {};
                                                                       int64_t _fun_call_156 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_150, _access_154); _fun_call_156;}));
                              _win_28 = _binop_157;
                              /* i = i + 1 */;
                              int64_t _binop_159 = (({ _i_25;}) + ({int64_t _literal_158 = 1; _literal_158;}));
                              _i_25 = _binop_159;
                              /* x = x + dx */;
                              int64_t _binop_160 = (({ _x_20;}) + ({ _dx_10;}));
                              _x_20 = _binop_160;
                              /* y = ymin */;
                              _y_22 = _ymin_6;
                              _for_33 = UNIT;
                              _index_34 = (_index_34 + _step_38);
                            };
                            /* for yindex <- [0..n - 2] do
  (this.boxes)(i) = new Box((xmax, y + dy - 1), (x, y))
  if yindex != 0 then
    win = win && link((this.boxes)(i), (this.boxes)(i - 1))
    win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
  end
  win = win && link((this.boxes)(i), (this.boxes)(i - n + 1))
  win = win && link((this.boxes)(i), (this.boxes)(i - n))
  i = i + 1
  y = y + dy
end */;
                            void* _for_161;
                            /* Range not generated */;
                            int64_t _literal_168 = 0;
                            int64_t _binop_170 = (({ _enc__arg_n;}) - ({int64_t _literal_169 = 2; _literal_169;}));
                            int64_t _literal_171 = 1;
                            int64_t _literal_172 = 1;
                            int64_t _step_166 = (_literal_172 * _literal_171);
                            range_assert_step(_step_166);
                            int64_t _index_162;
                            if ((_step_166 > 0))
                            {
                              _index_162 = _literal_168;
                            }
                            else
                            {
                              _index_162 = _binop_170;
                            };
                            while (((_index_162 >= _literal_168) && (_index_162 <= _binop_170)))
                            {
                              int64_t _yindex_163 = _index_162;
                              /* (this.boxes)(i) = new Box((xmax, y + dy - 1), (x, y)) */;
                              _enc__class__Ped_util_Regions_Box_t* _new_173 = _enc__constructor__Ped_util_Regions_Box(_ctx, NULL);
                              tuple_t* _tuple_174 = tuple_mk(_ctx, 2);
                              tuple_set_type(_tuple_174, 0, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_174, 1, ENCORE_PRIMITIVE);
                              int64_t _binop_177 = (({int64_t _binop_175 = (({ _y_22;}) + ({ _dy_32;})); _binop_175;}) - ({int64_t _literal_176 = 1; _literal_176;}));
                              tuple_set(_tuple_174, 0, ((encore_arg_t) {.i = _xmax_3}));
                              tuple_set(_tuple_174, 1, ((encore_arg_t) {.i = _binop_177}));
                              tuple_t* _tuple_178 = tuple_mk(_ctx, 2);
                              tuple_set_type(_tuple_178, 0, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_178, 1, ENCORE_PRIMITIVE);
                              tuple_set(_tuple_178, 0, ((encore_arg_t) {.i = _x_20}));
                              tuple_set(_tuple_178, 1, ((encore_arg_t) {.i = _y_22}));
                              pony_type_t* _tmp_179[] = {};
                              _enc__type_init__Ped_util_Regions_Box(_new_173);
                              _enc__method__Ped_util_Regions_Box_init_one_way(_ctx, _new_173, NULL, _tuple_174, _tuple_178);
                              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                              array_t* _fieldacc_180 = (*_this)._enc__field_boxes;
                              array_set(_fieldacc_180, _i_25, ((encore_arg_t) {.p = _new_173}));
                              /* if yindex != 0 then
  win = win && link((this.boxes)(i), (this.boxes)(i - 1))
  win = win && link((this.boxes)(i), (this.boxes)(i - n - 1))
end */;
                              void* _ite_181;
                              if (({int64_t _binop_183 = (({ _yindex_163;}) != ({int64_t _literal_182 = 0; _literal_182;})); _binop_183;}))
                              {
                                /* win = win && link((this.boxes)(i), (this.boxes)(i - 1)) */;
                                int64_t _binop_192 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_184 = (*_this)._enc__field_boxes;
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_185 = array_get(_fieldacc_184, _i_25).p;
                                                                         ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_186 = (*_this)._enc__field_boxes;
                                                                         int64_t _binop_188 = (({ _i_25;}) - ({int64_t _literal_187 = 1; _literal_187;}));
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_189 = array_get(_fieldacc_186, _binop_188).p;
                                                                         ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                         pony_type_t* _tmp_190[] = {};
                                                                         int64_t _fun_call_191 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_185, _access_189); _fun_call_191;}));
                                _win_28 = _binop_192;
                                /* win = win && link((this.boxes)(i), (this.boxes)(i - n - 1)) */;
                                int64_t _binop_202 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_193 = (*_this)._enc__field_boxes;
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_194 = array_get(_fieldacc_193, _i_25).p;
                                                                         ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                         array_t* _fieldacc_195 = (*_this)._enc__field_boxes;
                                                                         int64_t _binop_198 = (({int64_t _binop_196 = (({ _i_25;}) - ({ _enc__arg_n;})); _binop_196;}) - ({int64_t _literal_197 = 1; _literal_197;}));
                                                                         _enc__class__Ped_util_Regions_Box_t* _access_199 = array_get(_fieldacc_195, _binop_198).p;
                                                                         ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                         pony_type_t* _tmp_200[] = {};
                                                                         int64_t _fun_call_201 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_194, _access_199); _fun_call_201;}));
                                _win_28 = _binop_202;
                                _ite_181 = ((void*) UNIT);
                              }
                              else
                              {
                                UNIT;
                                _ite_181 = ((void*) UNIT);
                              };
                              /* win = win && link((this.boxes)(i), (this.boxes)(i - n + 1)) */;
                              int64_t _binop_212 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                       array_t* _fieldacc_203 = (*_this)._enc__field_boxes;
                                                                       _enc__class__Ped_util_Regions_Box_t* _access_204 = array_get(_fieldacc_203, _i_25).p;
                                                                       ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                       array_t* _fieldacc_205 = (*_this)._enc__field_boxes;
                                                                       int64_t _binop_208 = (({int64_t _binop_206 = (({ _i_25;}) - ({ _enc__arg_n;})); _binop_206;}) + ({int64_t _literal_207 = 1; _literal_207;}));
                                                                       _enc__class__Ped_util_Regions_Box_t* _access_209 = array_get(_fieldacc_205, _binop_208).p;
                                                                       ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                       pony_type_t* _tmp_210[] = {};
                                                                       int64_t _fun_call_211 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_204, _access_209); _fun_call_211;}));
                              _win_28 = _binop_212;
                              /* win = win && link((this.boxes)(i), (this.boxes)(i - n)) */;
                              int64_t _binop_220 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                       array_t* _fieldacc_213 = (*_this)._enc__field_boxes;
                                                                       _enc__class__Ped_util_Regions_Box_t* _access_214 = array_get(_fieldacc_213, _i_25).p;
                                                                       ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                       array_t* _fieldacc_215 = (*_this)._enc__field_boxes;
                                                                       int64_t _binop_216 = (({ _i_25;}) - ({ _enc__arg_n;}));
                                                                       _enc__class__Ped_util_Regions_Box_t* _access_217 = array_get(_fieldacc_215, _binop_216).p;
                                                                       ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                       pony_type_t* _tmp_218[] = {};
                                                                       int64_t _fun_call_219 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_214, _access_217); _fun_call_219;}));
                              _win_28 = _binop_220;
                              /* i = i + 1 */;
                              int64_t _binop_222 = (({ _i_25;}) + ({int64_t _literal_221 = 1; _literal_221;}));
                              _i_25 = _binop_222;
                              /* y = y + dy */;
                              int64_t _binop_223 = (({ _y_22;}) + ({ _dy_32;}));
                              _y_22 = _binop_223;
                              _for_161 = UNIT;
                              _index_162 = (_index_162 + _step_166);
                            };
                            /* (this.boxes)(i) = new Box((xmax, ymax), (x, y)) */;
                            _enc__class__Ped_util_Regions_Box_t* _new_224 = _enc__constructor__Ped_util_Regions_Box(_ctx, NULL);
                            tuple_t* _tuple_225 = tuple_mk(_ctx, 2);
                            tuple_set_type(_tuple_225, 0, ENCORE_PRIMITIVE);
                            tuple_set_type(_tuple_225, 1, ENCORE_PRIMITIVE);
                            tuple_set(_tuple_225, 0, ((encore_arg_t) {.i = _xmax_3}));
                            tuple_set(_tuple_225, 1, ((encore_arg_t) {.i = _ymax_4}));
                            tuple_t* _tuple_226 = tuple_mk(_ctx, 2);
                            tuple_set_type(_tuple_226, 0, ENCORE_PRIMITIVE);
                            tuple_set_type(_tuple_226, 1, ENCORE_PRIMITIVE);
                            tuple_set(_tuple_226, 0, ((encore_arg_t) {.i = _x_20}));
                            tuple_set(_tuple_226, 1, ((encore_arg_t) {.i = _y_22}));
                            pony_type_t* _tmp_227[] = {};
                            _enc__type_init__Ped_util_Regions_Box(_new_224);
                            _enc__method__Ped_util_Regions_Box_init_one_way(_ctx, _new_224, NULL, _tuple_225, _tuple_226);
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                            array_t* _fieldacc_228 = (*_this)._enc__field_boxes;
                            array_set(_fieldacc_228, _i_25, ((encore_arg_t) {.p = _new_224}));
                            /* win = win && link((this.boxes)(i), (this.boxes)(i - n)) */;
                            int64_t _binop_236 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                     array_t* _fieldacc_229 = (*_this)._enc__field_boxes;
                                                                     _enc__class__Ped_util_Regions_Box_t* _access_230 = array_get(_fieldacc_229, _i_25).p;
                                                                     ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                     array_t* _fieldacc_231 = (*_this)._enc__field_boxes;
                                                                     int64_t _binop_232 = (({ _i_25;}) - ({ _enc__arg_n;}));
                                                                     _enc__class__Ped_util_Regions_Box_t* _access_233 = array_get(_fieldacc_231, _binop_232).p;
                                                                     ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                     pony_type_t* _tmp_234[] = {};
                                                                     int64_t _fun_call_235 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_230, _access_233); _fun_call_235;}));
                            _win_28 = _binop_236;
                            /* win = win && link((this.boxes)(i), (this.boxes)(i - 1)) */;
                            int64_t _binop_245 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                     array_t* _fieldacc_237 = (*_this)._enc__field_boxes;
                                                                     _enc__class__Ped_util_Regions_Box_t* _access_238 = array_get(_fieldacc_237, _i_25).p;
                                                                     ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                     array_t* _fieldacc_239 = (*_this)._enc__field_boxes;
                                                                     int64_t _binop_241 = (({ _i_25;}) - ({int64_t _literal_240 = 1; _literal_240;}));
                                                                     _enc__class__Ped_util_Regions_Box_t* _access_242 = array_get(_fieldacc_239, _binop_241).p;
                                                                     ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                     pony_type_t* _tmp_243[] = {};
                                                                     int64_t _fun_call_244 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_238, _access_242); _fun_call_244;}));
                            _win_28 = _binop_245;
                            /* win = win && link((this.boxes)(i), (this.boxes)(i - n - 1)) */;
                            int64_t _binop_255 = (({ _win_28;}) && ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                     array_t* _fieldacc_246 = (*_this)._enc__field_boxes;
                                                                     _enc__class__Ped_util_Regions_Box_t* _access_247 = array_get(_fieldacc_246, _i_25).p;
                                                                     ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                                                                     array_t* _fieldacc_248 = (*_this)._enc__field_boxes;
                                                                     int64_t _binop_251 = (({int64_t _binop_249 = (({ _i_25;}) - ({ _enc__arg_n;})); _binop_249;}) - ({int64_t _literal_250 = 1; _literal_250;}));
                                                                     _enc__class__Ped_util_Regions_Box_t* _access_252 = array_get(_fieldacc_248, _binop_251).p;
                                                                     ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Regions.link");
                                                                     pony_type_t* _tmp_253[] = {};
                                                                     int64_t _fun_call_254 = _enc__global_fun__Ped_util_Regionslink(_ctx, NULL, _access_247, _access_252); _fun_call_254;}));
                            _win_28 = _binop_255;
                            _ite_16 = ((void*) UNIT);
                          }
                          else
                          {
                            _enc__class__Ped_util_Regions_Box_t* _new_256 = _enc__constructor__Ped_util_Regions_Box(_ctx, NULL);
                            tuple_t* _tuple_257 = tuple_mk(_ctx, 2);
                            tuple_set_type(_tuple_257, 0, ENCORE_PRIMITIVE);
                            tuple_set_type(_tuple_257, 1, ENCORE_PRIMITIVE);
                            tuple_set(_tuple_257, 0, ((encore_arg_t) {.i = _xmax_3}));
                            tuple_set(_tuple_257, 1, ((encore_arg_t) {.i = _ymax_4}));
                            tuple_t* _tuple_258 = tuple_mk(_ctx, 2);
                            tuple_set_type(_tuple_258, 0, ENCORE_PRIMITIVE);
                            tuple_set_type(_tuple_258, 1, ENCORE_PRIMITIVE);
                            tuple_set(_tuple_258, 0, ((encore_arg_t) {.i = _xmin_5}));
                            tuple_set(_tuple_258, 1, ((encore_arg_t) {.i = _ymin_6}));
                            pony_type_t* _tmp_259[] = {};
                            _enc__type_init__Ped_util_Regions_Box(_new_256);
                            _enc__method__Ped_util_Regions_Box_init_one_way(_ctx, _new_256, NULL, _tuple_257, _tuple_258);
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                            array_t* _fieldacc_260 = (*_this)._enc__field_boxes;
                            int64_t _literal_261 = 0;
                            array_set(_fieldacc_260, _literal_261, ((encore_arg_t) {.p = _new_256}));
                            _ite_16 = ((void*) UNIT);
                          };
                          /* for a <- agents do
  for b <- this.boxes do
    if get(b!add(a)) then
      break
    end
  end
end */;
                          void* _for_262;
                          int64_t _start_265 = 0;
                          int64_t _stop_266 = (array_size(_enc__arg_agents) - 1);
                          int64_t _src_step_268 = 1;
                          int64_t _literal_269 = 1;
                          int64_t _step_267 = (_literal_269 * _src_step_268);
                          range_assert_step(_step_267);
                          int64_t _index_263;
                          if ((_step_267 > 0))
                          {
                            _index_263 = _start_265;
                          }
                          else
                          {
                            _index_263 = _stop_266;
                          };
                          while (((_index_263 >= _start_265) && (_index_263 <= _stop_266)))
                          {
                            _enc__class__Ped_util_Agent_passive_Agent_t* _a_264 = array_get(_enc__arg_agents, _index_263).p;
                            void* _for_270;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "boxes");
                            array_t* _fieldacc_277 = (*_this)._enc__field_boxes;
                            int64_t _start_273 = 0;
                            int64_t _stop_274 = (array_size(_fieldacc_277) - 1);
                            int64_t _src_step_276 = 1;
                            int64_t _literal_278 = 1;
                            int64_t _step_275 = (_literal_278 * _src_step_276);
                            range_assert_step(_step_275);
                            int64_t _index_271;
                            if ((_step_275 > 0))
                            {
                              _index_271 = _start_273;
                            }
                            else
                            {
                              _index_271 = _stop_274;
                            };
                            while (((_index_271 >= _start_273) && (_index_271 <= _stop_274)))
                            {
                              _enc__class__Ped_util_Regions_Box_t* _b_272 = array_get(_fieldacc_277, _index_271).p;
                              void* _ite_279;
                              if (({check_receiver(_b_272, " ! ", "b", "add", "\"./Ped_util/Regions.enc\" (line 80, column 21)");
                                    pony_type_t* _tmp_280[] = {};
                                    future_t* _fut_281 = _enc__method__Ped_util_Regions_Box_add_future(_ctx, _b_272, NULL, _a_264);
                                    int64_t _tmp_282 = future_get_actor(_ctx, _fut_281).i; _tmp_282;}))
                              {
                                break;
                                _ite_279 = ((void*) UNIT);
                              }
                              else
                              {
                                UNIT;
                                _ite_279 = ((void*) UNIT);
                              };
                              _for_270 = _ite_279;
                              _index_271 = (_index_271 + _step_275);
                            };
                            _for_262 = _for_270;
                            _index_263 = (_index_263 + _step_267);
                          }; _for_262;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_init_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, array_t* _enc__arg_agents, int64_t _enc__arg_n)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_agents, array_trace);
  /* Not tracing field '_enc__arg_n' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_init_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_init_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_init_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_init));
  msg->f1 = _enc__arg_agents;
  msg->f2 = _enc__arg_n;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Tiling_box_init_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, array_t* _enc__arg_agents, int64_t _enc__arg_n, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_agents, array_trace);
  /* Not tracing field '_enc__arg_n' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Tiling_box_init_t* msg = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_init_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Tiling_box_init_t)), _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_init));
  msg->f1 = _enc__arg_agents;
  msg->f2 = _enc__arg_n;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Tiling_box_init_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Tiling_box_t* _this, pony_type_t** runtimeType, array_t* _enc__arg_agents, int64_t _enc__arg_n)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_agents, array_trace);
  /* Not tracing field '_enc__arg_n' */;
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_init));
  msg->f1 = _enc__arg_agents;
  msg->f2 = _enc__arg_n;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


static void _enc__dispatch__Ped_util_Regions_Tiling_box(pony_ctx_t** _ctx, pony_actor_t* _a, pony_msg_t* _m)
{
  _enc__class__Ped_util_Regions_Tiling_box_t* _this = ((_enc__class__Ped_util_Regions_Tiling_box_t*) _a);
  switch (_m->id)
  {
    case _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_await:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      pony_type_t* _enc__type__t = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_await_t*) _m)->_enc__type__t;
      future_t* _enc__arg_f = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_await_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_f, future_trace);
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {_enc__type__t};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Tiling_box_await(_ctx, _this, methodTypeVars, _enc__arg_f)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_await:
    {
      pony_type_t* _enc__type__t = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t*) _m)->_enc__type__t;
      future_t* _enc__arg_f = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_f, future_trace);
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {_enc__type__t};
      _enc__method__Ped_util_Regions_Tiling_box_await(_ctx, _this, methodTypeVars, _enc__arg_f);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_suspend:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Tiling_box_suspend(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_suspend:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Tiling_box_suspend(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_agents:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Tiling_box_agents(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_agents:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Tiling_box_agents(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_move:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      int64_t _enc__arg_i = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_move_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing field '_enc__arg_i' */;
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Tiling_box_move(_ctx, _this, methodTypeVars, _enc__arg_i)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_move:
    {
      int64_t _enc__arg_i = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing field '_enc__arg_i' */;
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Tiling_box_move(_ctx, _this, methodTypeVars, _enc__arg_i);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_init:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      array_t* _enc__arg_agents = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_init_t*) _m)->f1;
      int64_t _enc__arg_n = ((_enc__fut_msg__Ped_util_Regions_Tiling_box_init_t*) _m)->f2;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_agents, array_trace);
      /* Not tracing field '_enc__arg_n' */;
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Tiling_box_init(_ctx, _this, methodTypeVars, _enc__arg_agents, _enc__arg_n)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_init:
    {
      array_t* _enc__arg_agents = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t*) _m)->f1;
      int64_t _enc__arg_n = ((_enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t*) _m)->f2;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_agents, array_trace);
      /* Not tracing field '_enc__arg_n' */;
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Tiling_box_init(_ctx, _this, methodTypeVars, _enc__arg_agents, _enc__arg_n);
      break;
    }
    default:
    {
      printf("error, got invalid id: %zd", _m->id);
    }
  };
}


pony_type_t _enc__class__Ped_util_Regions_Tiling_box_type = {.id=_ENC__ID__Ped_util_Regions_Tiling_box, .size=sizeof(_enc__class__Ped_util_Regions_Tiling_box_t), .trace=_enc__trace__Ped_util_Regions_Tiling_box, .dispatch=_enc__dispatch__Ped_util_Regions_Tiling_box, .vtable=trait_method_selector};
