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


struct _enc__class__Ped_util_Regions_Box_t
{
  encore_actor_t _enc__actor;
  int64_t _enc__field_moving;
  int64_t _enc__field_size;
  int64_t _enc__field_ttl;
  option_t* _enc__field_newcommers;
  option_t* _enc__field_last;
  option_t* _enc__field_agents;
  _enc__class__Ped_util_Quad_tree_Quad_tree_t* _enc__field_matrix;
  option_t* _enc__field_bottom_right;
  option_t* _enc__field_bottom_left;
  option_t* _enc__field_top_left;
  option_t* _enc__field_top_right;
  option_t* _enc__field_right;
  option_t* _enc__field_left;
  option_t* _enc__field_down;
  option_t* _enc__field_up;
  int64_t _enc__field_ymin;
  int64_t _enc__field_xmin;
  int64_t _enc__field_ymax;
  int64_t _enc__field_xmax;
};


void _enc__type_init__Ped_util_Regions_Box(_enc__class__Ped_util_Regions_Box_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_Regions_Box(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_Regions_Box_t* _this = p;
  int64_t _enc__field_moving = _this->_enc__field_moving;
  /* Not tracing field '_enc__field_moving' */;
  int64_t _enc__field_size = _this->_enc__field_size;
  /* Not tracing field '_enc__field_size' */;
  int64_t _enc__field_ttl = _this->_enc__field_ttl;
  /* Not tracing field '_enc__field_ttl' */;
  option_t* _enc__field_newcommers = _this->_enc__field_newcommers;
  encore_trace_object((*_ctx), _enc__field_newcommers, option_trace);
  option_t* _enc__field_last = _this->_enc__field_last;
  encore_trace_object((*_ctx), _enc__field_last, option_trace);
  option_t* _enc__field_agents = _this->_enc__field_agents;
  encore_trace_object((*_ctx), _enc__field_agents, option_trace);
  _enc__class__Ped_util_Quad_tree_Quad_tree_t* _enc__field_matrix = _this->_enc__field_matrix;
  encore_trace_object((*_ctx), _enc__field_matrix, _enc__trace__Ped_util_Quad_tree_Quad_tree);
  option_t* _enc__field_bottom_right = _this->_enc__field_bottom_right;
  encore_trace_object((*_ctx), _enc__field_bottom_right, option_trace);
  option_t* _enc__field_bottom_left = _this->_enc__field_bottom_left;
  encore_trace_object((*_ctx), _enc__field_bottom_left, option_trace);
  option_t* _enc__field_top_left = _this->_enc__field_top_left;
  encore_trace_object((*_ctx), _enc__field_top_left, option_trace);
  option_t* _enc__field_top_right = _this->_enc__field_top_right;
  encore_trace_object((*_ctx), _enc__field_top_right, option_trace);
  option_t* _enc__field_right = _this->_enc__field_right;
  encore_trace_object((*_ctx), _enc__field_right, option_trace);
  option_t* _enc__field_left = _this->_enc__field_left;
  encore_trace_object((*_ctx), _enc__field_left, option_trace);
  option_t* _enc__field_down = _this->_enc__field_down;
  encore_trace_object((*_ctx), _enc__field_down, option_trace);
  option_t* _enc__field_up = _this->_enc__field_up;
  encore_trace_object((*_ctx), _enc__field_up, option_trace);
  int64_t _enc__field_ymin = _this->_enc__field_ymin;
  /* Not tracing field '_enc__field_ymin' */;
  int64_t _enc__field_xmin = _this->_enc__field_xmin;
  /* Not tracing field '_enc__field_xmin' */;
  int64_t _enc__field_ymax = _this->_enc__field_ymax;
  /* Not tracing field '_enc__field_ymax' */;
  int64_t _enc__field_xmax = _this->_enc__field_xmax;
  /* Not tracing field '_enc__field_xmax' */;
}


_enc__class__Ped_util_Regions_Box_t* _enc__constructor__Ped_util_Regions_Box(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_Regions_Box_t* _this = ((_enc__class__Ped_util_Regions_Box_t*) encore_create((*_ctx), (&(_enc__class__Ped_util_Regions_Box_type))));
  return _this;
}


void* _enc__method__Ped_util_Regions_Box_await(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_await(_ctx, _enc__arg_f);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "await");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Box_await_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_await_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_await_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_await_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f, future_t* _fut)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_await_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_await_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_await_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _enc__arg_f)
{
  pony_type_t* _enc__type__t = (runtimeType[0]);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_f, future_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_await_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_await_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_await_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_await));
  msg->f1 = _enc__arg_f;
  msg->_enc__type__t = _enc__type__t;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__Ped_util_Regions_Box_suspend(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  actor_suspend(_ctx);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "suspend");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Box_suspend_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_suspend_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_suspend_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_suspend_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_suspend_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_suspend_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_suspend));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_suspend_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_suspend_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_suspend_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_suspend_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_suspend));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


int64_t _enc__method__Ped_util_Regions_Box_move(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "move");
  /* this.ttl = this.ttl - 1 */;
  int64_t _binop_2 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ttl");
                        int64_t _fieldacc_0 = (*_this)._enc__field_ttl; _fieldacc_0;}) - ({int64_t _literal_1 = 1; _literal_1;}));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ttl"); _this;}))._enc__field_ttl = _binop_2;
  /* this.merge() */;
  check_receiver(_this, ".", "this", "merge", "\"./Ped_util/Regions.enc\" (line 338, column 5)");
  pony_type_t* _tmp_4[] = {};
  void* _sync_method_call_3 = _enc__method__Ped_util_Regions_Box_merge(_ctx, _this, NULL);
  /* var parent = this.agents */;
  /* parent = this.agents */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  option_t* _fieldacc_5 = (*_this)._enc__field_agents;
  option_t* _parent_7 = _fieldacc_5;
  /* var wrapped_a = this.agents */;
  /* wrapped_a = this.agents */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  option_t* _fieldacc_8 = (*_this)._enc__field_agents;
  option_t* _wrapped_a_10 = _fieldacc_8;
  /* var desired = new [int](6) */;
  /* desired = new [int](6) */;
  int64_t _literal_12 = 6;
  array_t* _array_11 = array_mk(_ctx, _literal_12, ENCORE_PRIMITIVE);
  array_t* _desired_14 = _array_11;
  /* while this.is_something(wrapped_a) do
  var a = match wrapped_a with
            case Just(value) =>
              value
            end
          
          end
  if a.a.ttl == this.ttl then
    a.a.ttl = a.a.ttl - 1
    a.a.next(desired)
    var i = 0
    var moved = false
    var x2 = 0
    var y2 = 0
    while i < |desired| && moved == false do
      val x = desired(i)
      val y = desired(i + 1)
      let
        up = y > this.ymax
        down = y < this.ymin
        right = x > this.xmax
        left = x < this.xmin
        border = if up then
                   if right then
                     this.top_right
                   else
                     if left then
                       this.top_left
                     else
                       this.up
                     end
                   end
                 else
                   if down then
                     if right then
                       this.bottom_right
                     else
                       if left then
                         this.bottom_left
                       else
                         this.down
                       end
                     end
                   else
                     if left then
                       this.left
                     else
                       if right then
                         this.right
                       else
                         Nothing
                       end
                     end
                   end
                 end
      in
        var ax = a.a.x
        var ay = a.a.y
        match border with
          case Nothing =>
            if not(y > this.ymax || y < this.ymin || x > this.xmax || x < this.xmin) && this.matrix.get_val(x, y) == false then
              this.matrix.remove(ax, ay)
              this.matrix.set(x, y)
              a.a.move_int(x, y)
              moved = true
            end
          end
          case Just(real_thing) =>
            x2 = ax
            y2 = ay
            val first = wrapped_a == this.agents
            var nejbor = real_thing!external_move(a.a, x, y)
            await(nejbor)
            val futurefilled = get(nejbor)
            match futurefilled with
              case Just(returned_agent) =>
                a.a = returned_agent
              end
              case Nothing =>
                this.size = this.size - 1
                this.matrix.remove(x2, y2)
                moved = true
                if first then
                  this.agents = a.next
                end
              end
            
            end
          end
        
        end
        i = i + 2
      end
    end
  end
  wrapped_a = a.next
end */;
  void* _while_205;
  while (({check_receiver(_this, ".", "this", "is_something", "\"./Ped_util/Regions.enc\" (line 342, column 11)");
           pony_type_t* _tmp_16[] = {};
           int64_t _sync_method_call_15 = _enc__method__Ped_util_Regions_Box_is_something(_ctx, _this, NULL, _wrapped_a_10); _sync_method_call_15;}))
  {
    /* var a = match wrapped_a with
          case Just(value) =>
            value
          end
        
        end */;
    /* a = match wrapped_a with
  case Just(value) =>
    value
  end

end */;
    _enc__class__Ped_util_Regions_Item_t* _match_17;
    _enc__class__Ped_util_Regions_Item_t* _value_18;
    if ((({int64_t _optionCheck_20;
           _optionCheck_20 = ((JUST == (*_wrapped_a_10).tag) && ({int64_t _varBinding_21;
                                                                  _enc__class__Ped_util_Regions_Item_t* _optionVal_19 = (*_wrapped_a_10).val.p;
                                                                  _value_18 = _optionVal_19;
                                                                  _varBinding_21 = 1; _varBinding_21;})); _optionCheck_20;}) && ({int64_t _literal_22 = 1/*True*/; _literal_22;})))
    {
      _match_17 = ((_enc__class__Ped_util_Regions_Item_t*) ({ _value_18;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
    _enc__class__Ped_util_Regions_Item_t* _a_24 = _match_17;
    /* if a.a.ttl == this.ttl then
  a.a.ttl = a.a.ttl - 1
  a.a.next(desired)
  var i = 0
  var moved = false
  var x2 = 0
  var y2 = 0
  while i < |desired| && moved == false do
    val x = desired(i)
    val y = desired(i + 1)
    let
      up = y > this.ymax
      down = y < this.ymin
      right = x > this.xmax
      left = x < this.xmin
      border = if up then
                 if right then
                   this.top_right
                 else
                   if left then
                     this.top_left
                   else
                     this.up
                   end
                 end
               else
                 if down then
                   if right then
                     this.bottom_right
                   else
                     if left then
                       this.bottom_left
                     else
                       this.down
                     end
                   end
                 else
                   if left then
                     this.left
                   else
                     if right then
                       this.right
                     else
                       Nothing
                     end
                   end
                 end
               end
    in
      var ax = a.a.x
      var ay = a.a.y
      match border with
        case Nothing =>
          if not(y > this.ymax || y < this.ymin || x > this.xmax || x < this.xmin) && this.matrix.get_val(x, y) == false then
            this.matrix.remove(ax, ay)
            this.matrix.set(x, y)
            a.a.move_int(x, y)
            moved = true
          end
        end
        case Just(real_thing) =>
          x2 = ax
          y2 = ay
          val first = wrapped_a == this.agents
          var nejbor = real_thing!external_move(a.a, x, y)
          await(nejbor)
          val futurefilled = get(nejbor)
          match futurefilled with
            case Just(returned_agent) =>
              a.a = returned_agent
            end
            case Nothing =>
              this.size = this.size - 1
              this.matrix.remove(x2, y2)
              moved = true
              if first then
                this.agents = a.next
              end
            end
          
          end
        end
      
      end
      i = i + 2
    end
  end
end */;
    void* _ite_25;
    if (({int64_t _binop_29 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
                                 _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_26 = (*_a_24)._enc__field_a;
                                 ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_26, "ttl");
                                 int64_t _fieldacc_27 = (*_fieldacc_26)._enc__field_ttl; _fieldacc_27;}) == ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ttl");
                                                                                                              int64_t _fieldacc_28 = (*_this)._enc__field_ttl; _fieldacc_28;})); _binop_29;}))
    {
      /* a.a.ttl = a.a.ttl - 1 */;
      int64_t _binop_33 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
                             _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_30 = (*_a_24)._enc__field_a;
                             ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_30, "ttl");
                             int64_t _fieldacc_31 = (*_fieldacc_30)._enc__field_ttl; _fieldacc_31;}) - ({int64_t _literal_32 = 1; _literal_32;}));
      (*({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
          _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_34 = (*_a_24)._enc__field_a;
          ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_34, "ttl"); _fieldacc_34;}))._enc__field_ttl = _binop_33;
      /* a.a.next(desired) */;
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
      _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_36 = (*_a_24)._enc__field_a;
      check_receiver(_fieldacc_36, ".", "a.a", "next", "\"./Ped_util/Regions.enc\" (line 348, column 9)");
      pony_type_t* _tmp_37[] = {};
      void* _sync_method_call_35 = _enc__method__Ped_util_Agent_passive_Agent_next(_ctx, _fieldacc_36, NULL, _desired_14);
      /* var i = 0 */;
      /* i = 0 */;
      int64_t _literal_38 = 0;
      int64_t _i_40 = _literal_38;
      /* var moved = false */;
      /* moved = false */;
      int64_t _literal_41 = 0/*False*/;
      int64_t _moved_43 = _literal_41;
      /* var x2 = 0 */;
      /* x2 = 0 */;
      int64_t _literal_44 = 0;
      int64_t _x2_46 = _literal_44;
      /* var y2 = 0 */;
      /* y2 = 0 */;
      int64_t _literal_47 = 0;
      int64_t _y2_49 = _literal_47;
      /* while i < |desired| && moved == false do
  val x = desired(i)
  val y = desired(i + 1)
  let
    up = y > this.ymax
    down = y < this.ymin
    right = x > this.xmax
    left = x < this.xmin
    border = if up then
               if right then
                 this.top_right
               else
                 if left then
                   this.top_left
                 else
                   this.up
                 end
               end
             else
               if down then
                 if right then
                   this.bottom_right
                 else
                   if left then
                     this.bottom_left
                   else
                     this.down
                   end
                 end
               else
                 if left then
                   this.left
                 else
                   if right then
                     this.right
                   else
                     Nothing
                   end
                 end
               end
             end
  in
    var ax = a.a.x
    var ay = a.a.y
    match border with
      case Nothing =>
        if not(y > this.ymax || y < this.ymin || x > this.xmax || x < this.xmin) && this.matrix.get_val(x, y) == false then
          this.matrix.remove(ax, ay)
          this.matrix.set(x, y)
          a.a.move_int(x, y)
          moved = true
        end
      end
      case Just(real_thing) =>
        x2 = ax
        y2 = ay
        val first = wrapped_a == this.agents
        var nejbor = real_thing!external_move(a.a, x, y)
        await(nejbor)
        val futurefilled = get(nejbor)
        match futurefilled with
          case Just(returned_agent) =>
            a.a = returned_agent
          end
          case Nothing =>
            this.size = this.size - 1
            this.matrix.remove(x2, y2)
            moved = true
            if first then
              this.agents = a.next
            end
          end
        
        end
      end
    
    end
    i = i + 2
  end
end */;
      void* _while_203;
      while (({int64_t _binop_54 = (({int64_t _binop_51 = (({ _i_40;}) < ({int64_t _size_50 = array_size(_desired_14); _size_50;})); _binop_51;}) && ({int64_t _binop_53 = (({ _moved_43;}) == ({int64_t _literal_52 = 0/*False*/; _literal_52;})); _binop_53;})); _binop_54;}))
      {
        /* val x = desired(i) */;
        /* x = desired(i) */;
        int64_t _access_55 = array_get(_desired_14, _i_40).i;
        int64_t _x_57 = _access_55;
        /* val y = desired(i + 1) */;
        /* y = desired(i + 1) */;
        int64_t _binop_59 = (({ _i_40;}) + ({int64_t _literal_58 = 1; _literal_58;}));
        int64_t _access_60 = array_get(_desired_14, _binop_59).i;
        int64_t _y_62 = _access_60;
        /* let
  up = y > this.ymax
  down = y < this.ymin
  right = x > this.xmax
  left = x < this.xmin
  border = if up then
             if right then
               this.top_right
             else
               if left then
                 this.top_left
               else
                 this.up
               end
             end
           else
             if down then
               if right then
                 this.bottom_right
               else
                 if left then
                   this.bottom_left
                 else
                   this.down
                 end
               end
             else
               if left then
                 this.left
               else
                 if right then
                   this.right
                 else
                   Nothing
                 end
               end
             end
           end
in
  var ax = a.a.x
  var ay = a.a.y
  match border with
    case Nothing =>
      if not(y > this.ymax || y < this.ymin || x > this.xmax || x < this.xmin) && this.matrix.get_val(x, y) == false then
        this.matrix.remove(ax, ay)
        this.matrix.set(x, y)
        a.a.move_int(x, y)
        moved = true
      end
    end
    case Just(real_thing) =>
      x2 = ax
      y2 = ay
      val first = wrapped_a == this.agents
      var nejbor = real_thing!external_move(a.a, x, y)
      await(nejbor)
      val futurefilled = get(nejbor)
      match futurefilled with
        case Just(returned_agent) =>
          a.a = returned_agent
        end
        case Nothing =>
          this.size = this.size - 1
          this.matrix.remove(x2, y2)
          moved = true
          if first then
            this.agents = a.next
          end
        end
      
      end
    end
  
  end
  i = i + 2
end */;
        /* up = y > this.ymax */;
        int64_t _binop_64 = (({ _y_62;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
                                             int64_t _fieldacc_63 = (*_this)._enc__field_ymax; _fieldacc_63;}));
        int64_t _up_66 = _binop_64;
        /* down = y < this.ymin */;
        int64_t _binop_68 = (({ _y_62;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
                                             int64_t _fieldacc_67 = (*_this)._enc__field_ymin; _fieldacc_67;}));
        int64_t _down_70 = _binop_68;
        /* right = x > this.xmax */;
        int64_t _binop_72 = (({ _x_57;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
                                             int64_t _fieldacc_71 = (*_this)._enc__field_xmax; _fieldacc_71;}));
        int64_t _right_74 = _binop_72;
        /* left = x < this.xmin */;
        int64_t _binop_76 = (({ _x_57;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
                                             int64_t _fieldacc_75 = (*_this)._enc__field_xmin; _fieldacc_75;}));
        int64_t _left_78 = _binop_76;
        /* border = if up then
  if right then
    this.top_right
  else
    if left then
      this.top_left
    else
      this.up
    end
  end
else
  if down then
    if right then
      this.bottom_right
    else
      if left then
        this.bottom_left
      else
        this.down
      end
    end
  else
    if left then
      this.left
    else
      if right then
        this.right
      else
        Nothing
      end
    end
  end
end */;
        option_t* _ite_79;
        if (({ _up_66;}))
        {
          option_t* _ite_80;
          if (({ _right_74;}))
          {
            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right");
            option_t* _fieldacc_81 = (*_this)._enc__field_top_right;
            _ite_80 = ((option_t*) _fieldacc_81);
          }
          else
          {
            option_t* _ite_82;
            if (({ _left_78;}))
            {
              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left");
              option_t* _fieldacc_83 = (*_this)._enc__field_top_left;
              _ite_82 = ((option_t*) _fieldacc_83);
            }
            else
            {
              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "up");
              option_t* _fieldacc_84 = (*_this)._enc__field_up;
              _ite_82 = ((option_t*) _fieldacc_84);
            };
            _ite_80 = ((option_t*) _ite_82);
          };
          _ite_79 = ((option_t*) _ite_80);
        }
        else
        {
          option_t* _ite_85;
          if (({ _down_70;}))
          {
            option_t* _ite_86;
            if (({ _right_74;}))
            {
              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right");
              option_t* _fieldacc_87 = (*_this)._enc__field_bottom_right;
              _ite_86 = ((option_t*) _fieldacc_87);
            }
            else
            {
              option_t* _ite_88;
              if (({ _left_78;}))
              {
                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left");
                option_t* _fieldacc_89 = (*_this)._enc__field_bottom_left;
                _ite_88 = ((option_t*) _fieldacc_89);
              }
              else
              {
                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "down");
                option_t* _fieldacc_90 = (*_this)._enc__field_down;
                _ite_88 = ((option_t*) _fieldacc_90);
              };
              _ite_86 = ((option_t*) _ite_88);
            };
            _ite_85 = ((option_t*) _ite_86);
          }
          else
          {
            option_t* _ite_91;
            if (({ _left_78;}))
            {
              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "left");
              option_t* _fieldacc_92 = (*_this)._enc__field_left;
              _ite_91 = ((option_t*) _fieldacc_92);
            }
            else
            {
              option_t* _ite_93;
              if (({ _right_74;}))
              {
                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "right");
                option_t* _fieldacc_94 = (*_this)._enc__field_right;
                _ite_93 = ((option_t*) _fieldacc_94);
              }
              else
              {
                option_t* _option_95 = (&(DEFAULT_NOTHING));
                _ite_93 = ((option_t*) _option_95);
              };
              _ite_91 = ((option_t*) _ite_93);
            };
            _ite_85 = ((option_t*) _ite_91);
          };
          _ite_79 = ((option_t*) _ite_85);
        };
        option_t* _border_97 = _ite_79;
        /* var ax = a.a.x */;
        /* ax = a.a.x */;
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
        _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_98 = (*_a_24)._enc__field_a;
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_98, "x");
        int64_t _fieldacc_99 = (*_fieldacc_98)._enc__field_x;
        int64_t _ax_101 = _fieldacc_99;
        /* var ay = a.a.y */;
        /* ay = a.a.y */;
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
        _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_102 = (*_a_24)._enc__field_a;
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_102, "y");
        int64_t _fieldacc_103 = (*_fieldacc_102)._enc__field_y;
        int64_t _ay_105 = _fieldacc_103;
        /* match border with
  case Nothing =>
    if not(y > this.ymax || y < this.ymin || x > this.xmax || x < this.xmin) && this.matrix.get_val(x, y) == false then
      this.matrix.remove(ax, ay)
      this.matrix.set(x, y)
      a.a.move_int(x, y)
      moved = true
    end
  end
  case Just(real_thing) =>
    x2 = ax
    y2 = ay
    val first = wrapped_a == this.agents
    var nejbor = real_thing!external_move(a.a, x, y)
    await(nejbor)
    val futurefilled = get(nejbor)
    match futurefilled with
      case Just(returned_agent) =>
        a.a = returned_agent
      end
      case Nothing =>
        this.size = this.size - 1
        this.matrix.remove(x2, y2)
        moved = true
        if first then
          this.agents = a.next
        end
      end
    
    end
  end

end */;
        void* _match_106;
        if ((({int64_t _valueCheck_198;
               _valueCheck_198 = (({option_t* _option_199 = (&(DEFAULT_NOTHING)); _option_199;}) == _border_97); _valueCheck_198;}) && ({int64_t _literal_200 = 1/*True*/; _literal_200;})))
        {
          _match_106 = ((void*) ({void* _ite_107;
                                  if (({int64_t _binop_125 = (({int64_t _binop_118 = (({int64_t _binop_115 = (({int64_t _binop_112 = (({int64_t _binop_109 = (({ _y_62;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
                                                                                                                                                                              int64_t _fieldacc_108 = (*_this)._enc__field_ymax; _fieldacc_108;})); _binop_109;}) || ({int64_t _binop_111 = (({ _y_62;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
                                                                                                                                                                                                                                                                                                             int64_t _fieldacc_110 = (*_this)._enc__field_ymin; _fieldacc_110;})); _binop_111;})); _binop_112;}) || ({int64_t _binop_114 = (({ _x_57;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
                                                                                                                                                                                                                                                                                                                                                                                                                                                            int64_t _fieldacc_113 = (*_this)._enc__field_xmax; _fieldacc_113;})); _binop_114;})); _binop_115;}) || ({int64_t _binop_117 = (({ _x_57;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           int64_t _fieldacc_116 = (*_this)._enc__field_xmin; _fieldacc_116;})); _binop_117;}));
                                                                int64_t _unary_119 = (! _binop_118); _unary_119;}) && ({int64_t _binop_124 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
                                                                                                                                                _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_121 = (*_this)._enc__field_matrix;
                                                                                                                                                check_receiver(_fieldacc_121, ".", "this.matrix", "get_val", "\"./Ped_util/Regions.enc\" (line 399, column 95)");
                                                                                                                                                pony_type_t* _tmp_122[] = {};
                                                                                                                                                int64_t _sync_method_call_120 = _enc__method__Ped_util_Quad_tree_Quad_tree_get_val(_ctx, _fieldacc_121, NULL, _x_57, _y_62); _sync_method_call_120;}) == ({int64_t _literal_123 = 0/*False*/; _literal_123;})); _binop_124;})); _binop_125;}))
                                  {
                                    /* this.matrix.remove(ax, ay) */;
                                    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
                                    _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_127 = (*_this)._enc__field_matrix;
                                    check_receiver(_fieldacc_127, ".", "this.matrix", "remove", "\"./Ped_util/Regions.enc\" (line 400, column 21)");
                                    pony_type_t* _tmp_128[] = {};
                                    int64_t _sync_method_call_126 = _enc__method__Ped_util_Quad_tree_Quad_tree_remove(_ctx, _fieldacc_127, NULL, _ax_101, _ay_105);
                                    /* this.matrix.set(x, y) */;
                                    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
                                    _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_130 = (*_this)._enc__field_matrix;
                                    check_receiver(_fieldacc_130, ".", "this.matrix", "set", "\"./Ped_util/Regions.enc\" (line 401, column 21)");
                                    pony_type_t* _tmp_131[] = {};
                                    void* _sync_method_call_129 = _enc__method__Ped_util_Quad_tree_Quad_tree_set(_ctx, _fieldacc_130, NULL, _x_57, _y_62);
                                    /* a.a.move_int(x, y) */;
                                    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
                                    _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_133 = (*_a_24)._enc__field_a;
                                    check_receiver(_fieldacc_133, ".", "a.a", "move_int", "\"./Ped_util/Regions.enc\" (line 402, column 21)");
                                    pony_type_t* _tmp_134[] = {};
                                    void* _sync_method_call_132 = _enc__method__Ped_util_Agent_passive_Agent_move_int(_ctx, _fieldacc_133, NULL, _x_57, _y_62);
                                    /* moved = true */;
                                    int64_t _literal_135 = 1/*True*/;
                                    _moved_43 = _literal_135;
                                    _ite_107 = ((void*) UNIT);
                                  }
                                  else
                                  {
                                    UNIT;
                                    _ite_107 = ((void*) UNIT);
                                  }; _ite_107;}));
        }
        else
        {
          _enc__class__Ped_util_Regions_Box_t* _real_thing_136;
          if ((({int64_t _optionCheck_195;
                 _optionCheck_195 = ((JUST == (*_border_97).tag) && ({int64_t _varBinding_196;
                                                                      _enc__class__Ped_util_Regions_Box_t* _optionVal_194 = (*_border_97).val.p;
                                                                      _real_thing_136 = _optionVal_194;
                                                                      _varBinding_196 = 1; _varBinding_196;})); _optionCheck_195;}) && ({int64_t _literal_197 = 1/*True*/; _literal_197;})))
          {
            _match_106 = ((void*) ({/* x2 = ax */;
                                    _x2_46 = _ax_101;
                                    /* y2 = ay */;
                                    _y2_49 = _ay_105;
                                    /* val first = wrapped_a == this.agents */;
                                    /* first = wrapped_a == this.agents */;
                                    tuple_t* _tuple_138 = tuple_mk(_ctx, 2);
                                    tuple_set_type(_tuple_138, 0, (&(option_type)));
                                    tuple_set_type(_tuple_138, 1, (&(option_type)));
                                    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
                                    option_t* _fieldacc_139 = (*_this)._enc__field_agents;
                                    tuple_set(_tuple_138, 0, ((encore_arg_t) {.p = _wrapped_a_10}));
                                    tuple_set(_tuple_138, 1, ((encore_arg_t) {.p = _fieldacc_139}));
                                    int64_t _match_137;
                                    _enc__class__Ped_util_Regions_Item_t* __fst_140;
                                    _enc__class__Ped_util_Regions_Item_t* __snd_141;
                                    if ((({int64_t _tupleCheck_156;
                                           _tupleCheck_156 = 1;
                                           option_t* _tupleAccess_157 = tuple_get(_tuple_138, 0).p;
                                           int64_t _optionCheck_159;
                                           _optionCheck_159 = ((JUST == (*_tupleAccess_157).tag) && ({int64_t _varBinding_160;
                                                                                                      _enc__class__Ped_util_Regions_Item_t* _optionVal_158 = (*_tupleAccess_157).val.p;
                                                                                                      __fst_140 = _optionVal_158;
                                                                                                      _varBinding_160 = 1; _varBinding_160;}));
                                           _tupleCheck_156 = (_tupleCheck_156 && _optionCheck_159);
                                           option_t* _tupleAccess_161 = tuple_get(_tuple_138, 1).p;
                                           int64_t _optionCheck_163;
                                           _optionCheck_163 = ((JUST == (*_tupleAccess_161).tag) && ({int64_t _varBinding_164;
                                                                                                      _enc__class__Ped_util_Regions_Item_t* _optionVal_162 = (*_tupleAccess_161).val.p;
                                                                                                      __snd_141 = _optionVal_162;
                                                                                                      _varBinding_164 = 1; _varBinding_164;}));
                                           _tupleCheck_156 = (_tupleCheck_156 && _optionCheck_163); _tupleCheck_156;}) && ({int64_t _binop_165 = (({ __fst_140;}) == ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_141;}))); _binop_165;})))
                                    {
                                      _match_137 = ((int64_t) ({int64_t _literal_142 = 1/*True*/; _literal_142;}));
                                    }
                                    else
                                    {
                                      if ((({int64_t _tupleCheck_148;
                                             _tupleCheck_148 = 1;
                                             option_t* _tupleAccess_149 = tuple_get(_tuple_138, 0).p;
                                             int64_t _valueCheck_150;
                                             _valueCheck_150 = (({option_t* _option_151 = (&(DEFAULT_NOTHING)); _option_151;}) == _tupleAccess_149);
                                             _tupleCheck_148 = (_tupleCheck_148 && _valueCheck_150);
                                             option_t* _tupleAccess_152 = tuple_get(_tuple_138, 1).p;
                                             int64_t _valueCheck_153;
                                             _valueCheck_153 = (({option_t* _option_154 = (&(DEFAULT_NOTHING)); _option_154;}) == _tupleAccess_152);
                                             _tupleCheck_148 = (_tupleCheck_148 && _valueCheck_153); _tupleCheck_148;}) && ({int64_t _literal_155 = 1/*True*/; _literal_155;})))
                                      {
                                        _match_137 = ((int64_t) ({int64_t _literal_143 = 1/*True*/; _literal_143;}));
                                      }
                                      else
                                      {
                                        tuple_t* ___144;
                                        if ((({int64_t _varBinding_146;
                                               ___144 = _tuple_138;
                                               _varBinding_146 = 1; _varBinding_146;}) && ({int64_t _literal_147 = 1/*True*/; _literal_147;})))
                                        {
                                          _match_137 = ((int64_t) ({int64_t _literal_145 = 0/*False*/; _literal_145;}));
                                        }
                                        else
                                        {
                                          fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                          exit(1);
                                        };
                                      };
                                    };
                                    int64_t _first_167 = _match_137;
                                    /* var nejbor = real_thing!external_move(a.a, x, y) */;
                                    /* nejbor = real_thing!external_move(a.a, x, y) */;
                                    check_receiver(_real_thing_136, " ! ", "real_thing", "external_move", "\"./Ped_util/Regions.enc\" (line 410, column 45)");
                                    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a");
                                    _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_168 = (*_a_24)._enc__field_a;
                                    pony_type_t* _tmp_169[] = {};
                                    future_t* _fut_170 = _enc__method__Ped_util_Regions_Box_external_move_future(_ctx, _real_thing_136, NULL, _fieldacc_168, _x_57, _y_62);
                                    future_t* _nejbor_172 = _fut_170;
                                    /* await(nejbor) */;
                                    future_await(_ctx, _nejbor_172);
                                    /* val futurefilled = get(nejbor) */;
                                    /* futurefilled = get(nejbor) */;
                                    option_t* _tmp_173 = future_get_actor(_ctx, _nejbor_172).p;
                                    option_t* _futurefilled_175 = _tmp_173;
                                    /* match futurefilled with
  case Just(returned_agent) =>
    a.a = returned_agent
  end
  case Nothing =>
    this.size = this.size - 1
    this.matrix.remove(x2, y2)
    moved = true
    if first then
      this.agents = a.next
    end
  end

end */;
                                    void* _match_176;
                                    _enc__class__Ped_util_Agent_passive_Agent_t* _returned_agent_177;
                                    if ((({int64_t _optionCheck_191;
                                           _optionCheck_191 = ((JUST == (*_futurefilled_175).tag) && ({int64_t _varBinding_192;
                                                                                                       _enc__class__Ped_util_Agent_passive_Agent_t* _optionVal_190 = (*_futurefilled_175).val.p;
                                                                                                       _returned_agent_177 = _optionVal_190;
                                                                                                       _varBinding_192 = 1; _varBinding_192;})); _optionCheck_191;}) && ({int64_t _literal_193 = 1/*True*/; _literal_193;})))
                                    {
                                      _match_176 = ((void*) ({(*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_a_24, "a"); _a_24;}))._enc__field_a = _returned_agent_177; UNIT;}));
                                    }
                                    else
                                    {
                                      if ((({int64_t _valueCheck_187;
                                             _valueCheck_187 = (({option_t* _option_188 = (&(DEFAULT_NOTHING)); _option_188;}) == _futurefilled_175); _valueCheck_187;}) && ({int64_t _literal_189 = 1/*True*/; _literal_189;})))
                                      {
                                        _match_176 = ((void*) ({/* this.size = this.size - 1 */;
                                                                int64_t _binop_180 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
                                                                                        int64_t _fieldacc_178 = (*_this)._enc__field_size; _fieldacc_178;}) - ({int64_t _literal_179 = 1; _literal_179;}));
                                                                (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _binop_180;
                                                                /* this.matrix.remove(x2, y2) */;
                                                                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
                                                                _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_182 = (*_this)._enc__field_matrix;
                                                                check_receiver(_fieldacc_182, ".", "this.matrix", "remove", "\"./Ped_util/Regions.enc\" (line 417, column 31)");
                                                                pony_type_t* _tmp_183[] = {};
                                                                int64_t _sync_method_call_181 = _enc__method__Ped_util_Quad_tree_Quad_tree_remove(_ctx, _fieldacc_182, NULL, _x2_46, _y2_49);
                                                                /* moved = true */;
                                                                int64_t _literal_184 = 1/*True*/;
                                                                _moved_43 = _literal_184;
                                                                /* if first then
  this.agents = a.next
end */;
                                                                void* _ite_185;
                                                                if (({ _first_167;}))
                                                                {
                                                                  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "next");
                                                                  option_t* _fieldacc_186 = (*_a_24)._enc__field_next;
                                                                  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agents"); _this;}))._enc__field_agents = _fieldacc_186;
                                                                  _ite_185 = ((void*) UNIT);
                                                                }
                                                                else
                                                                {
                                                                  UNIT;
                                                                  _ite_185 = ((void*) UNIT);
                                                                }; _ite_185;}));
                                      }
                                      else
                                      {
                                        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                        exit(1);
                                      };
                                    }; _match_176;}));
          }
          else
          {
            fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
            exit(1);
          };
        };
        /* i = i + 2 */;
        int64_t _binop_202 = (({ _i_40;}) + ({int64_t _literal_201 = 2; _literal_201;}));
        _i_40 = _binop_202;
        _while_203 = UNIT;
      };
      _ite_25 = ((void*) _while_203);
    }
    else
    {
      UNIT;
      _ite_25 = ((void*) UNIT);
    };
    /* wrapped_a = a.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_24, "next");
    option_t* _fieldacc_204 = (*_a_24)._enc__field_next;
    _wrapped_a_10 = _fieldacc_204;
    _while_205 = UNIT;
  };
  /* this.moving = false */;
  int64_t _literal_206 = 0/*False*/;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "moving"); _this;}))._enc__field_moving = _literal_206;
  /* true */;
  int64_t _literal_207 = 1/*True*/;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "move");
  return ((int64_t) _literal_207);
}


future_t* _enc__method__Ped_util_Regions_Box_move_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_move_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_move_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_move));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_move_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_move_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_move_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_move));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_move_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_move_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_move_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_move));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


int64_t _enc__method__Ped_util_Regions_Box_is_something(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, option_t* _enc__arg_a)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "is_something");
  int64_t _match_0;
  if ((({int64_t _valueCheck_6;
         _valueCheck_6 = (({option_t* _option_7 = (&(DEFAULT_NOTHING)); _option_7;}) == _enc__arg_a); _valueCheck_6;}) && ({int64_t _literal_8 = 1/*True*/; _literal_8;})))
  {
    _match_0 = ((int64_t) ({int64_t _literal_1 = 0/*False*/; _literal_1;}));
  }
  else
  {
    option_t* _unused_2;
    if ((({int64_t _varBinding_4;
           _unused_2 = _enc__arg_a;
           _varBinding_4 = 1; _varBinding_4;}) && ({int64_t _literal_5 = 1/*True*/; _literal_5;})))
    {
      _match_0 = ((int64_t) ({int64_t _literal_3 = 1/*True*/; _literal_3;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "is_something");
  return ((int64_t) _match_0);
}


future_t* _enc__method__Ped_util_Regions_Box_is_something_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, option_t* _enc__arg_a)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, option_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_is_something_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_is_something_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_is_something_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_is_something));
  msg->f1 = _enc__arg_a;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_is_something_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, option_t* _enc__arg_a, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, option_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_is_something_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_is_something_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_is_something_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_is_something));
  msg->f1 = _enc__arg_a;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_is_something_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, option_t* _enc__arg_a)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, option_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_is_something_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_is_something_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_is_something_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_is_something));
  msg->f1 = _enc__arg_a;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__Ped_util_Regions_Box_merge(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "merge");
  void* _ite_0;
  if (({tuple_t* _tuple_2 = tuple_mk(_ctx, 2);
        tuple_set_type(_tuple_2, 0, (&(option_type)));
        tuple_set_type(_tuple_2, 1, (&(option_type)));
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers");
        option_t* _fieldacc_3 = (*_this)._enc__field_newcommers;
        option_t* _option_4 = (&(DEFAULT_NOTHING));
        tuple_set(_tuple_2, 0, ((encore_arg_t) {.p = _fieldacc_3}));
        tuple_set(_tuple_2, 1, ((encore_arg_t) {.p = _option_4}));
        int64_t _match_1;
        _enc__class__Ped_util_Regions_Item_t* __fst_5;
        _enc__class__Ped_util_Regions_Item_t* __snd_6;
        if ((({int64_t _tupleCheck_21;
               _tupleCheck_21 = 1;
               option_t* _tupleAccess_22 = tuple_get(_tuple_2, 0).p;
               int64_t _optionCheck_24;
               _optionCheck_24 = ((JUST == (*_tupleAccess_22).tag) && ({int64_t _varBinding_25;
                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_23 = (*_tupleAccess_22).val.p;
                                                                        __fst_5 = _optionVal_23;
                                                                        _varBinding_25 = 1; _varBinding_25;}));
               _tupleCheck_21 = (_tupleCheck_21 && _optionCheck_24);
               option_t* _tupleAccess_26 = tuple_get(_tuple_2, 1).p;
               int64_t _optionCheck_28;
               _optionCheck_28 = ((JUST == (*_tupleAccess_26).tag) && ({int64_t _varBinding_29;
                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_27 = (*_tupleAccess_26).val.p;
                                                                        __snd_6 = _optionVal_27;
                                                                        _varBinding_29 = 1; _varBinding_29;}));
               _tupleCheck_21 = (_tupleCheck_21 && _optionCheck_28); _tupleCheck_21;}) && ({int64_t _binop_30 = (({ __fst_5;}) != ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_6;}))); _binop_30;})))
        {
          _match_1 = ((int64_t) ({int64_t _literal_7 = 1/*True*/; _literal_7;}));
        }
        else
        {
          if ((({int64_t _tupleCheck_13;
                 _tupleCheck_13 = 1;
                 option_t* _tupleAccess_14 = tuple_get(_tuple_2, 0).p;
                 int64_t _valueCheck_15;
                 _valueCheck_15 = (({option_t* _option_16 = (&(DEFAULT_NOTHING)); _option_16;}) == _tupleAccess_14);
                 _tupleCheck_13 = (_tupleCheck_13 && _valueCheck_15);
                 option_t* _tupleAccess_17 = tuple_get(_tuple_2, 1).p;
                 int64_t _valueCheck_18;
                 _valueCheck_18 = (({option_t* _option_19 = (&(DEFAULT_NOTHING)); _option_19;}) == _tupleAccess_17);
                 _tupleCheck_13 = (_tupleCheck_13 && _valueCheck_18); _tupleCheck_13;}) && ({int64_t _literal_20 = 1/*True*/; _literal_20;})))
          {
            _match_1 = ((int64_t) ({int64_t _literal_8 = 1/*True*/; _literal_8;}));
          }
          else
          {
            tuple_t* ___9;
            if ((({int64_t _varBinding_11;
                   ___9 = _tuple_2;
                   _varBinding_11 = 1; _varBinding_11;}) && ({int64_t _literal_12 = 1/*True*/; _literal_12;})))
            {
              _match_1 = ((int64_t) ({int64_t _literal_10 = 0/*False*/; _literal_10;}));
            }
            else
            {
              fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
              exit(1);
            };
          };
        };
        int64_t _unary_31 = (! _match_1); _unary_31;}))
  {
    /* if this.agents != Nothing then
  match (this.newcommers, this.last) with
    case (unused, Just(last)) =>
      last.append(this.agents)
    end
    case (Just(newc), unused) =>
      newc.append(this.agents)
    end
  
  end
  this.agents = this.newcommers
else
  this.agents = this.newcommers
end */;
    void* _ite_32;
    if (({tuple_t* _tuple_34 = tuple_mk(_ctx, 2);
          tuple_set_type(_tuple_34, 0, (&(option_type)));
          tuple_set_type(_tuple_34, 1, (&(option_type)));
          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
          option_t* _fieldacc_35 = (*_this)._enc__field_agents;
          option_t* _option_36 = (&(DEFAULT_NOTHING));
          tuple_set(_tuple_34, 0, ((encore_arg_t) {.p = _fieldacc_35}));
          tuple_set(_tuple_34, 1, ((encore_arg_t) {.p = _option_36}));
          int64_t _match_33;
          _enc__class__Ped_util_Regions_Item_t* __fst_37;
          _enc__class__Ped_util_Regions_Item_t* __snd_38;
          if ((({int64_t _tupleCheck_53;
                 _tupleCheck_53 = 1;
                 option_t* _tupleAccess_54 = tuple_get(_tuple_34, 0).p;
                 int64_t _optionCheck_56;
                 _optionCheck_56 = ((JUST == (*_tupleAccess_54).tag) && ({int64_t _varBinding_57;
                                                                          _enc__class__Ped_util_Regions_Item_t* _optionVal_55 = (*_tupleAccess_54).val.p;
                                                                          __fst_37 = _optionVal_55;
                                                                          _varBinding_57 = 1; _varBinding_57;}));
                 _tupleCheck_53 = (_tupleCheck_53 && _optionCheck_56);
                 option_t* _tupleAccess_58 = tuple_get(_tuple_34, 1).p;
                 int64_t _optionCheck_60;
                 _optionCheck_60 = ((JUST == (*_tupleAccess_58).tag) && ({int64_t _varBinding_61;
                                                                          _enc__class__Ped_util_Regions_Item_t* _optionVal_59 = (*_tupleAccess_58).val.p;
                                                                          __snd_38 = _optionVal_59;
                                                                          _varBinding_61 = 1; _varBinding_61;}));
                 _tupleCheck_53 = (_tupleCheck_53 && _optionCheck_60); _tupleCheck_53;}) && ({int64_t _binop_62 = (({ __fst_37;}) != ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_38;}))); _binop_62;})))
          {
            _match_33 = ((int64_t) ({int64_t _literal_39 = 1/*True*/; _literal_39;}));
          }
          else
          {
            if ((({int64_t _tupleCheck_45;
                   _tupleCheck_45 = 1;
                   option_t* _tupleAccess_46 = tuple_get(_tuple_34, 0).p;
                   int64_t _valueCheck_47;
                   _valueCheck_47 = (({option_t* _option_48 = (&(DEFAULT_NOTHING)); _option_48;}) == _tupleAccess_46);
                   _tupleCheck_45 = (_tupleCheck_45 && _valueCheck_47);
                   option_t* _tupleAccess_49 = tuple_get(_tuple_34, 1).p;
                   int64_t _valueCheck_50;
                   _valueCheck_50 = (({option_t* _option_51 = (&(DEFAULT_NOTHING)); _option_51;}) == _tupleAccess_49);
                   _tupleCheck_45 = (_tupleCheck_45 && _valueCheck_50); _tupleCheck_45;}) && ({int64_t _literal_52 = 1/*True*/; _literal_52;})))
            {
              _match_33 = ((int64_t) ({int64_t _literal_40 = 1/*True*/; _literal_40;}));
            }
            else
            {
              tuple_t* ___41;
              if ((({int64_t _varBinding_43;
                     ___41 = _tuple_34;
                     _varBinding_43 = 1; _varBinding_43;}) && ({int64_t _literal_44 = 1/*True*/; _literal_44;})))
              {
                _match_33 = ((int64_t) ({int64_t _literal_42 = 0/*False*/; _literal_42;}));
              }
              else
              {
                fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                exit(1);
              };
            };
          };
          int64_t _unary_63 = (! _match_33); _unary_63;}))
    {
      /* match (this.newcommers, this.last) with
  case (unused, Just(last)) =>
    last.append(this.agents)
  end
  case (Just(newc), unused) =>
    newc.append(this.agents)
  end

end */;
      tuple_t* _tuple_65 = tuple_mk(_ctx, 2);
      tuple_set_type(_tuple_65, 0, (&(option_type)));
      tuple_set_type(_tuple_65, 1, (&(option_type)));
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers");
      option_t* _fieldacc_66 = (*_this)._enc__field_newcommers;
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "last");
      option_t* _fieldacc_67 = (*_this)._enc__field_last;
      tuple_set(_tuple_65, 0, ((encore_arg_t) {.p = _fieldacc_66}));
      tuple_set(_tuple_65, 1, ((encore_arg_t) {.p = _fieldacc_67}));
      void* _match_64;
      option_t* _unused_68;
      _enc__class__Ped_util_Regions_Item_t* _last_69;
      if ((({int64_t _tupleCheck_86;
             _tupleCheck_86 = 1;
             option_t* _tupleAccess_87 = tuple_get(_tuple_65, 0).p;
             int64_t _varBinding_88;
             _unused_68 = _tupleAccess_87;
             _varBinding_88 = 1;
             _tupleCheck_86 = (_tupleCheck_86 && _varBinding_88);
             option_t* _tupleAccess_89 = tuple_get(_tuple_65, 1).p;
             int64_t _optionCheck_91;
             _optionCheck_91 = ((JUST == (*_tupleAccess_89).tag) && ({int64_t _varBinding_92;
                                                                      _enc__class__Ped_util_Regions_Item_t* _optionVal_90 = (*_tupleAccess_89).val.p;
                                                                      _last_69 = _optionVal_90;
                                                                      _varBinding_92 = 1; _varBinding_92;}));
             _tupleCheck_86 = (_tupleCheck_86 && _optionCheck_91); _tupleCheck_86;}) && ({int64_t _literal_93 = 1/*True*/; _literal_93;})))
      {
        _match_64 = ((void*) ({check_receiver(_last_69, ".", "last", "append", "\"./Ped_util/Regions.enc\" (line 319, column 41)");
                               ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
                               option_t* _fieldacc_71 = (*_this)._enc__field_agents;
                               pony_type_t* _tmp_72[] = {};
                               void* _sync_method_call_70 = _enc__method__Ped_util_Regions_Item_append(_ctx, _last_69, NULL, _fieldacc_71); _sync_method_call_70;}));
      }
      else
      {
        _enc__class__Ped_util_Regions_Item_t* _newc_73;
        option_t* _unused_74;
        if ((({int64_t _tupleCheck_78;
               _tupleCheck_78 = 1;
               option_t* _tupleAccess_79 = tuple_get(_tuple_65, 0).p;
               int64_t _optionCheck_81;
               _optionCheck_81 = ((JUST == (*_tupleAccess_79).tag) && ({int64_t _varBinding_82;
                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_80 = (*_tupleAccess_79).val.p;
                                                                        _newc_73 = _optionVal_80;
                                                                        _varBinding_82 = 1; _varBinding_82;}));
               _tupleCheck_78 = (_tupleCheck_78 && _optionCheck_81);
               option_t* _tupleAccess_83 = tuple_get(_tuple_65, 1).p;
               int64_t _varBinding_84;
               _unused_74 = _tupleAccess_83;
               _varBinding_84 = 1;
               _tupleCheck_78 = (_tupleCheck_78 && _varBinding_84); _tupleCheck_78;}) && ({int64_t _literal_85 = 1/*True*/; _literal_85;})))
        {
          _match_64 = ((void*) ({check_receiver(_newc_73, ".", "newc", "append", "\"./Ped_util/Regions.enc\" (line 320, column 41)");
                                 ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
                                 option_t* _fieldacc_76 = (*_this)._enc__field_agents;
                                 pony_type_t* _tmp_77[] = {};
                                 void* _sync_method_call_75 = _enc__method__Ped_util_Regions_Item_append(_ctx, _newc_73, NULL, _fieldacc_76); _sync_method_call_75;}));
        }
        else
        {
          fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
          exit(1);
        };
      };
      /* this.agents = this.newcommers */;
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers");
      option_t* _fieldacc_94 = (*_this)._enc__field_newcommers;
      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agents"); _this;}))._enc__field_agents = _fieldacc_94;
      _ite_32 = ((void*) UNIT);
    }
    else
    {
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers");
      option_t* _fieldacc_95 = (*_this)._enc__field_newcommers;
      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agents"); _this;}))._enc__field_agents = _fieldacc_95;
      _ite_32 = ((void*) UNIT);
    };
    /* this.newcommers = Nothing */;
    option_t* _option_96 = (&(DEFAULT_NOTHING));
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers"); _this;}))._enc__field_newcommers = _option_96;
    /* this.last = Nothing */;
    option_t* _option_97 = (&(DEFAULT_NOTHING));
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "last"); _this;}))._enc__field_last = _option_97;
    _ite_0 = ((void*) UNIT);
  }
  else
  {
    UNIT;
    _ite_0 = ((void*) UNIT);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "merge");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Box_merge_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_merge_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_merge_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_merge_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_merge));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_merge_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_merge_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_merge_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_merge_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_merge));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_merge_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_merge_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_merge_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_merge_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_merge));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


struct _enc__env_closure0
{
};


static void _enc__trace_closure0(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  struct _enc__env_closure0* _env = p;
}


static value_t _enc__closure_fun_closure0(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env)
{
  ENC_DTRACE1(CLOSURE_ENTRY, (uintptr_t)*_ctx);
  option_t* _enc__arg_input = (_args[0]).p;
  int64_t _match_0;
  if ((({int64_t _valueCheck_6;
         _valueCheck_6 = (({option_t* _option_7 = (&(DEFAULT_NOTHING)); _option_7;}) == _enc__arg_input); _valueCheck_6;}) && ({int64_t _literal_8 = 1/*True*/; _literal_8;})))
  {
    _match_0 = ((int64_t) ({int64_t _literal_1 = 1/*True*/; _literal_1;}));
  }
  else
  {
    option_t* _unused_2;
    if ((({int64_t _varBinding_4;
           _unused_2 = _enc__arg_input;
           _varBinding_4 = 1; _varBinding_4;}) && ({int64_t _literal_5 = 1/*True*/; _literal_5;})))
    {
      _match_0 = ((int64_t) ({int64_t _literal_3 = 0/*False*/; _literal_3;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE1(CLOSURE_EXIT, (uintptr_t)*_ctx);
  return ((encore_arg_t) {.i = _match_0});
}


int64_t _enc__method__Ped_util_Regions_Box_link(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Regions_Box_t* _enc__arg_a)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "link");
  /* var c = a!max() */;
  /* c = a!max() */;
  check_receiver(_enc__arg_a, " ! ", "a", "max", "\"./Ped_util/Regions.enc\" (line 248, column 15)");
  pony_type_t* _tmp_0[] = {};
  future_t* _fut_1 = _enc__method__Ped_util_Regions_Box_max_future(_ctx, _enc__arg_a, NULL);
  future_t* _c_3 = _fut_1;
  /* var d = a!min() */;
  /* d = a!min() */;
  check_receiver(_enc__arg_a, " ! ", "a", "min", "\"./Ped_util/Regions.enc\" (line 249, column 15)");
  pony_type_t* _tmp_4[] = {};
  future_t* _fut_5 = _enc__method__Ped_util_Regions_Box_min_future(_ctx, _enc__arg_a, NULL);
  future_t* _d_7 = _fut_5;
  /* await(c) */;
  future_await(_ctx, _c_3);
  /* await(d) */;
  future_await(_ctx, _d_7);
  /* match (this.xmax, this.ymax, this.xmin, this.ymin, get(c), get(d)) with
  case (x_max_1, y_max_1, x_min_1, y_min_1, (x_max_2, y_max_2), (x_min_2, y_min_2)) =>
    var top = y_max_1 + 1 == y_min_2
    var bottom = y_min_1 - 1 == y_max_2
    var right = x_max_1 + 1 == x_min_2
    var left = x_min_1 - 1 == x_max_2
    var isnull = fun (input : Maybe[Box]) => match input with
                                               case Nothing =>
                                                 true
                                               end
                                               case unused =>
                                                 false
                                               end
                                             
                                             end
    if not(left || right || top || bottom) then
      false
    else
      if top then
        if right then
          if isnull(this.top_right) then
            this.top_right = Just(a)
          end
        else
          if left then
            if isnull(this.top_left) then
              this.top_left = Just(a)
            end
          else
            if isnull(this.up) then
              this.up = Just(a)
            end
          end
        end
      else
        if bottom then
          if right then
            if isnull(this.bottom_right) then
              this.bottom_right = Just(a)
            end
          else
            if left then
              if isnull(this.bottom_left) then
                this.bottom_left = Just(a)
              end
            else
              if isnull(this.down) then
                this.down = Just(a)
              end
            end
          end
        else
          if right then
            if isnull(this.right) then
              this.right = Just(a)
            end
          else
            if isnull(this.left) then
              this.left = Just(a)
            end
          end
        end
      end
      true
    end
  end

end */;
  tuple_t* _tuple_9 = tuple_mk(_ctx, 6);
  tuple_set_type(_tuple_9, 0, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_9, 1, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_9, 2, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_9, 3, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_9, 4, (&(tuple_type)));
  tuple_set_type(_tuple_9, 5, (&(tuple_type)));
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
  int64_t _fieldacc_10 = (*_this)._enc__field_xmax;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
  int64_t _fieldacc_11 = (*_this)._enc__field_ymax;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
  int64_t _fieldacc_12 = (*_this)._enc__field_xmin;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
  int64_t _fieldacc_13 = (*_this)._enc__field_ymin;
  tuple_t* _tmp_14 = future_get_actor(_ctx, _c_3).p;
  tuple_t* _tmp_15 = future_get_actor(_ctx, _d_7).p;
  tuple_set(_tuple_9, 0, ((encore_arg_t) {.i = _fieldacc_10}));
  tuple_set(_tuple_9, 1, ((encore_arg_t) {.i = _fieldacc_11}));
  tuple_set(_tuple_9, 2, ((encore_arg_t) {.i = _fieldacc_12}));
  tuple_set(_tuple_9, 3, ((encore_arg_t) {.i = _fieldacc_13}));
  tuple_set(_tuple_9, 4, ((encore_arg_t) {.p = _tmp_14}));
  tuple_set(_tuple_9, 5, ((encore_arg_t) {.p = _tmp_15}));
  int64_t _match_8;
  int64_t _x_max_1_16;
  int64_t _y_max_1_17;
  int64_t _x_min_1_18;
  int64_t _y_min_1_19;
  int64_t _x_max_2_20;
  int64_t _y_max_2_21;
  int64_t _x_min_2_22;
  int64_t _y_min_2_23;
  if ((({int64_t _tupleCheck_101;
         _tupleCheck_101 = 1;
         int64_t _tupleAccess_102 = tuple_get(_tuple_9, 0).i;
         int64_t _varBinding_103;
         _x_max_1_16 = _tupleAccess_102;
         _varBinding_103 = 1;
         _tupleCheck_101 = (_tupleCheck_101 && _varBinding_103);
         int64_t _tupleAccess_104 = tuple_get(_tuple_9, 1).i;
         int64_t _varBinding_105;
         _y_max_1_17 = _tupleAccess_104;
         _varBinding_105 = 1;
         _tupleCheck_101 = (_tupleCheck_101 && _varBinding_105);
         int64_t _tupleAccess_106 = tuple_get(_tuple_9, 2).i;
         int64_t _varBinding_107;
         _x_min_1_18 = _tupleAccess_106;
         _varBinding_107 = 1;
         _tupleCheck_101 = (_tupleCheck_101 && _varBinding_107);
         int64_t _tupleAccess_108 = tuple_get(_tuple_9, 3).i;
         int64_t _varBinding_109;
         _y_min_1_19 = _tupleAccess_108;
         _varBinding_109 = 1;
         _tupleCheck_101 = (_tupleCheck_101 && _varBinding_109);
         tuple_t* _tupleAccess_110 = tuple_get(_tuple_9, 4).p;
         int64_t _tupleCheck_111;
         _tupleCheck_111 = 1;
         int64_t _tupleAccess_112 = tuple_get(_tupleAccess_110, 0).i;
         int64_t _varBinding_113;
         _x_max_2_20 = _tupleAccess_112;
         _varBinding_113 = 1;
         _tupleCheck_111 = (_tupleCheck_111 && _varBinding_113);
         int64_t _tupleAccess_114 = tuple_get(_tupleAccess_110, 1).i;
         int64_t _varBinding_115;
         _y_max_2_21 = _tupleAccess_114;
         _varBinding_115 = 1;
         _tupleCheck_111 = (_tupleCheck_111 && _varBinding_115);
         _tupleCheck_101 = (_tupleCheck_101 && _tupleCheck_111);
         tuple_t* _tupleAccess_116 = tuple_get(_tuple_9, 5).p;
         int64_t _tupleCheck_117;
         _tupleCheck_117 = 1;
         int64_t _tupleAccess_118 = tuple_get(_tupleAccess_116, 0).i;
         int64_t _varBinding_119;
         _x_min_2_22 = _tupleAccess_118;
         _varBinding_119 = 1;
         _tupleCheck_117 = (_tupleCheck_117 && _varBinding_119);
         int64_t _tupleAccess_120 = tuple_get(_tupleAccess_116, 1).i;
         int64_t _varBinding_121;
         _y_min_2_23 = _tupleAccess_120;
         _varBinding_121 = 1;
         _tupleCheck_117 = (_tupleCheck_117 && _varBinding_121);
         _tupleCheck_101 = (_tupleCheck_101 && _tupleCheck_117); _tupleCheck_101;}) && ({int64_t _literal_122 = 1/*True*/; _literal_122;})))
  {
    _match_8 = ((int64_t) ({/* var top = y_max_1 + 1 == y_min_2 */;
                            /* top = y_max_1 + 1 == y_min_2 */;
                            int64_t _binop_26 = (({int64_t _binop_25 = (({ _y_max_1_17;}) + ({int64_t _literal_24 = 1; _literal_24;})); _binop_25;}) == ({ _y_min_2_23;}));
                            int64_t _top_28 = _binop_26;
                            /* var bottom = y_min_1 - 1 == y_max_2 */;
                            /* bottom = y_min_1 - 1 == y_max_2 */;
                            int64_t _binop_31 = (({int64_t _binop_30 = (({ _y_min_1_19;}) - ({int64_t _literal_29 = 1; _literal_29;})); _binop_30;}) == ({ _y_max_2_21;}));
                            int64_t _bottom_33 = _binop_31;
                            /* var right = x_max_1 + 1 == x_min_2 */;
                            /* right = x_max_1 + 1 == x_min_2 */;
                            int64_t _binop_36 = (({int64_t _binop_35 = (({ _x_max_1_16;}) + ({int64_t _literal_34 = 1; _literal_34;})); _binop_35;}) == ({ _x_min_2_22;}));
                            int64_t _right_38 = _binop_36;
                            /* var left = x_min_1 - 1 == x_max_2 */;
                            /* left = x_min_1 - 1 == x_max_2 */;
                            int64_t _binop_41 = (({int64_t _binop_40 = (({ _x_min_1_18;}) - ({int64_t _literal_39 = 1; _literal_39;})); _binop_40;}) == ({ _x_max_2_20;}));
                            int64_t _left_43 = _binop_41;
                            /* var isnull = fun (input : Maybe[Box]) => match input with
                                           case Nothing =>
                                             true
                                           end
                                           case unused =>
                                             false
                                           end
                                         
                                         end */;
                            /* isnull = fun (input : Maybe[Box]) => match input with
                              case Nothing =>
                                true
                              end
                              case unused =>
                                false
                              end
                            
                            end */;
                            struct _enc__env_closure0* _enc__env_closure0 = encore_alloc((*_ctx), sizeof(struct _enc__env_closure0));
                            closure_t* _tmp_44 = closure_mk(_ctx, _enc__closure_fun_closure0, _enc__env_closure0, _enc__trace_closure0, NULL);
                            closure_t* _isnull_46 = _tmp_44;
                            /* if not(left || right || top || bottom) then
  false
else
  if top then
    if right then
      if isnull(this.top_right) then
        this.top_right = Just(a)
      end
    else
      if left then
        if isnull(this.top_left) then
          this.top_left = Just(a)
        end
      else
        if isnull(this.up) then
          this.up = Just(a)
        end
      end
    end
  else
    if bottom then
      if right then
        if isnull(this.bottom_right) then
          this.bottom_right = Just(a)
        end
      else
        if left then
          if isnull(this.bottom_left) then
            this.bottom_left = Just(a)
          end
        else
          if isnull(this.down) then
            this.down = Just(a)
          end
        end
      end
    else
      if right then
        if isnull(this.right) then
          this.right = Just(a)
        end
      else
        if isnull(this.left) then
          this.left = Just(a)
        end
      end
    end
  end
  true
end */;
                            int64_t _ite_47;
                            if (({int64_t _binop_50 = (({int64_t _binop_49 = (({int64_t _binop_48 = (({ _left_43;}) || ({ _right_38;})); _binop_48;}) || ({ _top_28;})); _binop_49;}) || ({ _bottom_33;}));
                                  int64_t _unary_51 = (! _binop_50); _unary_51;}))
                            {
                              int64_t _literal_52 = 0/*False*/;
                              _ite_47 = ((int64_t) _literal_52);
                            }
                            else
                            {
                              /* if top then
  if right then
    if isnull(this.top_right) then
      this.top_right = Just(a)
    end
  else
    if left then
      if isnull(this.top_left) then
        this.top_left = Just(a)
      end
    else
      if isnull(this.up) then
        this.up = Just(a)
      end
    end
  end
else
  if bottom then
    if right then
      if isnull(this.bottom_right) then
        this.bottom_right = Just(a)
      end
    else
      if left then
        if isnull(this.bottom_left) then
          this.bottom_left = Just(a)
        end
      else
        if isnull(this.down) then
          this.down = Just(a)
        end
      end
    end
  else
    if right then
      if isnull(this.right) then
        this.right = Just(a)
      end
    else
      if isnull(this.left) then
        this.left = Just(a)
      end
    end
  end
end */;
                              void* _ite_53;
                              if (({ _top_28;}))
                              {
                                void* _ite_54;
                                if (({ _right_38;}))
                                {
                                  void* _ite_55;
                                  if (({value_t _tmp_57[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right");
                                                                                     option_t* _fieldacc_56 = (*_this)._enc__field_top_right; _fieldacc_56;})})};
                                        ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                        int64_t _clos_58 = closure_call(_ctx, _isnull_46, _tmp_57).i; _clos_58;}))
                                  {
                                    option_t* _option_59 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right"); _this;}))._enc__field_top_right = _option_59;
                                    _ite_55 = ((void*) UNIT);
                                  }
                                  else
                                  {
                                    UNIT;
                                    _ite_55 = ((void*) UNIT);
                                  };
                                  _ite_54 = ((void*) _ite_55);
                                }
                                else
                                {
                                  void* _ite_60;
                                  if (({ _left_43;}))
                                  {
                                    void* _ite_61;
                                    if (({value_t _tmp_63[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left");
                                                                                       option_t* _fieldacc_62 = (*_this)._enc__field_top_left; _fieldacc_62;})})};
                                          ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                          int64_t _clos_64 = closure_call(_ctx, _isnull_46, _tmp_63).i; _clos_64;}))
                                    {
                                      option_t* _option_65 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left"); _this;}))._enc__field_top_left = _option_65;
                                      _ite_61 = ((void*) UNIT);
                                    }
                                    else
                                    {
                                      UNIT;
                                      _ite_61 = ((void*) UNIT);
                                    };
                                    _ite_60 = ((void*) _ite_61);
                                  }
                                  else
                                  {
                                    void* _ite_66;
                                    if (({value_t _tmp_68[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "up");
                                                                                       option_t* _fieldacc_67 = (*_this)._enc__field_up; _fieldacc_67;})})};
                                          ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                          int64_t _clos_69 = closure_call(_ctx, _isnull_46, _tmp_68).i; _clos_69;}))
                                    {
                                      option_t* _option_70 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "up"); _this;}))._enc__field_up = _option_70;
                                      _ite_66 = ((void*) UNIT);
                                    }
                                    else
                                    {
                                      UNIT;
                                      _ite_66 = ((void*) UNIT);
                                    };
                                    _ite_60 = ((void*) _ite_66);
                                  };
                                  _ite_54 = ((void*) _ite_60);
                                };
                                _ite_53 = ((void*) _ite_54);
                              }
                              else
                              {
                                void* _ite_71;
                                if (({ _bottom_33;}))
                                {
                                  void* _ite_72;
                                  if (({ _right_38;}))
                                  {
                                    void* _ite_73;
                                    if (({value_t _tmp_75[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right");
                                                                                       option_t* _fieldacc_74 = (*_this)._enc__field_bottom_right; _fieldacc_74;})})};
                                          ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                          int64_t _clos_76 = closure_call(_ctx, _isnull_46, _tmp_75).i; _clos_76;}))
                                    {
                                      option_t* _option_77 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right"); _this;}))._enc__field_bottom_right = _option_77;
                                      _ite_73 = ((void*) UNIT);
                                    }
                                    else
                                    {
                                      UNIT;
                                      _ite_73 = ((void*) UNIT);
                                    };
                                    _ite_72 = ((void*) _ite_73);
                                  }
                                  else
                                  {
                                    void* _ite_78;
                                    if (({ _left_43;}))
                                    {
                                      void* _ite_79;
                                      if (({value_t _tmp_81[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left");
                                                                                         option_t* _fieldacc_80 = (*_this)._enc__field_bottom_left; _fieldacc_80;})})};
                                            ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                            int64_t _clos_82 = closure_call(_ctx, _isnull_46, _tmp_81).i; _clos_82;}))
                                      {
                                        option_t* _option_83 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                        (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left"); _this;}))._enc__field_bottom_left = _option_83;
                                        _ite_79 = ((void*) UNIT);
                                      }
                                      else
                                      {
                                        UNIT;
                                        _ite_79 = ((void*) UNIT);
                                      };
                                      _ite_78 = ((void*) _ite_79);
                                    }
                                    else
                                    {
                                      void* _ite_84;
                                      if (({value_t _tmp_86[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "down");
                                                                                         option_t* _fieldacc_85 = (*_this)._enc__field_down; _fieldacc_85;})})};
                                            ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                            int64_t _clos_87 = closure_call(_ctx, _isnull_46, _tmp_86).i; _clos_87;}))
                                      {
                                        option_t* _option_88 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                        (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "down"); _this;}))._enc__field_down = _option_88;
                                        _ite_84 = ((void*) UNIT);
                                      }
                                      else
                                      {
                                        UNIT;
                                        _ite_84 = ((void*) UNIT);
                                      };
                                      _ite_78 = ((void*) _ite_84);
                                    };
                                    _ite_72 = ((void*) _ite_78);
                                  };
                                  _ite_71 = ((void*) _ite_72);
                                }
                                else
                                {
                                  void* _ite_89;
                                  if (({ _right_38;}))
                                  {
                                    void* _ite_90;
                                    if (({value_t _tmp_92[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "right");
                                                                                       option_t* _fieldacc_91 = (*_this)._enc__field_right; _fieldacc_91;})})};
                                          ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                          int64_t _clos_93 = closure_call(_ctx, _isnull_46, _tmp_92).i; _clos_93;}))
                                    {
                                      option_t* _option_94 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "right"); _this;}))._enc__field_right = _option_94;
                                      _ite_90 = ((void*) UNIT);
                                    }
                                    else
                                    {
                                      UNIT;
                                      _ite_90 = ((void*) UNIT);
                                    };
                                    _ite_89 = ((void*) _ite_90);
                                  }
                                  else
                                  {
                                    void* _ite_95;
                                    if (({value_t _tmp_97[] = {((encore_arg_t) {.p = ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "left");
                                                                                       option_t* _fieldacc_96 = (*_this)._enc__field_left; _fieldacc_96;})})};
                                          ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "isnull");
                                          int64_t _clos_98 = closure_call(_ctx, _isnull_46, _tmp_97).i; _clos_98;}))
                                    {
                                      option_t* _option_99 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), ENCORE_ACTIVE);
                                      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "left"); _this;}))._enc__field_left = _option_99;
                                      _ite_95 = ((void*) UNIT);
                                    }
                                    else
                                    {
                                      UNIT;
                                      _ite_95 = ((void*) UNIT);
                                    };
                                    _ite_89 = ((void*) _ite_95);
                                  };
                                  _ite_71 = ((void*) _ite_89);
                                };
                                _ite_53 = ((void*) _ite_71);
                              };
                              /* true */;
                              int64_t _literal_100 = 1/*True*/;
                              _ite_47 = ((int64_t) _literal_100);
                            }; _ite_47;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "link");
  return ((int64_t) _match_8);
}


future_t* _enc__method__Ped_util_Regions_Box_link_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Regions_Box_t* _enc__arg_a)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_actor((*_ctx), ((pony_actor_t*) _enc__arg_a));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_link_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_link_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_link_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_link));
  msg->f1 = _enc__arg_a;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_link_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Regions_Box_t* _enc__arg_a, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_actor((*_ctx), ((pony_actor_t*) _enc__arg_a));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_link_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_link_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_link_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_link));
  msg->f1 = _enc__arg_a;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_link_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Regions_Box_t* _enc__arg_a)
{
  pony_gc_send((*_ctx));
  encore_trace_actor((*_ctx), ((pony_actor_t*) _enc__arg_a));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_link_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_link_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_link_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_link));
  msg->f1 = _enc__arg_a;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


int64_t _enc__method__Ped_util_Regions_Box_add_internal(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "add_internal");
  int64_t _ite_0;
  if (({int64_t _binop_5 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
                              _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_2 = (*_this)._enc__field_matrix;
                              check_receiver(_fieldacc_2, ".", "this.matrix", "get_val", "\"./Ped_util/Regions.enc\" (line 219, column 8)");
                              pony_type_t* _tmp_3[] = {};
                              int64_t _sync_method_call_1 = _enc__method__Ped_util_Quad_tree_Quad_tree_get_val(_ctx, _fieldacc_2, NULL, _enc__arg_x, _enc__arg_y); _sync_method_call_1;}) == ({int64_t _literal_4 = 1/*True*/; _literal_4;})); _binop_5;}))
  {
    /* print("AGENTS IN TOP OF eachother\n") */;
    char* _literal_6 = "AGENTS IN TOP OF eachother\n";
    fprintf(stdout, "%s", _literal_6);
    /* while true do
  ()
end */;
    void* _while_8;
    while (({int64_t _literal_7 = 1/*True*/; _literal_7;}))
    {
      UNIT;
      _while_8 = UNIT;
    };
    /* true */;
    int64_t _literal_9 = 1/*True*/;
    _ite_0 = ((int64_t) _literal_9);
  }
  else
  {
    /* this.matrix.set(x, y) */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
    _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_11 = (*_this)._enc__field_matrix;
    check_receiver(_fieldacc_11, ".", "this.matrix", "set", "\"./Ped_util/Regions.enc\" (line 226, column 7)");
    pony_type_t* _tmp_12[] = {};
    void* _sync_method_call_10 = _enc__method__Ped_util_Quad_tree_Quad_tree_set(_ctx, _fieldacc_11, NULL, _enc__arg_x, _enc__arg_y);
    /* if this.newcommers == Nothing then
  this.newcommers = Just(new Item(a))
else
  if this.last == Nothing then
    this.last = Just(new Item(a))
    match this.newcommers with
      case Just(thing) =>
        thing.append(this.last)
      end
    
    end
  else
    var new_wraper = Just(new Item(a))
    match this.last with
      case Just(thing) =>
        thing.append(new_wraper)
      end
    
    end
    this.last = new_wraper
  end
end */;
    void* _ite_13;
    if (({tuple_t* _tuple_15 = tuple_mk(_ctx, 2);
          tuple_set_type(_tuple_15, 0, (&(option_type)));
          tuple_set_type(_tuple_15, 1, (&(option_type)));
          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers");
          option_t* _fieldacc_16 = (*_this)._enc__field_newcommers;
          option_t* _option_17 = (&(DEFAULT_NOTHING));
          tuple_set(_tuple_15, 0, ((encore_arg_t) {.p = _fieldacc_16}));
          tuple_set(_tuple_15, 1, ((encore_arg_t) {.p = _option_17}));
          int64_t _match_14;
          _enc__class__Ped_util_Regions_Item_t* __fst_18;
          _enc__class__Ped_util_Regions_Item_t* __snd_19;
          if ((({int64_t _tupleCheck_34;
                 _tupleCheck_34 = 1;
                 option_t* _tupleAccess_35 = tuple_get(_tuple_15, 0).p;
                 int64_t _optionCheck_37;
                 _optionCheck_37 = ((JUST == (*_tupleAccess_35).tag) && ({int64_t _varBinding_38;
                                                                          _enc__class__Ped_util_Regions_Item_t* _optionVal_36 = (*_tupleAccess_35).val.p;
                                                                          __fst_18 = _optionVal_36;
                                                                          _varBinding_38 = 1; _varBinding_38;}));
                 _tupleCheck_34 = (_tupleCheck_34 && _optionCheck_37);
                 option_t* _tupleAccess_39 = tuple_get(_tuple_15, 1).p;
                 int64_t _optionCheck_41;
                 _optionCheck_41 = ((JUST == (*_tupleAccess_39).tag) && ({int64_t _varBinding_42;
                                                                          _enc__class__Ped_util_Regions_Item_t* _optionVal_40 = (*_tupleAccess_39).val.p;
                                                                          __snd_19 = _optionVal_40;
                                                                          _varBinding_42 = 1; _varBinding_42;}));
                 _tupleCheck_34 = (_tupleCheck_34 && _optionCheck_41); _tupleCheck_34;}) && ({int64_t _binop_43 = (({ __fst_18;}) == ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_19;}))); _binop_43;})))
          {
            _match_14 = ((int64_t) ({int64_t _literal_20 = 1/*True*/; _literal_20;}));
          }
          else
          {
            if ((({int64_t _tupleCheck_26;
                   _tupleCheck_26 = 1;
                   option_t* _tupleAccess_27 = tuple_get(_tuple_15, 0).p;
                   int64_t _valueCheck_28;
                   _valueCheck_28 = (({option_t* _option_29 = (&(DEFAULT_NOTHING)); _option_29;}) == _tupleAccess_27);
                   _tupleCheck_26 = (_tupleCheck_26 && _valueCheck_28);
                   option_t* _tupleAccess_30 = tuple_get(_tuple_15, 1).p;
                   int64_t _valueCheck_31;
                   _valueCheck_31 = (({option_t* _option_32 = (&(DEFAULT_NOTHING)); _option_32;}) == _tupleAccess_30);
                   _tupleCheck_26 = (_tupleCheck_26 && _valueCheck_31); _tupleCheck_26;}) && ({int64_t _literal_33 = 1/*True*/; _literal_33;})))
            {
              _match_14 = ((int64_t) ({int64_t _literal_21 = 1/*True*/; _literal_21;}));
            }
            else
            {
              tuple_t* ___22;
              if ((({int64_t _varBinding_24;
                     ___22 = _tuple_15;
                     _varBinding_24 = 1; _varBinding_24;}) && ({int64_t _literal_25 = 1/*True*/; _literal_25;})))
              {
                _match_14 = ((int64_t) ({int64_t _literal_23 = 0/*False*/; _literal_23;}));
              }
              else
              {
                fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                exit(1);
              };
            };
          }; _match_14;}))
    {
      _enc__class__Ped_util_Regions_Item_t* _new_44 = _enc__constructor__Ped_util_Regions_Item(_ctx, NULL);
      pony_type_t* _tmp_45[] = {};
      _enc__type_init__Ped_util_Regions_Item(_new_44);
      _enc__method__Ped_util_Regions_Item_init(_ctx, _new_44, NULL, _enc__arg_a);
      option_t* _option_46 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_44}), (&(_enc__class__Ped_util_Regions_Item_type)));
      (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers"); _this;}))._enc__field_newcommers = _option_46;
      _ite_13 = ((void*) UNIT);
    }
    else
    {
      void* _ite_47;
      if (({tuple_t* _tuple_49 = tuple_mk(_ctx, 2);
            tuple_set_type(_tuple_49, 0, (&(option_type)));
            tuple_set_type(_tuple_49, 1, (&(option_type)));
            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "last");
            option_t* _fieldacc_50 = (*_this)._enc__field_last;
            option_t* _option_51 = (&(DEFAULT_NOTHING));
            tuple_set(_tuple_49, 0, ((encore_arg_t) {.p = _fieldacc_50}));
            tuple_set(_tuple_49, 1, ((encore_arg_t) {.p = _option_51}));
            int64_t _match_48;
            _enc__class__Ped_util_Regions_Item_t* __fst_52;
            _enc__class__Ped_util_Regions_Item_t* __snd_53;
            if ((({int64_t _tupleCheck_68;
                   _tupleCheck_68 = 1;
                   option_t* _tupleAccess_69 = tuple_get(_tuple_49, 0).p;
                   int64_t _optionCheck_71;
                   _optionCheck_71 = ((JUST == (*_tupleAccess_69).tag) && ({int64_t _varBinding_72;
                                                                            _enc__class__Ped_util_Regions_Item_t* _optionVal_70 = (*_tupleAccess_69).val.p;
                                                                            __fst_52 = _optionVal_70;
                                                                            _varBinding_72 = 1; _varBinding_72;}));
                   _tupleCheck_68 = (_tupleCheck_68 && _optionCheck_71);
                   option_t* _tupleAccess_73 = tuple_get(_tuple_49, 1).p;
                   int64_t _optionCheck_75;
                   _optionCheck_75 = ((JUST == (*_tupleAccess_73).tag) && ({int64_t _varBinding_76;
                                                                            _enc__class__Ped_util_Regions_Item_t* _optionVal_74 = (*_tupleAccess_73).val.p;
                                                                            __snd_53 = _optionVal_74;
                                                                            _varBinding_76 = 1; _varBinding_76;}));
                   _tupleCheck_68 = (_tupleCheck_68 && _optionCheck_75); _tupleCheck_68;}) && ({int64_t _binop_77 = (({ __fst_52;}) == ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_53;}))); _binop_77;})))
            {
              _match_48 = ((int64_t) ({int64_t _literal_54 = 1/*True*/; _literal_54;}));
            }
            else
            {
              if ((({int64_t _tupleCheck_60;
                     _tupleCheck_60 = 1;
                     option_t* _tupleAccess_61 = tuple_get(_tuple_49, 0).p;
                     int64_t _valueCheck_62;
                     _valueCheck_62 = (({option_t* _option_63 = (&(DEFAULT_NOTHING)); _option_63;}) == _tupleAccess_61);
                     _tupleCheck_60 = (_tupleCheck_60 && _valueCheck_62);
                     option_t* _tupleAccess_64 = tuple_get(_tuple_49, 1).p;
                     int64_t _valueCheck_65;
                     _valueCheck_65 = (({option_t* _option_66 = (&(DEFAULT_NOTHING)); _option_66;}) == _tupleAccess_64);
                     _tupleCheck_60 = (_tupleCheck_60 && _valueCheck_65); _tupleCheck_60;}) && ({int64_t _literal_67 = 1/*True*/; _literal_67;})))
              {
                _match_48 = ((int64_t) ({int64_t _literal_55 = 1/*True*/; _literal_55;}));
              }
              else
              {
                tuple_t* ___56;
                if ((({int64_t _varBinding_58;
                       ___56 = _tuple_49;
                       _varBinding_58 = 1; _varBinding_58;}) && ({int64_t _literal_59 = 1/*True*/; _literal_59;})))
                {
                  _match_48 = ((int64_t) ({int64_t _literal_57 = 0/*False*/; _literal_57;}));
                }
                else
                {
                  fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                  exit(1);
                };
              };
            }; _match_48;}))
      {
        /* this.last = Just(new Item(a)) */;
        _enc__class__Ped_util_Regions_Item_t* _new_78 = _enc__constructor__Ped_util_Regions_Item(_ctx, NULL);
        pony_type_t* _tmp_79[] = {};
        _enc__type_init__Ped_util_Regions_Item(_new_78);
        _enc__method__Ped_util_Regions_Item_init(_ctx, _new_78, NULL, _enc__arg_a);
        option_t* _option_80 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_78}), (&(_enc__class__Ped_util_Regions_Item_type)));
        (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "last"); _this;}))._enc__field_last = _option_80;
        /* match this.newcommers with
  case Just(thing) =>
    thing.append(this.last)
  end

end */;
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers");
        option_t* _fieldacc_82 = (*_this)._enc__field_newcommers;
        void* _match_81;
        _enc__class__Ped_util_Regions_Item_t* _thing_83;
        if ((({int64_t _optionCheck_88;
               _optionCheck_88 = ((JUST == (*_fieldacc_82).tag) && ({int64_t _varBinding_89;
                                                                     _enc__class__Ped_util_Regions_Item_t* _optionVal_87 = (*_fieldacc_82).val.p;
                                                                     _thing_83 = _optionVal_87;
                                                                     _varBinding_89 = 1; _varBinding_89;})); _optionCheck_88;}) && ({int64_t _literal_90 = 1/*True*/; _literal_90;})))
        {
          _match_81 = ((void*) ({check_receiver(_thing_83, ".", "thing", "append", "\"./Ped_util/Regions.enc\" (line 233, column 35)");
                                 ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "last");
                                 option_t* _fieldacc_85 = (*_this)._enc__field_last;
                                 pony_type_t* _tmp_86[] = {};
                                 void* _sync_method_call_84 = _enc__method__Ped_util_Regions_Item_append(_ctx, _thing_83, NULL, _fieldacc_85); _sync_method_call_84;}));
        }
        else
        {
          fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
          exit(1);
        };
        _ite_47 = ((void*) _match_81);
      }
      else
      {
        /* var new_wraper = Just(new Item(a)) */;
        /* new_wraper = Just(new Item(a)) */;
        _enc__class__Ped_util_Regions_Item_t* _new_91 = _enc__constructor__Ped_util_Regions_Item(_ctx, NULL);
        pony_type_t* _tmp_92[] = {};
        _enc__type_init__Ped_util_Regions_Item(_new_91);
        _enc__method__Ped_util_Regions_Item_init(_ctx, _new_91, NULL, _enc__arg_a);
        option_t* _option_93 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_91}), (&(_enc__class__Ped_util_Regions_Item_type)));
        option_t* _new_wraper_95 = _option_93;
        /* match this.last with
  case Just(thing) =>
    thing.append(new_wraper)
  end

end */;
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "last");
        option_t* _fieldacc_97 = (*_this)._enc__field_last;
        void* _match_96;
        _enc__class__Ped_util_Regions_Item_t* _thing_98;
        if ((({int64_t _optionCheck_102;
               _optionCheck_102 = ((JUST == (*_fieldacc_97).tag) && ({int64_t _varBinding_103;
                                                                      _enc__class__Ped_util_Regions_Item_t* _optionVal_101 = (*_fieldacc_97).val.p;
                                                                      _thing_98 = _optionVal_101;
                                                                      _varBinding_103 = 1; _varBinding_103;})); _optionCheck_102;}) && ({int64_t _literal_104 = 1/*True*/; _literal_104;})))
        {
          _match_96 = ((void*) ({check_receiver(_thing_98, ".", "thing", "append", "\"./Ped_util/Regions.enc\" (line 238, column 33)");
                                 pony_type_t* _tmp_100[] = {};
                                 void* _sync_method_call_99 = _enc__method__Ped_util_Regions_Item_append(_ctx, _thing_98, NULL, _new_wraper_95); _sync_method_call_99;}));
        }
        else
        {
          fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
          exit(1);
        };
        /* this.last = new_wraper */;
        (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "last"); _this;}))._enc__field_last = _new_wraper_95;
        _ite_47 = ((void*) UNIT);
      };
      _ite_13 = ((void*) _ite_47);
    };
    /* this.size = this.size + 1 */;
    int64_t _binop_107 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
                            int64_t _fieldacc_105 = (*_this)._enc__field_size; _fieldacc_105;}) + ({int64_t _literal_106 = 1; _literal_106;}));
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _binop_107;
    /* true */;
    int64_t _literal_108 = 1/*True*/;
    _ite_0 = ((int64_t) _literal_108);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "add_internal");
  return ((int64_t) _ite_0);
}


future_t* _enc__method__Ped_util_Regions_Box_add_internal_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* Not tracing field '_enc__arg_x' */;
  /* Not tracing field '_enc__arg_y' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_add_internal_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_add_internal_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_add_internal_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_add_internal));
  msg->f1 = _enc__arg_a;
  msg->f2 = _enc__arg_x;
  msg->f3 = _enc__arg_y;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_add_internal_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* Not tracing field '_enc__arg_x' */;
  /* Not tracing field '_enc__arg_y' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_add_internal_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_add_internal_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_add_internal_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_add_internal));
  msg->f1 = _enc__arg_a;
  msg->f2 = _enc__arg_x;
  msg->f3 = _enc__arg_y;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_add_internal_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* Not tracing field '_enc__arg_x' */;
  /* Not tracing field '_enc__arg_y' */;
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_add_internal_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_add_internal_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_add_internal_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_add_internal));
  msg->f1 = _enc__arg_a;
  msg->f2 = _enc__arg_x;
  msg->f3 = _enc__arg_y;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


option_t* _enc__method__Ped_util_Regions_Box_external_move(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "external_move");
  option_t* _ite_0;
  if (({int64_t _binop_11 = (({int64_t _binop_8 = (({int64_t _binop_5 = (({int64_t _binop_2 = (({ _enc__arg_x;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
                                                                                                                     int64_t _fieldacc_1 = (*_this)._enc__field_xmax; _fieldacc_1;})); _binop_2;}) || ({int64_t _binop_4 = (({ _enc__arg_x;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
                                                                                                                                                                                                                                                  int64_t _fieldacc_3 = (*_this)._enc__field_xmin; _fieldacc_3;})); _binop_4;})); _binop_5;}) || ({int64_t _binop_7 = (({ _enc__arg_y;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
                                                                                                                                                                                                                                                                                                                                                                                             int64_t _fieldacc_6 = (*_this)._enc__field_ymax; _fieldacc_6;})); _binop_7;})); _binop_8;}) || ({int64_t _binop_10 = (({ _enc__arg_y;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         int64_t _fieldacc_9 = (*_this)._enc__field_ymin; _fieldacc_9;})); _binop_10;})); _binop_11;}))
  {
    option_t* _option_12 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), (&(_enc__class__Ped_util_Agent_passive_Agent_type)));
    _ite_0 = ((option_t*) _option_12);
  }
  else
  {
    option_t* _ite_13;
    if (({int64_t _binop_18 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix");
                                 _enc__class__Ped_util_Quad_tree_Quad_tree_t* _fieldacc_15 = (*_this)._enc__field_matrix;
                                 check_receiver(_fieldacc_15, ".", "this.matrix", "get_val", "\"./Ped_util/Regions.enc\" (line 209, column 10)");
                                 pony_type_t* _tmp_16[] = {};
                                 int64_t _sync_method_call_14 = _enc__method__Ped_util_Quad_tree_Quad_tree_get_val(_ctx, _fieldacc_15, NULL, _enc__arg_x, _enc__arg_y); _sync_method_call_14;}) == ({int64_t _literal_17 = 1/*True*/; _literal_17;})); _binop_18;}))
    {
      option_t* _option_19 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _enc__arg_a}), (&(_enc__class__Ped_util_Agent_passive_Agent_type)));
      _ite_13 = ((option_t*) _option_19);
    }
    else
    {
      /* a.move_int(x, y) */;
      check_receiver(_enc__arg_a, ".", "a", "move_int", "\"./Ped_util/Regions.enc\" (line 212, column 9)");
      pony_type_t* _tmp_21[] = {};
      void* _sync_method_call_20 = _enc__method__Ped_util_Agent_passive_Agent_move_int(_ctx, _enc__arg_a, NULL, _enc__arg_x, _enc__arg_y);
      /* this.add_internal(a, x, y) */;
      check_receiver(_this, ".", "this", "add_internal", "\"./Ped_util/Regions.enc\" (line 213, column 9)");
      pony_type_t* _tmp_23[] = {};
      int64_t _sync_method_call_22 = _enc__method__Ped_util_Regions_Box_add_internal(_ctx, _this, NULL, _enc__arg_a, _enc__arg_x, _enc__arg_y);
      /* Nothing */;
      option_t* _option_24 = (&(DEFAULT_NOTHING));
      _ite_13 = ((option_t*) _option_24);
    };
    _ite_0 = ((option_t*) _ite_13);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "external_move");
  return ((option_t*) _ite_0);
}


future_t* _enc__method__Ped_util_Regions_Box_external_move_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  future_t* _fut = future_mk(_ctx, (&(option_type)));
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* Not tracing field '_enc__arg_x' */;
  /* Not tracing field '_enc__arg_y' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_external_move_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_external_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_external_move_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_external_move));
  msg->f1 = _enc__arg_a;
  msg->f2 = _enc__arg_x;
  msg->f3 = _enc__arg_y;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_external_move_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* Not tracing field '_enc__arg_x' */;
  /* Not tracing field '_enc__arg_y' */;
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_external_move_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_external_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_external_move_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_external_move));
  msg->f1 = _enc__arg_a;
  msg->f2 = _enc__arg_x;
  msg->f3 = _enc__arg_y;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_external_move_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* Not tracing field '_enc__arg_x' */;
  /* Not tracing field '_enc__arg_y' */;
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_external_move_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_external_move_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_external_move_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_external_move));
  msg->f1 = _enc__arg_a;
  msg->f2 = _enc__arg_x;
  msg->f3 = _enc__arg_y;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


int64_t _enc__method__Ped_util_Regions_Box_add(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "add");
  int64_t _ite_0;
  if (({int64_t _binop_15 = (({int64_t _binop_11 = (({int64_t _binop_7 = (({int64_t _binop_3 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_a, "x");
                                                                                                  int64_t _fieldacc_1 = (*_enc__arg_a)._enc__field_x; _fieldacc_1;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
                                                                                                                                                                         int64_t _fieldacc_2 = (*_this)._enc__field_xmax; _fieldacc_2;})); _binop_3;}) || ({int64_t _binop_6 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_a, "x");
                                                                                                                                                                                                                                                                                  int64_t _fieldacc_4 = (*_enc__arg_a)._enc__field_x; _fieldacc_4;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
                                                                                                                                                                                                                                                                                                                                                         int64_t _fieldacc_5 = (*_this)._enc__field_xmin; _fieldacc_5;})); _binop_6;})); _binop_7;}) || ({int64_t _binop_10 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_a, "y");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 int64_t _fieldacc_8 = (*_enc__arg_a)._enc__field_y; _fieldacc_8;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        int64_t _fieldacc_9 = (*_this)._enc__field_ymax; _fieldacc_9;})); _binop_10;})); _binop_11;}) || ({int64_t _binop_14 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_a, "y");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  int64_t _fieldacc_12 = (*_enc__arg_a)._enc__field_y; _fieldacc_12;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           int64_t _fieldacc_13 = (*_this)._enc__field_ymin; _fieldacc_13;})); _binop_14;})); _binop_15;}))
  {
    int64_t _literal_16 = 0/*False*/;
    _ite_0 = ((int64_t) _literal_16);
  }
  else
  {
    check_receiver(_this, ".", "this", "add_internal", "\"./Ped_util/Regions.enc\" (line 202, column 7)");
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_a, "x");
    int64_t _fieldacc_18 = (*_enc__arg_a)._enc__field_x;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_a, "y");
    int64_t _fieldacc_19 = (*_enc__arg_a)._enc__field_y;
    pony_type_t* _tmp_20[] = {};
    int64_t _sync_method_call_17 = _enc__method__Ped_util_Regions_Box_add_internal(_ctx, _this, NULL, _enc__arg_a, _fieldacc_18, _fieldacc_19);
    _ite_0 = ((int64_t) _sync_method_call_17);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "add");
  return ((int64_t) _ite_0);
}


future_t* _enc__method__Ped_util_Regions_Box_add_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_add_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_add_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_add_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_add));
  msg->f1 = _enc__arg_a;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_add_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_add_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_add_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_add_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_add));
  msg->f1 = _enc__arg_a;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_add_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_add_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_add_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_add_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_add));
  msg->f1 = _enc__arg_a;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


array_t* _enc__method__Ped_util_Regions_Box_agents(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  /* this.merge() */;
  check_receiver(_this, ".", "this", "merge", "\"./Ped_util/Regions.enc\" (line 170, column 5)");
  pony_type_t* _tmp_1[] = {};
  void* _sync_method_call_0 = _enc__method__Ped_util_Regions_Box_merge(_ctx, _this, NULL);
  /* match this.agents with
  case Nothing =>
    new [(int, int)](0)
  end
  case thing =>
    var cur = thing
    var i = 0
    while cur != Nothing do
      i = i + 1
      match cur with
        case Just(thing) =>
          cur = thing.next
        end
      
      end
    end
    var ret = new [(int, int)](i)
    cur = this.agents
    i = 0
    while cur != Nothing do
      match cur with
        case Just(thing) =>
          cur = thing.next
          ret(i) = thing.a.pos()
        end
      
      end
      i = i + 1
    end
    ret
  end

end */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  option_t* _fieldacc_3 = (*_this)._enc__field_agents;
  array_t* _match_2;
  if ((({int64_t _valueCheck_102;
         _valueCheck_102 = (({option_t* _option_103 = (&(DEFAULT_NOTHING)); _option_103;}) == _fieldacc_3); _valueCheck_102;}) && ({int64_t _literal_104 = 1/*True*/; _literal_104;})))
  {
    _match_2 = ((array_t*) ({int64_t _literal_5 = 0;
                             array_t* _array_4 = array_mk(_ctx, _literal_5, (&(tuple_type))); _array_4;}));
  }
  else
  {
    option_t* _thing_6;
    if ((({int64_t _varBinding_100;
           _thing_6 = _fieldacc_3;
           _varBinding_100 = 1; _varBinding_100;}) && ({int64_t _literal_101 = 1/*True*/; _literal_101;})))
    {
      _match_2 = ((array_t*) ({/* var cur = thing */;
                               /* cur = thing */;
                               option_t* _cur_8 = _thing_6;
                               /* var i = 0 */;
                               /* i = 0 */;
                               int64_t _literal_9 = 0;
                               int64_t _i_11 = _literal_9;
                               /* while cur != Nothing do
  i = i + 1
  match cur with
    case Just(thing) =>
      cur = thing.next
    end
  
  end
end */;
                               void* _while_51;
                               while (({tuple_t* _tuple_13 = tuple_mk(_ctx, 2);
                                        tuple_set_type(_tuple_13, 0, (&(option_type)));
                                        tuple_set_type(_tuple_13, 1, (&(option_type)));
                                        option_t* _option_14 = (&(DEFAULT_NOTHING));
                                        tuple_set(_tuple_13, 0, ((encore_arg_t) {.p = _cur_8}));
                                        tuple_set(_tuple_13, 1, ((encore_arg_t) {.p = _option_14}));
                                        int64_t _match_12;
                                        _enc__class__Ped_util_Regions_Item_t* __fst_15;
                                        _enc__class__Ped_util_Regions_Item_t* __snd_16;
                                        if ((({int64_t _tupleCheck_31;
                                               _tupleCheck_31 = 1;
                                               option_t* _tupleAccess_32 = tuple_get(_tuple_13, 0).p;
                                               int64_t _optionCheck_34;
                                               _optionCheck_34 = ((JUST == (*_tupleAccess_32).tag) && ({int64_t _varBinding_35;
                                                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_33 = (*_tupleAccess_32).val.p;
                                                                                                        __fst_15 = _optionVal_33;
                                                                                                        _varBinding_35 = 1; _varBinding_35;}));
                                               _tupleCheck_31 = (_tupleCheck_31 && _optionCheck_34);
                                               option_t* _tupleAccess_36 = tuple_get(_tuple_13, 1).p;
                                               int64_t _optionCheck_38;
                                               _optionCheck_38 = ((JUST == (*_tupleAccess_36).tag) && ({int64_t _varBinding_39;
                                                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_37 = (*_tupleAccess_36).val.p;
                                                                                                        __snd_16 = _optionVal_37;
                                                                                                        _varBinding_39 = 1; _varBinding_39;}));
                                               _tupleCheck_31 = (_tupleCheck_31 && _optionCheck_38); _tupleCheck_31;}) && ({int64_t _binop_40 = (({ __fst_15;}) != ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_16;}))); _binop_40;})))
                                        {
                                          _match_12 = ((int64_t) ({int64_t _literal_17 = 1/*True*/; _literal_17;}));
                                        }
                                        else
                                        {
                                          if ((({int64_t _tupleCheck_23;
                                                 _tupleCheck_23 = 1;
                                                 option_t* _tupleAccess_24 = tuple_get(_tuple_13, 0).p;
                                                 int64_t _valueCheck_25;
                                                 _valueCheck_25 = (({option_t* _option_26 = (&(DEFAULT_NOTHING)); _option_26;}) == _tupleAccess_24);
                                                 _tupleCheck_23 = (_tupleCheck_23 && _valueCheck_25);
                                                 option_t* _tupleAccess_27 = tuple_get(_tuple_13, 1).p;
                                                 int64_t _valueCheck_28;
                                                 _valueCheck_28 = (({option_t* _option_29 = (&(DEFAULT_NOTHING)); _option_29;}) == _tupleAccess_27);
                                                 _tupleCheck_23 = (_tupleCheck_23 && _valueCheck_28); _tupleCheck_23;}) && ({int64_t _literal_30 = 1/*True*/; _literal_30;})))
                                          {
                                            _match_12 = ((int64_t) ({int64_t _literal_18 = 1/*True*/; _literal_18;}));
                                          }
                                          else
                                          {
                                            tuple_t* ___19;
                                            if ((({int64_t _varBinding_21;
                                                   ___19 = _tuple_13;
                                                   _varBinding_21 = 1; _varBinding_21;}) && ({int64_t _literal_22 = 1/*True*/; _literal_22;})))
                                            {
                                              _match_12 = ((int64_t) ({int64_t _literal_20 = 0/*False*/; _literal_20;}));
                                            }
                                            else
                                            {
                                              fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                              exit(1);
                                            };
                                          };
                                        };
                                        int64_t _unary_41 = (! _match_12); _unary_41;}))
                               {
                                 /* i = i + 1 */;
                                 int64_t _binop_43 = (({ _i_11;}) + ({int64_t _literal_42 = 1; _literal_42;}));
                                 _i_11 = _binop_43;
                                 /* match cur with
  case Just(thing) =>
    cur = thing.next
  end

end */;
                                 void* _match_44;
                                 _enc__class__Ped_util_Regions_Item_t* _thing_45;
                                 if ((({int64_t _optionCheck_48;
                                        _optionCheck_48 = ((JUST == (*_cur_8).tag) && ({int64_t _varBinding_49;
                                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_47 = (*_cur_8).val.p;
                                                                                        _thing_45 = _optionVal_47;
                                                                                        _varBinding_49 = 1; _varBinding_49;})); _optionCheck_48;}) && ({int64_t _literal_50 = 1/*True*/; _literal_50;})))
                                 {
                                   _match_44 = ((void*) ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_thing_45, "next");
                                                          option_t* _fieldacc_46 = (*_thing_45)._enc__field_next;
                                                          _cur_8 = _fieldacc_46; UNIT;}));
                                 }
                                 else
                                 {
                                   fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                   exit(1);
                                 };
                                 _while_51 = _match_44;
                               };
                               /* var ret = new [(int, int)](i) */;
                               /* ret = new [(int, int)](i) */;
                               array_t* _array_52 = array_mk(_ctx, _i_11, (&(tuple_type)));
                               array_t* _ret_54 = _array_52;
                               /* cur = this.agents */;
                               ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
                               option_t* _fieldacc_55 = (*_this)._enc__field_agents;
                               _cur_8 = _fieldacc_55;
                               /* i = 0 */;
                               int64_t _literal_56 = 0;
                               _i_11 = _literal_56;
                               /* while cur != Nothing do
  match cur with
    case Just(thing) =>
      cur = thing.next
      ret(i) = thing.a.pos()
    end
  
  end
  i = i + 1
end */;
                               void* _while_99;
                               while (({tuple_t* _tuple_58 = tuple_mk(_ctx, 2);
                                        tuple_set_type(_tuple_58, 0, (&(option_type)));
                                        tuple_set_type(_tuple_58, 1, (&(option_type)));
                                        option_t* _option_59 = (&(DEFAULT_NOTHING));
                                        tuple_set(_tuple_58, 0, ((encore_arg_t) {.p = _cur_8}));
                                        tuple_set(_tuple_58, 1, ((encore_arg_t) {.p = _option_59}));
                                        int64_t _match_57;
                                        _enc__class__Ped_util_Regions_Item_t* __fst_60;
                                        _enc__class__Ped_util_Regions_Item_t* __snd_61;
                                        if ((({int64_t _tupleCheck_76;
                                               _tupleCheck_76 = 1;
                                               option_t* _tupleAccess_77 = tuple_get(_tuple_58, 0).p;
                                               int64_t _optionCheck_79;
                                               _optionCheck_79 = ((JUST == (*_tupleAccess_77).tag) && ({int64_t _varBinding_80;
                                                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_78 = (*_tupleAccess_77).val.p;
                                                                                                        __fst_60 = _optionVal_78;
                                                                                                        _varBinding_80 = 1; _varBinding_80;}));
                                               _tupleCheck_76 = (_tupleCheck_76 && _optionCheck_79);
                                               option_t* _tupleAccess_81 = tuple_get(_tuple_58, 1).p;
                                               int64_t _optionCheck_83;
                                               _optionCheck_83 = ((JUST == (*_tupleAccess_81).tag) && ({int64_t _varBinding_84;
                                                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_82 = (*_tupleAccess_81).val.p;
                                                                                                        __snd_61 = _optionVal_82;
                                                                                                        _varBinding_84 = 1; _varBinding_84;}));
                                               _tupleCheck_76 = (_tupleCheck_76 && _optionCheck_83); _tupleCheck_76;}) && ({int64_t _binop_85 = (({ __fst_60;}) != ((_enc__class__Ped_util_Regions_Item_t*) ({ __snd_61;}))); _binop_85;})))
                                        {
                                          _match_57 = ((int64_t) ({int64_t _literal_62 = 1/*True*/; _literal_62;}));
                                        }
                                        else
                                        {
                                          if ((({int64_t _tupleCheck_68;
                                                 _tupleCheck_68 = 1;
                                                 option_t* _tupleAccess_69 = tuple_get(_tuple_58, 0).p;
                                                 int64_t _valueCheck_70;
                                                 _valueCheck_70 = (({option_t* _option_71 = (&(DEFAULT_NOTHING)); _option_71;}) == _tupleAccess_69);
                                                 _tupleCheck_68 = (_tupleCheck_68 && _valueCheck_70);
                                                 option_t* _tupleAccess_72 = tuple_get(_tuple_58, 1).p;
                                                 int64_t _valueCheck_73;
                                                 _valueCheck_73 = (({option_t* _option_74 = (&(DEFAULT_NOTHING)); _option_74;}) == _tupleAccess_72);
                                                 _tupleCheck_68 = (_tupleCheck_68 && _valueCheck_73); _tupleCheck_68;}) && ({int64_t _literal_75 = 1/*True*/; _literal_75;})))
                                          {
                                            _match_57 = ((int64_t) ({int64_t _literal_63 = 1/*True*/; _literal_63;}));
                                          }
                                          else
                                          {
                                            tuple_t* ___64;
                                            if ((({int64_t _varBinding_66;
                                                   ___64 = _tuple_58;
                                                   _varBinding_66 = 1; _varBinding_66;}) && ({int64_t _literal_67 = 1/*True*/; _literal_67;})))
                                            {
                                              _match_57 = ((int64_t) ({int64_t _literal_65 = 0/*False*/; _literal_65;}));
                                            }
                                            else
                                            {
                                              fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                              exit(1);
                                            };
                                          };
                                        };
                                        int64_t _unary_86 = (! _match_57); _unary_86;}))
                               {
                                 /* match cur with
  case Just(thing) =>
    cur = thing.next
    ret(i) = thing.a.pos()
  end

end */;
                                 void* _match_87;
                                 _enc__class__Ped_util_Regions_Item_t* _thing_88;
                                 if ((({int64_t _optionCheck_94;
                                        _optionCheck_94 = ((JUST == (*_cur_8).tag) && ({int64_t _varBinding_95;
                                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_93 = (*_cur_8).val.p;
                                                                                        _thing_88 = _optionVal_93;
                                                                                        _varBinding_95 = 1; _varBinding_95;})); _optionCheck_94;}) && ({int64_t _literal_96 = 1/*True*/; _literal_96;})))
                                 {
                                   _match_87 = ((void*) ({/* cur = thing.next */;
                                                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_thing_88, "next");
                                                          option_t* _fieldacc_89 = (*_thing_88)._enc__field_next;
                                                          _cur_8 = _fieldacc_89;
                                                          /* ret(i) = thing.a.pos() */;
                                                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_thing_88, "a");
                                                          _enc__class__Ped_util_Agent_passive_Agent_t* _fieldacc_91 = (*_thing_88)._enc__field_a;
                                                          check_receiver(_fieldacc_91, ".", "thing.a", "pos", "\"./Ped_util/Regions.enc\" (line 189, column 31)");
                                                          pony_type_t* _tmp_92[] = {};
                                                          tuple_t* _sync_method_call_90 = _enc__method__Ped_util_Agent_passive_Agent_pos(_ctx, _fieldacc_91, NULL);
                                                          array_set(_ret_54, _i_11, ((encore_arg_t) {.p = _sync_method_call_90})); UNIT;}));
                                 }
                                 else
                                 {
                                   fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                   exit(1);
                                 };
                                 /* i = i + 1 */;
                                 int64_t _binop_98 = (({ _i_11;}) + ({int64_t _literal_97 = 1; _literal_97;}));
                                 _i_11 = _binop_98;
                                 _while_99 = UNIT;
                               };
                               /* ret */; _ret_54;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "agents");
  return ((array_t*) _match_2);
}


future_t* _enc__method__Ped_util_Regions_Box_agents_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, (&(array_type)));
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_agents_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_agents_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_agents_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_agents));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_agents_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_agents_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_agents_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_agents_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_agents));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_agents_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_agents_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_agents_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_agents_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_agents));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


tuple_t* _enc__method__Ped_util_Regions_Box_max(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "max");
  tuple_t* _tuple_0 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_0, 0, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_0, 1, ENCORE_PRIMITIVE);
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
  int64_t _fieldacc_1 = (*_this)._enc__field_xmax;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
  int64_t _fieldacc_2 = (*_this)._enc__field_ymax;
  tuple_set(_tuple_0, 0, ((encore_arg_t) {.i = _fieldacc_1}));
  tuple_set(_tuple_0, 1, ((encore_arg_t) {.i = _fieldacc_2}));
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "max");
  return ((tuple_t*) _tuple_0);
}


future_t* _enc__method__Ped_util_Regions_Box_max_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, (&(tuple_type)));
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_max_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_max_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_max_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_max));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_max_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_max_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_max_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_max_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_max));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_max_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_max_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_max_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_max_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_max));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


tuple_t* _enc__method__Ped_util_Regions_Box_min(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "min");
  tuple_t* _tuple_0 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_0, 0, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_0, 1, ENCORE_PRIMITIVE);
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
  int64_t _fieldacc_1 = (*_this)._enc__field_xmin;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
  int64_t _fieldacc_2 = (*_this)._enc__field_ymin;
  tuple_set(_tuple_0, 0, ((encore_arg_t) {.i = _fieldacc_1}));
  tuple_set(_tuple_0, 1, ((encore_arg_t) {.i = _fieldacc_2}));
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "min");
  return ((tuple_t*) _tuple_0);
}


future_t* _enc__method__Ped_util_Regions_Box_min_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  future_t* _fut = future_mk(_ctx, (&(tuple_type)));
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_min_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_min_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_min_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_min));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_min_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_min_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_min_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_min_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_min));
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_min_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType)
{
  pony_gc_send((*_ctx));
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_min_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_min_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_min_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_min));
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


void* _enc__method__Ped_util_Regions_Box_init(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, tuple_t* _enc__arg_in_max, tuple_t* _enc__arg_in_min)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  /* this.agents = Nothing */;
  option_t* _option_0 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agents"); _this;}))._enc__field_agents = _option_0;
  /* this.last = Nothing */;
  option_t* _option_1 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "last"); _this;}))._enc__field_last = _option_1;
  /* this.newcommers = Nothing */;
  option_t* _option_2 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "newcommers"); _this;}))._enc__field_newcommers = _option_2;
  /* this.up = Nothing */;
  option_t* _option_3 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "up"); _this;}))._enc__field_up = _option_3;
  /* this.down = Nothing */;
  option_t* _option_4 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "down"); _this;}))._enc__field_down = _option_4;
  /* this.left = Nothing */;
  option_t* _option_5 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "left"); _this;}))._enc__field_left = _option_5;
  /* this.right = Nothing */;
  option_t* _option_6 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "right"); _this;}))._enc__field_right = _option_6;
  /* this.top_left = Nothing */;
  option_t* _option_7 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left"); _this;}))._enc__field_top_left = _option_7;
  /* this.bottom_right = Nothing */;
  option_t* _option_8 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right"); _this;}))._enc__field_bottom_right = _option_8;
  /* this.bottom_left = Nothing */;
  option_t* _option_9 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left"); _this;}))._enc__field_bottom_left = _option_9;
  /* this.matrix = new Quad_tree(in_max, in_min) */;
  _enc__class__Ped_util_Quad_tree_Quad_tree_t* _new_10 = _enc__constructor__Ped_util_Quad_tree_Quad_tree(_ctx, NULL);
  pony_type_t* _tmp_11[] = {};
  _enc__type_init__Ped_util_Quad_tree_Quad_tree(_new_10);
  _enc__method__Ped_util_Quad_tree_Quad_tree_init(_ctx, _new_10, NULL, _enc__arg_in_max, _enc__arg_in_min);
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "matrix"); _this;}))._enc__field_matrix = _new_10;
  /* this.top_right = Nothing */;
  option_t* _option_12 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right"); _this;}))._enc__field_top_right = _option_12;
  /* match in_max with
  case (x, y) =>
    this.xmax = x
    this.ymax = y
  end

end */;
  void* _match_13;
  int64_t _x_14;
  int64_t _y_15;
  if ((({int64_t _tupleCheck_16;
         _tupleCheck_16 = 1;
         int64_t _tupleAccess_17 = tuple_get(_enc__arg_in_max, 0).i;
         int64_t _varBinding_18;
         _x_14 = _tupleAccess_17;
         _varBinding_18 = 1;
         _tupleCheck_16 = (_tupleCheck_16 && _varBinding_18);
         int64_t _tupleAccess_19 = tuple_get(_enc__arg_in_max, 1).i;
         int64_t _varBinding_20;
         _y_15 = _tupleAccess_19;
         _varBinding_20 = 1;
         _tupleCheck_16 = (_tupleCheck_16 && _varBinding_20); _tupleCheck_16;}) && ({int64_t _literal_21 = 1/*True*/; _literal_21;})))
  {
    _match_13 = ((void*) ({/* this.xmax = x */;
                           (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax"); _this;}))._enc__field_xmax = _x_14;
                           /* this.ymax = y */;
                           (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax"); _this;}))._enc__field_ymax = _y_15; UNIT;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  /* match in_min with
  case (x, y) =>
    this.xmin = x
    this.ymin = y
  end

end */;
  void* _match_22;
  int64_t _x_23;
  int64_t _y_24;
  if ((({int64_t _tupleCheck_25;
         _tupleCheck_25 = 1;
         int64_t _tupleAccess_26 = tuple_get(_enc__arg_in_min, 0).i;
         int64_t _varBinding_27;
         _x_23 = _tupleAccess_26;
         _varBinding_27 = 1;
         _tupleCheck_25 = (_tupleCheck_25 && _varBinding_27);
         int64_t _tupleAccess_28 = tuple_get(_enc__arg_in_min, 1).i;
         int64_t _varBinding_29;
         _y_24 = _tupleAccess_28;
         _varBinding_29 = 1;
         _tupleCheck_25 = (_tupleCheck_25 && _varBinding_29); _tupleCheck_25;}) && ({int64_t _literal_30 = 1/*True*/; _literal_30;})))
  {
    _match_22 = ((void*) ({/* this.xmin = x */;
                           (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin"); _this;}))._enc__field_xmin = _x_23;
                           /* this.ymin = y */;
                           (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin"); _this;}))._enc__field_ymin = _y_24; UNIT;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  /* this.ttl = 10001 */;
  int64_t _literal_31 = 10001;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ttl"); _this;}))._enc__field_ttl = _literal_31;
  /* this.size = 0 */;
  int64_t _literal_32 = 0;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _literal_32;
  /* this.moving = false */;
  int64_t _literal_33 = 0/*False*/;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "moving"); _this;}))._enc__field_moving = _literal_33;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


future_t* _enc__method__Ped_util_Regions_Box_init_future(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, tuple_t* _enc__arg_in_max, tuple_t* _enc__arg_in_min)
{
  future_t* _fut = future_mk(_ctx, ENCORE_PRIMITIVE);
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_in_max, tuple_trace);
  encore_trace_object((*_ctx), _enc__arg_in_min, tuple_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_init_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_init_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_init_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_init));
  msg->f1 = _enc__arg_in_max;
  msg->f2 = _enc__arg_in_min;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


future_t* _enc__method__Ped_util_Regions_Box_init_forward(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, tuple_t* _enc__arg_in_max, tuple_t* _enc__arg_in_min, future_t* _fut)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_in_max, tuple_trace);
  encore_trace_object((*_ctx), _enc__arg_in_min, tuple_trace);
  encore_trace_object((*_ctx), _fut, future_trace);
  pony_send_done((*_ctx));
  _enc__fut_msg__Ped_util_Regions_Box_init_t* msg = ((_enc__fut_msg__Ped_util_Regions_Box_init_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__fut_msg__Ped_util_Regions_Box_init_t)), _ENC__FUT_MSG__Ped_util_Regions_Box_init));
  msg->f1 = _enc__arg_in_max;
  msg->f2 = _enc__arg_in_min;
  msg->_fut = _fut;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
  return _fut;
}


void _enc__method__Ped_util_Regions_Box_init_one_way(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Box_t* _this, pony_type_t** runtimeType, tuple_t* _enc__arg_in_max, tuple_t* _enc__arg_in_min)
{
  pony_gc_send((*_ctx));
  encore_trace_object((*_ctx), _enc__arg_in_max, tuple_trace);
  encore_trace_object((*_ctx), _enc__arg_in_min, tuple_trace);
  /* No tracing future for oneway msg */;
  pony_send_done((*_ctx));
  _enc__oneway_msg__Ped_util_Regions_Box_init_t* msg = ((_enc__oneway_msg__Ped_util_Regions_Box_init_t*) pony_alloc_msg(POOL_INDEX(sizeof(_enc__oneway_msg__Ped_util_Regions_Box_init_t)), _ENC__ONEWAY_MSG__Ped_util_Regions_Box_init));
  msg->f1 = _enc__arg_in_max;
  msg->f2 = _enc__arg_in_min;
  pony_sendv((*_ctx), ((pony_actor_t*) _this), ((pony_msg_t*) msg));
}


static void _enc__dispatch__Ped_util_Regions_Box(pony_ctx_t** _ctx, pony_actor_t* _a, pony_msg_t* _m)
{
  _enc__class__Ped_util_Regions_Box_t* _this = ((_enc__class__Ped_util_Regions_Box_t*) _a);
  switch (_m->id)
  {
    case _ENC__FUT_MSG__Ped_util_Regions_Box_await:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      pony_type_t* _enc__type__t = ((_enc__fut_msg__Ped_util_Regions_Box_await_t*) _m)->_enc__type__t;
      future_t* _enc__arg_f = ((_enc__fut_msg__Ped_util_Regions_Box_await_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_f, future_trace);
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {_enc__type__t};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_await(_ctx, _this, methodTypeVars, _enc__arg_f)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_await:
    {
      pony_type_t* _enc__type__t = ((_enc__oneway_msg__Ped_util_Regions_Box_await_t*) _m)->_enc__type__t;
      future_t* _enc__arg_f = ((_enc__oneway_msg__Ped_util_Regions_Box_await_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_f, future_trace);
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {_enc__type__t};
      _enc__method__Ped_util_Regions_Box_await(_ctx, _this, methodTypeVars, _enc__arg_f);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_suspend:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_suspend(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_suspend:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_suspend(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_move:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.i = _enc__method__Ped_util_Regions_Box_move(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_move:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_move(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_is_something:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      option_t* _enc__arg_a = ((_enc__fut_msg__Ped_util_Regions_Box_is_something_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, option_trace);
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.i = _enc__method__Ped_util_Regions_Box_is_something(_ctx, _this, methodTypeVars, _enc__arg_a)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_is_something:
    {
      option_t* _enc__arg_a = ((_enc__oneway_msg__Ped_util_Regions_Box_is_something_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, option_trace);
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_is_something(_ctx, _this, methodTypeVars, _enc__arg_a);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_merge:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_merge(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_merge:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_merge(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_link:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      _enc__class__Ped_util_Regions_Box_t* _enc__arg_a = ((_enc__fut_msg__Ped_util_Regions_Box_link_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_actor((*_ctx), ((pony_actor_t*) _enc__arg_a));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.i = _enc__method__Ped_util_Regions_Box_link(_ctx, _this, methodTypeVars, _enc__arg_a)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_link:
    {
      _enc__class__Ped_util_Regions_Box_t* _enc__arg_a = ((_enc__oneway_msg__Ped_util_Regions_Box_link_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_actor((*_ctx), ((pony_actor_t*) _enc__arg_a));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_link(_ctx, _this, methodTypeVars, _enc__arg_a);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_add_internal:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a = ((_enc__fut_msg__Ped_util_Regions_Box_add_internal_t*) _m)->f1;
      int64_t _enc__arg_x = ((_enc__fut_msg__Ped_util_Regions_Box_add_internal_t*) _m)->f2;
      int64_t _enc__arg_y = ((_enc__fut_msg__Ped_util_Regions_Box_add_internal_t*) _m)->f3;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
      /* Not tracing field '_enc__arg_x' */;
      /* Not tracing field '_enc__arg_y' */;
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.i = _enc__method__Ped_util_Regions_Box_add_internal(_ctx, _this, methodTypeVars, _enc__arg_a, _enc__arg_x, _enc__arg_y)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_add_internal:
    {
      _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a = ((_enc__oneway_msg__Ped_util_Regions_Box_add_internal_t*) _m)->f1;
      int64_t _enc__arg_x = ((_enc__oneway_msg__Ped_util_Regions_Box_add_internal_t*) _m)->f2;
      int64_t _enc__arg_y = ((_enc__oneway_msg__Ped_util_Regions_Box_add_internal_t*) _m)->f3;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
      /* Not tracing field '_enc__arg_x' */;
      /* Not tracing field '_enc__arg_y' */;
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_add_internal(_ctx, _this, methodTypeVars, _enc__arg_a, _enc__arg_x, _enc__arg_y);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_external_move:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a = ((_enc__fut_msg__Ped_util_Regions_Box_external_move_t*) _m)->f1;
      int64_t _enc__arg_x = ((_enc__fut_msg__Ped_util_Regions_Box_external_move_t*) _m)->f2;
      int64_t _enc__arg_y = ((_enc__fut_msg__Ped_util_Regions_Box_external_move_t*) _m)->f3;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
      /* Not tracing field '_enc__arg_x' */;
      /* Not tracing field '_enc__arg_y' */;
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_external_move(_ctx, _this, methodTypeVars, _enc__arg_a, _enc__arg_x, _enc__arg_y)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_external_move:
    {
      _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a = ((_enc__oneway_msg__Ped_util_Regions_Box_external_move_t*) _m)->f1;
      int64_t _enc__arg_x = ((_enc__oneway_msg__Ped_util_Regions_Box_external_move_t*) _m)->f2;
      int64_t _enc__arg_y = ((_enc__oneway_msg__Ped_util_Regions_Box_external_move_t*) _m)->f3;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
      /* Not tracing field '_enc__arg_x' */;
      /* Not tracing field '_enc__arg_y' */;
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_external_move(_ctx, _this, methodTypeVars, _enc__arg_a, _enc__arg_x, _enc__arg_y);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_add:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a = ((_enc__fut_msg__Ped_util_Regions_Box_add_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.i = _enc__method__Ped_util_Regions_Box_add(_ctx, _this, methodTypeVars, _enc__arg_a)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_add:
    {
      _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a = ((_enc__oneway_msg__Ped_util_Regions_Box_add_t*) _m)->f1;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_a, _enc__trace__Ped_util_Agent_passive_Agent);
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_add(_ctx, _this, methodTypeVars, _enc__arg_a);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_agents:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_agents(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_agents:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_agents(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_max:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_max(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_max:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_max(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_min:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_min(_ctx, _this, methodTypeVars)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_min:
    {
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_min(_ctx, _this, methodTypeVars);
      break;
    }
    case _ENC__FUT_MSG__Ped_util_Regions_Box_init:
    {
      future_t* _fut = ((encore_fut_msg_t*) _m)->_fut;
      tuple_t* _enc__arg_in_max = ((_enc__fut_msg__Ped_util_Regions_Box_init_t*) _m)->f1;
      tuple_t* _enc__arg_in_min = ((_enc__fut_msg__Ped_util_Regions_Box_init_t*) _m)->f2;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_in_max, tuple_trace);
      encore_trace_object((*_ctx), _enc__arg_in_min, tuple_trace);
      encore_trace_object((*_ctx), _fut, future_type.trace);
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      future_fulfil(_ctx, _fut, ((encore_arg_t) {.p = _enc__method__Ped_util_Regions_Box_init(_ctx, _this, methodTypeVars, _enc__arg_in_max, _enc__arg_in_min)}));
      break;
    }
    case _ENC__ONEWAY_MSG__Ped_util_Regions_Box_init:
    {
      tuple_t* _enc__arg_in_max = ((_enc__oneway_msg__Ped_util_Regions_Box_init_t*) _m)->f1;
      tuple_t* _enc__arg_in_min = ((_enc__oneway_msg__Ped_util_Regions_Box_init_t*) _m)->f2;
      
      // --- GC on receive ----------------------------------------;
      pony_gc_recv((*_ctx));
      encore_trace_object((*_ctx), _enc__arg_in_max, tuple_trace);
      encore_trace_object((*_ctx), _enc__arg_in_min, tuple_trace);
      /* Not tracing the future in a oneWay send */;
      pony_recv_done((*_ctx));
      // --- GC on receive ----------------------------------------;
      
      pony_type_t* methodTypeVars[] = {};
      _enc__method__Ped_util_Regions_Box_init(_ctx, _this, methodTypeVars, _enc__arg_in_max, _enc__arg_in_min);
      break;
    }
    default:
    {
      printf("error, got invalid id: %zd", _m->id);
    }
  };
}


pony_type_t _enc__class__Ped_util_Regions_Box_type = {.id=_ENC__ID__Ped_util_Regions_Box, .size=sizeof(_enc__class__Ped_util_Regions_Box_t), .trace=_enc__trace__Ped_util_Regions_Box, .dispatch=_enc__dispatch__Ped_util_Regions_Box, .vtable=trait_method_selector};
