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


void _enc__type_init__Ped_util_Quad_tree_Quad(_enc__class__Ped_util_Quad_tree_Quad_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_Quad_tree_Quad(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_Quad_tree_Quad_t* _this = p;
  int64_t _enc__field_size = _this->_enc__field_size;
  /* Not tracing field '_enc__field_size' */;
  int64_t _enc__field_min_y = _this->_enc__field_min_y;
  /* Not tracing field '_enc__field_min_y' */;
  int64_t _enc__field_min_x = _this->_enc__field_min_x;
  /* Not tracing field '_enc__field_min_x' */;
  int64_t _enc__field_max_y = _this->_enc__field_max_y;
  /* Not tracing field '_enc__field_max_y' */;
  int64_t _enc__field_max_x = _this->_enc__field_max_x;
  /* Not tracing field '_enc__field_max_x' */;
  option_t* _enc__field_bottom_left = _this->_enc__field_bottom_left;
  encore_trace_object((*_ctx), _enc__field_bottom_left, option_trace);
  option_t* _enc__field_bottom_right = _this->_enc__field_bottom_right;
  encore_trace_object((*_ctx), _enc__field_bottom_right, option_trace);
  option_t* _enc__field_top_left = _this->_enc__field_top_left;
  encore_trace_object((*_ctx), _enc__field_top_left, option_trace);
  option_t* _enc__field_top_right = _this->_enc__field_top_right;
  encore_trace_object((*_ctx), _enc__field_top_right, option_trace);
  array_t* _enc__field_agent = _this->_enc__field_agent;
  encore_trace_object((*_ctx), _enc__field_agent, array_trace);
}


_enc__class__Ped_util_Quad_tree_Quad_t* _enc__constructor__Ped_util_Quad_tree_Quad(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_Quad_tree_Quad_t* _this = ((_enc__class__Ped_util_Quad_tree_Quad_t*) encore_alloc((*_ctx), sizeof(_enc__class__Ped_util_Quad_tree_Quad_t)));
  _this->_enc__self_type = (&(_enc__class__Ped_util_Quad_tree_Quad_type));
  return _this;
}


_enc__class__Ped_util_Quad_tree_Quad_t* _enc__method__Ped_util_Quad_tree_Quad_recur(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "recur");
  /* val top = y > this.max_y - this.min_y / 2 + this.min_y */;
  /* top = y > this.max_y - this.min_y / 2 + this.min_y */;
  int64_t _binop_7 = (({ _enc__arg_y;}) > ({int64_t _binop_6 = (({int64_t _binop_4 = (({int64_t _binop_2 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_y");
                                                                                                              int64_t _fieldacc_0 = (*_this)._enc__field_max_y; _fieldacc_0;}) - ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                                                                                                                                                   int64_t _fieldacc_1 = (*_this)._enc__field_min_y; _fieldacc_1;})); _binop_2;}) / ({int64_t _literal_3 = 2; _literal_3;})); _binop_4;}) + ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                                                                                                                                                                                                                                                                                              int64_t _fieldacc_5 = (*_this)._enc__field_min_y; _fieldacc_5;})); _binop_6;}));
  int64_t _top_9 = _binop_7;
  /* val right = x > this.max_x - this.min_x / 2 + this.min_x */;
  /* right = x > this.max_x - this.min_x / 2 + this.min_x */;
  int64_t _binop_17 = (({ _enc__arg_x;}) > ({int64_t _binop_16 = (({int64_t _binop_14 = (({int64_t _binop_12 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_x");
                                                                                                                  int64_t _fieldacc_10 = (*_this)._enc__field_max_x; _fieldacc_10;}) - ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                                                                                                                                                         int64_t _fieldacc_11 = (*_this)._enc__field_min_x; _fieldacc_11;})); _binop_12;}) / ({int64_t _literal_13 = 2; _literal_13;})); _binop_14;}) + ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                                                                                                                                                                                                                                                                                                          int64_t _fieldacc_15 = (*_this)._enc__field_min_x; _fieldacc_15;})); _binop_16;}));
  int64_t _right_19 = _binop_17;
  /* if top then
  if right then
    match this.top_right with
      case Just(qq) =>
        qq
      end
    
    end
  else
    match this.top_left with
      case Just(qq) =>
        qq
      end
    
    end
  end
else
  if right then
    match this.bottom_right with
      case Just(qq) =>
        qq
      end
    
    end
  else
    match this.bottom_left with
      case Just(qq) =>
        qq
      end
    
    end
  end
end */;
  _enc__class__Ped_util_Quad_tree_Quad_t* _ite_20;
  if (({ _top_9;}))
  {
    _enc__class__Ped_util_Quad_tree_Quad_t* _ite_21;
    if (({ _right_19;}))
    {
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right");
      option_t* _fieldacc_23 = (*_this)._enc__field_top_right;
      _enc__class__Ped_util_Quad_tree_Quad_t* _match_22;
      _enc__class__Ped_util_Quad_tree_Quad_t* _qq_24;
      if ((({int64_t _optionCheck_26;
             _optionCheck_26 = ((JUST == (*_fieldacc_23).tag) && ({int64_t _varBinding_27;
                                                                   _enc__class__Ped_util_Quad_tree_Quad_t* _optionVal_25 = (*_fieldacc_23).val.p;
                                                                   _qq_24 = _optionVal_25;
                                                                   _varBinding_27 = 1; _varBinding_27;})); _optionCheck_26;}) && ({int64_t _literal_28 = 1/*True*/; _literal_28;})))
      {
        _match_22 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) ({ _qq_24;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
      _ite_21 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) _match_22);
    }
    else
    {
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left");
      option_t* _fieldacc_30 = (*_this)._enc__field_top_left;
      _enc__class__Ped_util_Quad_tree_Quad_t* _match_29;
      _enc__class__Ped_util_Quad_tree_Quad_t* _qq_31;
      if ((({int64_t _optionCheck_33;
             _optionCheck_33 = ((JUST == (*_fieldacc_30).tag) && ({int64_t _varBinding_34;
                                                                   _enc__class__Ped_util_Quad_tree_Quad_t* _optionVal_32 = (*_fieldacc_30).val.p;
                                                                   _qq_31 = _optionVal_32;
                                                                   _varBinding_34 = 1; _varBinding_34;})); _optionCheck_33;}) && ({int64_t _literal_35 = 1/*True*/; _literal_35;})))
      {
        _match_29 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) ({ _qq_31;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
      _ite_21 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) _match_29);
    };
    _ite_20 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) _ite_21);
  }
  else
  {
    _enc__class__Ped_util_Quad_tree_Quad_t* _ite_36;
    if (({ _right_19;}))
    {
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right");
      option_t* _fieldacc_38 = (*_this)._enc__field_bottom_right;
      _enc__class__Ped_util_Quad_tree_Quad_t* _match_37;
      _enc__class__Ped_util_Quad_tree_Quad_t* _qq_39;
      if ((({int64_t _optionCheck_41;
             _optionCheck_41 = ((JUST == (*_fieldacc_38).tag) && ({int64_t _varBinding_42;
                                                                   _enc__class__Ped_util_Quad_tree_Quad_t* _optionVal_40 = (*_fieldacc_38).val.p;
                                                                   _qq_39 = _optionVal_40;
                                                                   _varBinding_42 = 1; _varBinding_42;})); _optionCheck_41;}) && ({int64_t _literal_43 = 1/*True*/; _literal_43;})))
      {
        _match_37 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) ({ _qq_39;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
      _ite_36 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) _match_37);
    }
    else
    {
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left");
      option_t* _fieldacc_45 = (*_this)._enc__field_bottom_left;
      _enc__class__Ped_util_Quad_tree_Quad_t* _match_44;
      _enc__class__Ped_util_Quad_tree_Quad_t* _qq_46;
      if ((({int64_t _optionCheck_48;
             _optionCheck_48 = ((JUST == (*_fieldacc_45).tag) && ({int64_t _varBinding_49;
                                                                   _enc__class__Ped_util_Quad_tree_Quad_t* _optionVal_47 = (*_fieldacc_45).val.p;
                                                                   _qq_46 = _optionVal_47;
                                                                   _varBinding_49 = 1; _varBinding_49;})); _optionCheck_48;}) && ({int64_t _literal_50 = 1/*True*/; _literal_50;})))
      {
        _match_44 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) ({ _qq_46;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
      _ite_36 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) _match_44);
    };
    _ite_20 = ((_enc__class__Ped_util_Quad_tree_Quad_t*) _ite_36);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "recur");
  return ((_enc__class__Ped_util_Quad_tree_Quad_t*) _ite_20);
}


int64_t _enc__method__Ped_util_Quad_tree_Quad_same(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "same");
  int64_t _ite_0;
  if (({int64_t _binop_14 = (({int64_t _binop_9 = (({int64_t _binop_4 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                                                                           array_t* _fieldacc_1 = (*_this)._enc__field_agent;
                                                                           int64_t _size_2 = array_size(_fieldacc_1); _size_2;}) == ({int64_t _literal_3 = 2; _literal_3;})); _binop_4;}) && ({int64_t _binop_8 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                                                                                                                                                                                                                     array_t* _fieldacc_5 = (*_this)._enc__field_agent;
                                                                                                                                                                                                                     int64_t _literal_6 = 0;
                                                                                                                                                                                                                     int64_t _access_7 = array_get(_fieldacc_5, _literal_6).i; _access_7;}) == ({ _enc__arg_x;})); _binop_8;})); _binop_9;}) && ({int64_t _binop_13 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                                                                                                                                                                                                                                                                                                                                                                         array_t* _fieldacc_10 = (*_this)._enc__field_agent;
                                                                                                                                                                                                                                                                                                                                                                         int64_t _literal_11 = 1;
                                                                                                                                                                                                                                                                                                                                                                         int64_t _access_12 = array_get(_fieldacc_10, _literal_11).i; _access_12;}) == ({ _enc__arg_y;})); _binop_13;})); _binop_14;}))
  {
    int64_t _literal_15 = 1/*True*/;
    _ite_0 = ((int64_t) _literal_15);
  }
  else
  {
    int64_t _literal_16 = 0/*False*/;
    _ite_0 = ((int64_t) _literal_16);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "same");
  return ((int64_t) _ite_0);
}


int64_t _enc__method__Ped_util_Quad_tree_Quad_isin(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "isin");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right");
  option_t* _fieldacc_1 = (*_this)._enc__field_top_right;
  int64_t _match_0;
  if ((({int64_t _valueCheck_13;
         _valueCheck_13 = (({option_t* _option_14 = (&(DEFAULT_NOTHING)); _option_14;}) == _fieldacc_1); _valueCheck_13;}) && ({int64_t _literal_15 = 1/*True*/; _literal_15;})))
  {
    _match_0 = ((int64_t) ({check_receiver(_this, ".", "this", "same", "\"./Ped_util/Quad_tree.enc\" (line 83, column 23)");
                            pony_type_t* _tmp_3[] = {};
                            int64_t _sync_method_call_2 = _enc__method__Ped_util_Quad_tree_Quad_same(_ctx, _this, NULL, _enc__arg_x, _enc__arg_y); _sync_method_call_2;}));
  }
  else
  {
    _enc__class__Ped_util_Quad_tree_Quad_t* _unused_4;
    if ((({int64_t _optionCheck_10;
           _optionCheck_10 = ((JUST == (*_fieldacc_1).tag) && ({int64_t _varBinding_11;
                                                                _enc__class__Ped_util_Quad_tree_Quad_t* _optionVal_9 = (*_fieldacc_1).val.p;
                                                                _unused_4 = _optionVal_9;
                                                                _varBinding_11 = 1; _varBinding_11;})); _optionCheck_10;}) && ({int64_t _literal_12 = 1/*True*/; _literal_12;})))
    {
      _match_0 = ((int64_t) ({check_receiver(_this, ".", "this", "recur", "\"./Ped_util/Quad_tree.enc\" (line 84, column 28)");
                              pony_type_t* _tmp_7[] = {};
                              _enc__class__Ped_util_Quad_tree_Quad_t* _sync_method_call_6 = _enc__method__Ped_util_Quad_tree_Quad_recur(_ctx, _this, NULL, _enc__arg_x, _enc__arg_y);
                              check_receiver(_sync_method_call_6, ".", "this.recur(x, y)", "isin", "\"./Ped_util/Quad_tree.enc\" (line 84, column 28)");
                              pony_type_t* _tmp_8[] = {};
                              int64_t _sync_method_call_5 = _enc__method__Ped_util_Quad_tree_Quad_isin(_ctx, _sync_method_call_6, NULL, _enc__arg_x, _enc__arg_y); _sync_method_call_5;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "isin");
  return ((int64_t) _match_0);
}


int64_t _enc__method__Ped_util_Quad_tree_Quad_remove(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right");
  option_t* _fieldacc_1 = (*_this)._enc__field_top_right;
  int64_t _match_0;
  if ((({int64_t _valueCheck_33;
         _valueCheck_33 = (({option_t* _option_34 = (&(DEFAULT_NOTHING)); _option_34;}) == _fieldacc_1); _valueCheck_33;}) && ({int64_t _literal_35 = 1/*True*/; _literal_35;})))
  {
    _match_0 = ((int64_t) ({int64_t _ite_2;
                            if (({check_receiver(_this, ".", "this", "same", "\"./Ped_util/Quad_tree.enc\" (line 58, column 14)");
                                  pony_type_t* _tmp_4[] = {};
                                  int64_t _sync_method_call_3 = _enc__method__Ped_util_Quad_tree_Quad_same(_ctx, _this, NULL, _enc__arg_x, _enc__arg_y); _sync_method_call_3;}))
                            {
                              /* this.agent = new [int](0) */;
                              int64_t _literal_6 = 0;
                              array_t* _array_5 = array_mk(_ctx, _literal_6, ENCORE_PRIMITIVE);
                              (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agent"); _this;}))._enc__field_agent = _array_5;
                              /* this.size = 0 */;
                              int64_t _literal_7 = 0;
                              (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _literal_7;
                              /* true */;
                              int64_t _literal_8 = 1/*True*/;
                              _ite_2 = ((int64_t) _literal_8);
                            }
                            else
                            {
                              int64_t _literal_9 = 0/*False*/;
                              _ite_2 = ((int64_t) _literal_9);
                            }; _ite_2;}));
  }
  else
  {
    _enc__class__Ped_util_Quad_tree_Quad_t* _unused_10;
    if ((({int64_t _optionCheck_30;
           _optionCheck_30 = ((JUST == (*_fieldacc_1).tag) && ({int64_t _varBinding_31;
                                                                _enc__class__Ped_util_Quad_tree_Quad_t* _optionVal_29 = (*_fieldacc_1).val.p;
                                                                _unused_10 = _optionVal_29;
                                                                _varBinding_31 = 1; _varBinding_31;})); _optionCheck_30;}) && ({int64_t _literal_32 = 1/*True*/; _literal_32;})))
    {
      _match_0 = ((int64_t) ({/* val hit = this.recur(x, y).remove(x, y) */;
                              /* hit = this.recur(x, y).remove(x, y) */;
                              check_receiver(_this, ".", "this", "recur", "\"./Ped_util/Quad_tree.enc\" (line 67, column 21)");
                              pony_type_t* _tmp_13[] = {};
                              _enc__class__Ped_util_Quad_tree_Quad_t* _sync_method_call_12 = _enc__method__Ped_util_Quad_tree_Quad_recur(_ctx, _this, NULL, _enc__arg_x, _enc__arg_y);
                              check_receiver(_sync_method_call_12, ".", "this.recur(x, y)", "remove", "\"./Ped_util/Quad_tree.enc\" (line 67, column 21)");
                              pony_type_t* _tmp_14[] = {};
                              int64_t _sync_method_call_11 = _enc__method__Ped_util_Quad_tree_Quad_remove(_ctx, _sync_method_call_12, NULL, _enc__arg_x, _enc__arg_y);
                              int64_t _hit_16 = _sync_method_call_11;
                              /* if hit then
  this.size = this.size - 1
  if this.size == 0 then
    this.top_right = Nothing
    this.bottom_right = Nothing
    this.top_left = Nothing
    this.bottom_left = Nothing
  end
end */;
                              void* _ite_17;
                              if (({ _hit_16;}))
                              {
                                /* this.size = this.size - 1 */;
                                int64_t _binop_20 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
                                                       int64_t _fieldacc_18 = (*_this)._enc__field_size; _fieldacc_18;}) - ({int64_t _literal_19 = 1; _literal_19;}));
                                (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _binop_20;
                                /* if this.size == 0 then
  this.top_right = Nothing
  this.bottom_right = Nothing
  this.top_left = Nothing
  this.bottom_left = Nothing
end */;
                                void* _ite_21;
                                if (({int64_t _binop_24 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
                                                             int64_t _fieldacc_22 = (*_this)._enc__field_size; _fieldacc_22;}) == ({int64_t _literal_23 = 0; _literal_23;})); _binop_24;}))
                                {
                                  /* this.top_right = Nothing */;
                                  option_t* _option_25 = (&(DEFAULT_NOTHING));
                                  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right"); _this;}))._enc__field_top_right = _option_25;
                                  /* this.bottom_right = Nothing */;
                                  option_t* _option_26 = (&(DEFAULT_NOTHING));
                                  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right"); _this;}))._enc__field_bottom_right = _option_26;
                                  /* this.top_left = Nothing */;
                                  option_t* _option_27 = (&(DEFAULT_NOTHING));
                                  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left"); _this;}))._enc__field_top_left = _option_27;
                                  /* this.bottom_left = Nothing */;
                                  option_t* _option_28 = (&(DEFAULT_NOTHING));
                                  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left"); _this;}))._enc__field_bottom_left = _option_28;
                                  _ite_21 = ((void*) UNIT);
                                }
                                else
                                {
                                  UNIT;
                                  _ite_21 = ((void*) UNIT);
                                };
                                _ite_17 = ((void*) _ite_21);
                              }
                              else
                              {
                                UNIT;
                                _ite_17 = ((void*) UNIT);
                              };
                              /* hit */; _hit_16;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
  return ((int64_t) _match_0);
}


void* _enc__method__Ped_util_Quad_tree_Quad_add(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "add");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right");
  option_t* _fieldacc_1 = (*_this)._enc__field_top_right;
  void* _match_0;
  if ((({int64_t _valueCheck_105;
         _valueCheck_105 = (({option_t* _option_106 = (&(DEFAULT_NOTHING)); _option_106;}) == _fieldacc_1); _valueCheck_105;}) && ({int64_t _literal_107 = 1/*True*/; _literal_107;})))
  {
    _match_0 = ((void*) ({void* _ite_2;
                          if (({int64_t _binop_6 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                                                      array_t* _fieldacc_3 = (*_this)._enc__field_agent;
                                                      int64_t _size_4 = array_size(_fieldacc_3); _size_4;}) == ({int64_t _literal_5 = 0; _literal_5;})); _binop_6;}))
                          {
                            /* this.agent = new [int](2) */;
                            int64_t _literal_8 = 2;
                            array_t* _array_7 = array_mk(_ctx, _literal_8, ENCORE_PRIMITIVE);
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agent"); _this;}))._enc__field_agent = _array_7;
                            /* this.agent(0) = x */;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                            array_t* _fieldacc_9 = (*_this)._enc__field_agent;
                            int64_t _literal_10 = 0;
                            array_set(_fieldacc_9, _literal_10, ((encore_arg_t) {.i = _enc__arg_x}));
                            /* this.agent(1) = y */;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                            array_t* _fieldacc_11 = (*_this)._enc__field_agent;
                            int64_t _literal_12 = 1;
                            array_set(_fieldacc_11, _literal_12, ((encore_arg_t) {.i = _enc__arg_y}));
                            /* this.size = 1 */;
                            int64_t _literal_13 = 1;
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _literal_13;
                            _ite_2 = ((void*) UNIT);
                          }
                          else
                          {
                            /* val dx = this.max_x - this.min_x / 2 */;
                            /* dx = this.max_x - this.min_x / 2 */;
                            int64_t _binop_18 = (({int64_t _binop_16 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_x");
                                                                          int64_t _fieldacc_14 = (*_this)._enc__field_max_x; _fieldacc_14;}) - ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                                                                                                                 int64_t _fieldacc_15 = (*_this)._enc__field_min_x; _fieldacc_15;})); _binop_16;}) / ({int64_t _literal_17 = 2; _literal_17;}));
                            int64_t _dx_20 = _binop_18;
                            /* val dy = this.max_y - this.min_y / 2 */;
                            /* dy = this.max_y - this.min_y / 2 */;
                            int64_t _binop_25 = (({int64_t _binop_23 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_y");
                                                                          int64_t _fieldacc_21 = (*_this)._enc__field_max_y; _fieldacc_21;}) - ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                                                                                                                 int64_t _fieldacc_22 = (*_this)._enc__field_min_y; _fieldacc_22;})); _binop_23;}) / ({int64_t _literal_24 = 2; _literal_24;}));
                            int64_t _dy_27 = _binop_25;
                            /* this.top_right = Just(new Quad(this.max_x, this.max_y, this.min_x + dx + 1, this.min_y + dy + 1)) */;
                            _enc__class__Ped_util_Quad_tree_Quad_t* _new_28 = _enc__constructor__Ped_util_Quad_tree_Quad(_ctx, NULL);
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_x");
                            int64_t _fieldacc_29 = (*_this)._enc__field_max_x;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_y");
                            int64_t _fieldacc_30 = (*_this)._enc__field_max_y;
                            int64_t _binop_34 = (({int64_t _binop_32 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                                          int64_t _fieldacc_31 = (*_this)._enc__field_min_x; _fieldacc_31;}) + ({ _dx_20;})); _binop_32;}) + ({int64_t _literal_33 = 1; _literal_33;}));
                            int64_t _binop_38 = (({int64_t _binop_36 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                                          int64_t _fieldacc_35 = (*_this)._enc__field_min_y; _fieldacc_35;}) + ({ _dy_27;})); _binop_36;}) + ({int64_t _literal_37 = 1; _literal_37;}));
                            pony_type_t* _tmp_39[] = {};
                            _enc__type_init__Ped_util_Quad_tree_Quad(_new_28);
                            _enc__method__Ped_util_Quad_tree_Quad_init(_ctx, _new_28, NULL, _fieldacc_29, _fieldacc_30, _binop_34, _binop_38);
                            option_t* _option_40 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_28}), (&(_enc__class__Ped_util_Quad_tree_Quad_type)));
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right"); _this;}))._enc__field_top_right = _option_40;
                            /* this.top_left = Just(new Quad(this.min_x + dx, this.max_y, this.min_x, this.min_y + dy + 1)) */;
                            _enc__class__Ped_util_Quad_tree_Quad_t* _new_41 = _enc__constructor__Ped_util_Quad_tree_Quad(_ctx, NULL);
                            int64_t _binop_43 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                   int64_t _fieldacc_42 = (*_this)._enc__field_min_x; _fieldacc_42;}) + ({ _dx_20;}));
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_y");
                            int64_t _fieldacc_44 = (*_this)._enc__field_max_y;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                            int64_t _fieldacc_45 = (*_this)._enc__field_min_x;
                            int64_t _binop_49 = (({int64_t _binop_47 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                                          int64_t _fieldacc_46 = (*_this)._enc__field_min_y; _fieldacc_46;}) + ({ _dy_27;})); _binop_47;}) + ({int64_t _literal_48 = 1; _literal_48;}));
                            pony_type_t* _tmp_50[] = {};
                            _enc__type_init__Ped_util_Quad_tree_Quad(_new_41);
                            _enc__method__Ped_util_Quad_tree_Quad_init(_ctx, _new_41, NULL, _binop_43, _fieldacc_44, _fieldacc_45, _binop_49);
                            option_t* _option_51 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_41}), (&(_enc__class__Ped_util_Quad_tree_Quad_type)));
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left"); _this;}))._enc__field_top_left = _option_51;
                            /* this.bottom_right = Just(new Quad(this.max_x, this.min_y + dy, this.min_x + dx + 1, this.min_y)) */;
                            _enc__class__Ped_util_Quad_tree_Quad_t* _new_52 = _enc__constructor__Ped_util_Quad_tree_Quad(_ctx, NULL);
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "max_x");
                            int64_t _fieldacc_53 = (*_this)._enc__field_max_x;
                            int64_t _binop_55 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                   int64_t _fieldacc_54 = (*_this)._enc__field_min_y; _fieldacc_54;}) + ({ _dy_27;}));
                            int64_t _binop_59 = (({int64_t _binop_57 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                                          int64_t _fieldacc_56 = (*_this)._enc__field_min_x; _fieldacc_56;}) + ({ _dx_20;})); _binop_57;}) + ({int64_t _literal_58 = 1; _literal_58;}));
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                            int64_t _fieldacc_60 = (*_this)._enc__field_min_y;
                            pony_type_t* _tmp_61[] = {};
                            _enc__type_init__Ped_util_Quad_tree_Quad(_new_52);
                            _enc__method__Ped_util_Quad_tree_Quad_init(_ctx, _new_52, NULL, _fieldacc_53, _binop_55, _binop_59, _fieldacc_60);
                            option_t* _option_62 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_52}), (&(_enc__class__Ped_util_Quad_tree_Quad_type)));
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right"); _this;}))._enc__field_bottom_right = _option_62;
                            /* this.bottom_left = Just(new Quad(this.min_x + dx, this.min_y + dy, this.min_x, this.min_y)) */;
                            _enc__class__Ped_util_Quad_tree_Quad_t* _new_63 = _enc__constructor__Ped_util_Quad_tree_Quad(_ctx, NULL);
                            int64_t _binop_65 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                                                   int64_t _fieldacc_64 = (*_this)._enc__field_min_x; _fieldacc_64;}) + ({ _dx_20;}));
                            int64_t _binop_67 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                                                   int64_t _fieldacc_66 = (*_this)._enc__field_min_y; _fieldacc_66;}) + ({ _dy_27;}));
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x");
                            int64_t _fieldacc_68 = (*_this)._enc__field_min_x;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y");
                            int64_t _fieldacc_69 = (*_this)._enc__field_min_y;
                            pony_type_t* _tmp_70[] = {};
                            _enc__type_init__Ped_util_Quad_tree_Quad(_new_63);
                            _enc__method__Ped_util_Quad_tree_Quad_init(_ctx, _new_63, NULL, _binop_65, _binop_67, _fieldacc_68, _fieldacc_69);
                            option_t* _option_71 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _new_63}), (&(_enc__class__Ped_util_Quad_tree_Quad_type)));
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left"); _this;}))._enc__field_bottom_left = _option_71;
                            /* this.size = 2 */;
                            int64_t _literal_72 = 2;
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _literal_72;
                            /* this.recur(this.agent(0), this.agent(1)).add(this.agent(0), this.agent(1)) */;
                            check_receiver(_this, ".", "this", "recur", "\"./Ped_util/Quad_tree.enc\" (line 44, column 15)");
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                            array_t* _fieldacc_75 = (*_this)._enc__field_agent;
                            int64_t _literal_76 = 0;
                            int64_t _access_77 = array_get(_fieldacc_75, _literal_76).i;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                            array_t* _fieldacc_78 = (*_this)._enc__field_agent;
                            int64_t _literal_79 = 1;
                            int64_t _access_80 = array_get(_fieldacc_78, _literal_79).i;
                            pony_type_t* _tmp_81[] = {};
                            _enc__class__Ped_util_Quad_tree_Quad_t* _sync_method_call_74 = _enc__method__Ped_util_Quad_tree_Quad_recur(_ctx, _this, NULL, _access_77, _access_80);
                            check_receiver(_sync_method_call_74, ".", "this.recur((this.agent)(0), (this.agent)(1))", "add", "\"./Ped_util/Quad_tree.enc\" (line 44, column 15)");
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                            array_t* _fieldacc_82 = (*_this)._enc__field_agent;
                            int64_t _literal_83 = 0;
                            int64_t _access_84 = array_get(_fieldacc_82, _literal_83).i;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "agent");
                            array_t* _fieldacc_85 = (*_this)._enc__field_agent;
                            int64_t _literal_86 = 1;
                            int64_t _access_87 = array_get(_fieldacc_85, _literal_86).i;
                            pony_type_t* _tmp_88[] = {};
                            void* _sync_method_call_73 = _enc__method__Ped_util_Quad_tree_Quad_add(_ctx, _sync_method_call_74, NULL, _access_84, _access_87);
                            /* this.recur(x, y).add(x, y) */;
                            check_receiver(_this, ".", "this", "recur", "\"./Ped_util/Quad_tree.enc\" (line 45, column 15)");
                            pony_type_t* _tmp_91[] = {};
                            _enc__class__Ped_util_Quad_tree_Quad_t* _sync_method_call_90 = _enc__method__Ped_util_Quad_tree_Quad_recur(_ctx, _this, NULL, _enc__arg_x, _enc__arg_y);
                            check_receiver(_sync_method_call_90, ".", "this.recur(x, y)", "add", "\"./Ped_util/Quad_tree.enc\" (line 45, column 15)");
                            pony_type_t* _tmp_92[] = {};
                            void* _sync_method_call_89 = _enc__method__Ped_util_Quad_tree_Quad_add(_ctx, _sync_method_call_90, NULL, _enc__arg_x, _enc__arg_y);
                            /* this.agent = new [int](0) */;
                            int64_t _literal_94 = 0;
                            array_t* _array_93 = array_mk(_ctx, _literal_94, ENCORE_PRIMITIVE);
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agent"); _this;}))._enc__field_agent = _array_93;
                            _ite_2 = ((void*) UNIT);
                          }; _ite_2;}));
  }
  else
  {
    option_t* _unused_95;
    if ((({int64_t _varBinding_103;
           _unused_95 = _fieldacc_1;
           _varBinding_103 = 1; _varBinding_103;}) && ({int64_t _literal_104 = 1/*True*/; _literal_104;})))
    {
      _match_0 = ((void*) ({/* this.size = this.size + 1 */;
                            int64_t _binop_98 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
                                                   int64_t _fieldacc_96 = (*_this)._enc__field_size; _fieldacc_96;}) + ({int64_t _literal_97 = 1; _literal_97;}));
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _binop_98;
                            /* this.recur(x, y).add(x, y) */;
                            check_receiver(_this, ".", "this", "recur", "\"./Ped_util/Quad_tree.enc\" (line 51, column 15)");
                            pony_type_t* _tmp_101[] = {};
                            _enc__class__Ped_util_Quad_tree_Quad_t* _sync_method_call_100 = _enc__method__Ped_util_Quad_tree_Quad_recur(_ctx, _this, NULL, _enc__arg_x, _enc__arg_y);
                            check_receiver(_sync_method_call_100, ".", "this.recur(x, y)", "add", "\"./Ped_util/Quad_tree.enc\" (line 51, column 15)");
                            pony_type_t* _tmp_102[] = {};
                            void* _sync_method_call_99 = _enc__method__Ped_util_Quad_tree_Quad_add(_ctx, _sync_method_call_100, NULL, _enc__arg_x, _enc__arg_y); _sync_method_call_99;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "add");
  return UNIT;
}


void* _enc__method__Ped_util_Quad_tree_Quad_init(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_max_x, int64_t _enc__arg_max_y, int64_t _enc__arg_min_x, int64_t _enc__arg_min_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  /* this.max_x = max_x */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "max_x"); _this;}))._enc__field_max_x = _enc__arg_max_x;
  /* this.max_y = max_y */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "max_y"); _this;}))._enc__field_max_y = _enc__arg_max_y;
  /* this.min_x = min_x */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "min_x"); _this;}))._enc__field_min_x = _enc__arg_min_x;
  /* this.min_y = min_y */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "min_y"); _this;}))._enc__field_min_y = _enc__arg_min_y;
  /* this.top_right = Nothing */;
  option_t* _option_0 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_right"); _this;}))._enc__field_top_right = _option_0;
  /* this.top_left = Nothing */;
  option_t* _option_1 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "top_left"); _this;}))._enc__field_top_left = _option_1;
  /* this.bottom_right = Nothing */;
  option_t* _option_2 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_right"); _this;}))._enc__field_bottom_right = _option_2;
  /* this.bottom_left = Nothing */;
  option_t* _option_3 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "bottom_left"); _this;}))._enc__field_bottom_left = _option_3;
  /* this.size = 0 */;
  int64_t _literal_4 = 0;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _literal_4;
  /* this.agent = new [int](0) */;
  int64_t _literal_6 = 0;
  array_t* _array_5 = array_mk(_ctx, _literal_6, ENCORE_PRIMITIVE);
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "agent"); _this;}))._enc__field_agent = _array_5;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class__Ped_util_Quad_tree_Quad_type = {.id=_ENC__ID__Ped_util_Quad_tree_Quad, .size=sizeof(_enc__class__Ped_util_Quad_tree_Quad_t), .trace=_enc__trace__Ped_util_Quad_tree_Quad, .vtable=trait_method_selector};
