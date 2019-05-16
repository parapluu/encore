#include "header.h"


static void* trait_method_selector(int id)
{
  switch (id)
  {
    case _ENC__MSG_Collection_Collection_clone:
    {
      return _enc__method_LinkedList_LinkedList_clone;
      break;
    }
    case _ENC__MSG_Collection_Collection_size:
    {
      return _enc__method_LinkedList_LinkedList_size;
      break;
    }
    case _ENC__MSG_Collection_Collection_remove_front:
    {
      return _enc__method_LinkedList_LinkedList_remove_front;
      break;
    }
    case _ENC__MSG_Collection_Collection_remove_back:
    {
      return _enc__method_LinkedList_LinkedList_remove_back;
      break;
    }
    case _ENC__MSG_Collection_Collection_remove:
    {
      return _enc__method_LinkedList_LinkedList_remove;
      break;
    }
    case _ENC__MSG_Collection_Collection_prepend_all:
    {
      return _enc__method_LinkedList_LinkedList_prepend_all;
      break;
    }
    case _ENC__MSG_Collection_Collection_prepend:
    {
      return _enc__method_LinkedList_LinkedList_prepend;
      break;
    }
    case _ENC__MSG_Collection_Collection_nth:
    {
      return _enc__method_LinkedList_LinkedList_nth;
      break;
    }
    case _ENC__MSG_Collection_Collection_insert:
    {
      return _enc__method_LinkedList_LinkedList_insert;
      break;
    }
    case _ENC__MSG_Collection_Collection_element_at:
    {
      return _enc__method_LinkedList_LinkedList_element_at;
      break;
    }
    case _ENC__MSG_Collection_Collection_contains:
    {
      return _enc__method_LinkedList_LinkedList_contains;
      break;
    }
    case _ENC__MSG_Collection_Collection_append_all:
    {
      return _enc__method_LinkedList_LinkedList_append_all;
      break;
    }
    case _ENC__MSG_Collection_Collection_append:
    {
      return _enc__method_LinkedList_LinkedList_append;
      break;
    }
    default:
    {
      printf("error, got invalid id: %d", id);
    }
  };
  return NULL;
}


void _enc__type_init_LinkedList_LinkedList(_enc__class_LinkedList_LinkedList_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  _this->_enc__type_t = va_arg(params, pony_type_t *);
  va_end(params);
}


void _enc__trace_LinkedList_LinkedList(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class_LinkedList_LinkedList_t* _this = p;
  pony_type_t* _enc__type_t = _this->_enc__type_t;
  int64_t _enc__field_size = _this->_enc__field_size;
  /* Not tracing field '_enc__field_size' */;
  option_t* _enc__field_last = _this->_enc__field_last;
  encore_trace_object((*_ctx), _enc__field_last, option_trace);
  option_t* _enc__field_first = _this->_enc__field_first;
  encore_trace_object((*_ctx), _enc__field_first, option_trace);
}


_enc__class_LinkedList_LinkedList_t* _enc__constructor_LinkedList_LinkedList(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class_LinkedList_LinkedList_t* _this = ((_enc__class_LinkedList_LinkedList_t*) encore_alloc((*_ctx), sizeof(_enc__class_LinkedList_LinkedList_t)));
  _this->_enc__self_type = (&(_enc__class_LinkedList_LinkedList_type));
  return _this;
}


struct _enc__env_closure13
{
  _enc__class_LinkedList_LinkedList_t* _enc__field_result;
  future_t* _fut;
};


static void _enc__trace_closure13(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  struct _enc__env_closure13* _env = p;
  _enc__class_LinkedList_LinkedList_t* _enc__field_result = (*((struct _enc__env_closure13*) _env))._enc__field_result;
  encore_trace_object((*_ctx), _env->_enc__field_result, _enc__trace_LinkedList_LinkedList);
}


static value_t _enc__closure_fun_closure13(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env)
{
  ENC_DTRACE1(CLOSURE_ENTRY, (uintptr_t)*_ctx);
  encore_arg_t _enc__arg_x = (_args[0]);
  _enc__class_LinkedList_LinkedList_t* _enc__field_result = (*((struct _enc__env_closure13*) _env))._enc__field_result;
  check_receiver(_enc__field_result, ".", "result", "append", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 374, column 20)");
  pony_type_t* _tmp_1[] = {};
  void* _sync_method_call_0 = _enc__method_LinkedList_LinkedList_append(_ctx, _enc__field_result, NULL, _enc__arg_x);
  ENC_DTRACE1(CLOSURE_EXIT, (uintptr_t)*_ctx);
  return ((encore_arg_t) {.p = UNIT});
}


_enc__class_LinkedList_LinkedList_t* _enc__method_LinkedList_LinkedList_flatMap(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "flatMap");
  pony_type_t* _enc__type_u = (runtimeType[0]);
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* val result = new LinkedList[u]() */;
  /* result = new LinkedList[u]() */;
  _enc__class_LinkedList_LinkedList_t* _new_0 = _enc__constructor_LinkedList_LinkedList(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init_LinkedList_LinkedList(_new_0, _enc__type_u);
  _enc__method_LinkedList_LinkedList_init(_ctx, _new_0, NULL);
  _enc__class_LinkedList_LinkedList_t* _result_3 = _new_0;
  /* var cursor = this.first */;
  /* cursor = this.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_4 = (*_this)._enc__field_first;
  option_t* _cursor_6 = _fieldacc_4;
  /* while cursor != Nothing do
  val current = match cursor with
                  case Just(n) =>
                    n
                  end
                
                end
  val es = f(current.value)
  es.foreach(fun (x : u) => result.append(x))
  cursor = current.next
end */;
  while (({tuple_t* _tuple_8 = tuple_mk(_ctx, 2);
           tuple_set_type(_tuple_8, 0, (&(option_type)));
           tuple_set_type(_tuple_8, 1, (&(option_type)));
           option_t* _option_9 = (&(DEFAULT_NOTHING));
           tuple_set(_tuple_8, 0, ((encore_arg_t) {.p = _cursor_6}));
           tuple_set(_tuple_8, 1, ((encore_arg_t) {.p = _option_9}));
           int64_t _match_7;
           _enc__class_LinkedList_LinkedNode_t* __fst_10;
           _enc__class_LinkedList_LinkedNode_t* __snd_11;
           if ((({int64_t _tupleCheck_26;
                  _tupleCheck_26 = 1;
                  option_t* _tupleAccess_27 = tuple_get(_tuple_8, 0).p;
                  int64_t _optionCheck_29;
                  _optionCheck_29 = ((JUST == (*_tupleAccess_27).tag) && ({int64_t _varBinding_30;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_28 = (*_tupleAccess_27).val.p;
                                                                           __fst_10 = _optionVal_28;
                                                                           _varBinding_30 = 1; _varBinding_30;}));
                  _tupleCheck_26 = (_tupleCheck_26 && _optionCheck_29);
                  option_t* _tupleAccess_31 = tuple_get(_tuple_8, 1).p;
                  int64_t _optionCheck_33;
                  _optionCheck_33 = ((JUST == (*_tupleAccess_31).tag) && ({int64_t _varBinding_34;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_32 = (*_tupleAccess_31).val.p;
                                                                           __snd_11 = _optionVal_32;
                                                                           _varBinding_34 = 1; _varBinding_34;}));
                  _tupleCheck_26 = (_tupleCheck_26 && _optionCheck_33); _tupleCheck_26;}) && ({int64_t _binop_35 = (({ __fst_10;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_11;}))); _binop_35;})))
           {
             _match_7 = ((int64_t) ({int64_t _literal_12 = 1/*True*/; _literal_12;}));
           }
           else
           {
             if ((({int64_t _tupleCheck_18;
                    _tupleCheck_18 = 1;
                    option_t* _tupleAccess_19 = tuple_get(_tuple_8, 0).p;
                    int64_t _valueCheck_20;
                    _valueCheck_20 = (({option_t* _option_21 = (&(DEFAULT_NOTHING)); _option_21;}) == _tupleAccess_19);
                    _tupleCheck_18 = (_tupleCheck_18 && _valueCheck_20);
                    option_t* _tupleAccess_22 = tuple_get(_tuple_8, 1).p;
                    int64_t _valueCheck_23;
                    _valueCheck_23 = (({option_t* _option_24 = (&(DEFAULT_NOTHING)); _option_24;}) == _tupleAccess_22);
                    _tupleCheck_18 = (_tupleCheck_18 && _valueCheck_23); _tupleCheck_18;}) && ({int64_t _literal_25 = 1/*True*/; _literal_25;})))
             {
               _match_7 = ((int64_t) ({int64_t _literal_13 = 1/*True*/; _literal_13;}));
             }
             else
             {
               tuple_t* ___14;
               if ((({int64_t _varBinding_16;
                      ___14 = _tuple_8;
                      _varBinding_16 = 1; _varBinding_16;}) && ({int64_t _literal_17 = 1/*True*/; _literal_17;})))
               {
                 _match_7 = ((int64_t) ({int64_t _literal_15 = 0/*False*/; _literal_15;}));
               }
               else
               {
                 fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 367, column 18) ***\n");
                 exit(1);
               };
             };
           };
           int64_t _unary_36 = (! _match_7); _unary_36;}))
  {
    /* val current = match cursor with
                case Just(n) =>
                  n
                end
              
              end */;
    /* current = match cursor with
  case Just(n) =>
    n
  end

end */;
    _enc__class_LinkedList_LinkedNode_t* _match_37;
    _enc__class_LinkedList_LinkedNode_t* _n_38;
    if ((({int64_t _optionCheck_40;
           _optionCheck_40 = ((JUST == (*_cursor_6).tag) && ({int64_t _varBinding_41;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_39 = (*_cursor_6).val.p;
                                                              _n_38 = _optionVal_39;
                                                              _varBinding_41 = 1; _varBinding_41;})); _optionCheck_40;}) && ({int64_t _literal_42 = 1/*True*/; _literal_42;})))
    {
      _match_37 = ((_enc__class_LinkedList_LinkedNode_t*) ({ _n_38;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 368, column 21) ***\n");
      exit(1);
    };
    _enc__class_LinkedList_LinkedNode_t* _current_44 = _match_37;
    /* val es = f(current.value) */;
    /* es = f(current.value) */;
    value_t _tmp_46[] = {({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "value");
                           encore_arg_t _fieldacc_45 = (*_current_44)._enc__field_value; _fieldacc_45;})};
    ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
    _enc__class_LinkedList_LinkedList_t* _clos_47 = closure_call(_ctx, _enc__arg_f, _tmp_46).p;
    _enc__class_LinkedList_LinkedList_t* _es_49 = _clos_47;
    /* es.foreach(fun (x : u) => result.append(x)) */;
    check_receiver(_es_49, ".", "es", "foreach", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 373, column 7)");
    struct _enc__env_closure13* _enc__env_closure13 = encore_alloc((*_ctx), sizeof(struct _enc__env_closure13));
    (*_enc__env_closure13)._enc__field_result = _result_3;
    closure_t* _tmp_51 = closure_mk(_ctx, _enc__closure_fun_closure13, _enc__env_closure13, _enc__trace_closure13, NULL);
    pony_type_t* _tmp_53[] = {};
    void* _sync_method_call_50 = _enc__method_LinkedList_LinkedList_foreach(_ctx, _es_49, NULL, ((closure_t*) _tmp_51));
    /* cursor = current.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "next");
    option_t* _fieldacc_54 = (*_current_44)._enc__field_next;
    _cursor_6 = _fieldacc_54;
  };
  /* return(result) */;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "flatMap");
  return _result_3;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "flatMap");
  return ((_enc__class_LinkedList_LinkedList_t*) UNIT);
}


_enc__class_LinkedList_LinkedList_t* _enc__method_LinkedList_LinkedList_filter(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "filter");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* val result = new LinkedList[t]() */;
  /* result = new LinkedList[t]() */;
  _enc__class_LinkedList_LinkedList_t* _new_0 = _enc__constructor_LinkedList_LinkedList(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init_LinkedList_LinkedList(_new_0, _enc__type_t);
  _enc__method_LinkedList_LinkedList_init(_ctx, _new_0, NULL);
  _enc__class_LinkedList_LinkedList_t* _result_3 = _new_0;
  /* var cursor = this.first */;
  /* cursor = this.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_4 = (*_this)._enc__field_first;
  option_t* _cursor_6 = _fieldacc_4;
  /* while cursor != Nothing do
  val current = match cursor with
                  case Just(n) =>
                    n
                  end
                
                end
  if f(current.value) then
    result.append(current.value)
  end
  cursor = current.next
end */;
  while (({tuple_t* _tuple_8 = tuple_mk(_ctx, 2);
           tuple_set_type(_tuple_8, 0, (&(option_type)));
           tuple_set_type(_tuple_8, 1, (&(option_type)));
           option_t* _option_9 = (&(DEFAULT_NOTHING));
           tuple_set(_tuple_8, 0, ((encore_arg_t) {.p = _cursor_6}));
           tuple_set(_tuple_8, 1, ((encore_arg_t) {.p = _option_9}));
           int64_t _match_7;
           _enc__class_LinkedList_LinkedNode_t* __fst_10;
           _enc__class_LinkedList_LinkedNode_t* __snd_11;
           if ((({int64_t _tupleCheck_26;
                  _tupleCheck_26 = 1;
                  option_t* _tupleAccess_27 = tuple_get(_tuple_8, 0).p;
                  int64_t _optionCheck_29;
                  _optionCheck_29 = ((JUST == (*_tupleAccess_27).tag) && ({int64_t _varBinding_30;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_28 = (*_tupleAccess_27).val.p;
                                                                           __fst_10 = _optionVal_28;
                                                                           _varBinding_30 = 1; _varBinding_30;}));
                  _tupleCheck_26 = (_tupleCheck_26 && _optionCheck_29);
                  option_t* _tupleAccess_31 = tuple_get(_tuple_8, 1).p;
                  int64_t _optionCheck_33;
                  _optionCheck_33 = ((JUST == (*_tupleAccess_31).tag) && ({int64_t _varBinding_34;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_32 = (*_tupleAccess_31).val.p;
                                                                           __snd_11 = _optionVal_32;
                                                                           _varBinding_34 = 1; _varBinding_34;}));
                  _tupleCheck_26 = (_tupleCheck_26 && _optionCheck_33); _tupleCheck_26;}) && ({int64_t _binop_35 = (({ __fst_10;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_11;}))); _binop_35;})))
           {
             _match_7 = ((int64_t) ({int64_t _literal_12 = 1/*True*/; _literal_12;}));
           }
           else
           {
             if ((({int64_t _tupleCheck_18;
                    _tupleCheck_18 = 1;
                    option_t* _tupleAccess_19 = tuple_get(_tuple_8, 0).p;
                    int64_t _valueCheck_20;
                    _valueCheck_20 = (({option_t* _option_21 = (&(DEFAULT_NOTHING)); _option_21;}) == _tupleAccess_19);
                    _tupleCheck_18 = (_tupleCheck_18 && _valueCheck_20);
                    option_t* _tupleAccess_22 = tuple_get(_tuple_8, 1).p;
                    int64_t _valueCheck_23;
                    _valueCheck_23 = (({option_t* _option_24 = (&(DEFAULT_NOTHING)); _option_24;}) == _tupleAccess_22);
                    _tupleCheck_18 = (_tupleCheck_18 && _valueCheck_23); _tupleCheck_18;}) && ({int64_t _literal_25 = 1/*True*/; _literal_25;})))
             {
               _match_7 = ((int64_t) ({int64_t _literal_13 = 1/*True*/; _literal_13;}));
             }
             else
             {
               tuple_t* ___14;
               if ((({int64_t _varBinding_16;
                      ___14 = _tuple_8;
                      _varBinding_16 = 1; _varBinding_16;}) && ({int64_t _literal_17 = 1/*True*/; _literal_17;})))
               {
                 _match_7 = ((int64_t) ({int64_t _literal_15 = 0/*False*/; _literal_15;}));
               }
               else
               {
                 fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 349, column 18) ***\n");
                 exit(1);
               };
             };
           };
           int64_t _unary_36 = (! _match_7); _unary_36;}))
  {
    /* val current = match cursor with
                case Just(n) =>
                  n
                end
              
              end */;
    /* current = match cursor with
  case Just(n) =>
    n
  end

end */;
    _enc__class_LinkedList_LinkedNode_t* _match_37;
    _enc__class_LinkedList_LinkedNode_t* _n_38;
    if ((({int64_t _optionCheck_40;
           _optionCheck_40 = ((JUST == (*_cursor_6).tag) && ({int64_t _varBinding_41;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_39 = (*_cursor_6).val.p;
                                                              _n_38 = _optionVal_39;
                                                              _varBinding_41 = 1; _varBinding_41;})); _optionCheck_40;}) && ({int64_t _literal_42 = 1/*True*/; _literal_42;})))
    {
      _match_37 = ((_enc__class_LinkedList_LinkedNode_t*) ({ _n_38;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 350, column 21) ***\n");
      exit(1);
    };
    _enc__class_LinkedList_LinkedNode_t* _current_44 = _match_37;
    /* if f(current.value) then
  result.append(current.value)
end */;
    void* _ite_45;
    if (({value_t _tmp_47[] = {({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "value");
                                 encore_arg_t _fieldacc_46 = (*_current_44)._enc__field_value; _fieldacc_46;})};
          ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
          int64_t _clos_48 = closure_call(_ctx, _enc__arg_f, _tmp_47).i; _clos_48;}))
    {
      check_receiver(_result_3, ".", "result", "append", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 355, column 9)");
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "value");
      encore_arg_t _fieldacc_50 = (*_current_44)._enc__field_value;
      pony_type_t* _tmp_51[] = {};
      void* _sync_method_call_49 = _enc__method_LinkedList_LinkedList_append(_ctx, _result_3, NULL, _fieldacc_50);
      _ite_45 = ((void*) _sync_method_call_49);
    }
    else
    {
      UNIT;
      _ite_45 = ((void*) UNIT);
    };
    /* cursor = current.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "next");
    option_t* _fieldacc_52 = (*_current_44)._enc__field_next;
    _cursor_6 = _fieldacc_52;
  };
  /* return(result) */;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "filter");
  return _result_3;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "filter");
  return ((_enc__class_LinkedList_LinkedList_t*) UNIT);
}


_enc__class_LinkedList_LinkedList_t* _enc__method_LinkedList_LinkedList_map(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "map");
  pony_type_t* _enc__type_u = (runtimeType[0]);
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* val result = new LinkedList[u]() */;
  /* result = new LinkedList[u]() */;
  _enc__class_LinkedList_LinkedList_t* _new_0 = _enc__constructor_LinkedList_LinkedList(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init_LinkedList_LinkedList(_new_0, _enc__type_u);
  _enc__method_LinkedList_LinkedList_init(_ctx, _new_0, NULL);
  _enc__class_LinkedList_LinkedList_t* _result_3 = _new_0;
  /* var cursor = this.first */;
  /* cursor = this.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_4 = (*_this)._enc__field_first;
  option_t* _cursor_6 = _fieldacc_4;
  /* while cursor != Nothing do
  val current = match cursor with
                  case Just(n) =>
                    n
                  end
                
                end
  result.append(f(current.value))
  cursor = current.next
end */;
  while (({tuple_t* _tuple_8 = tuple_mk(_ctx, 2);
           tuple_set_type(_tuple_8, 0, (&(option_type)));
           tuple_set_type(_tuple_8, 1, (&(option_type)));
           option_t* _option_9 = (&(DEFAULT_NOTHING));
           tuple_set(_tuple_8, 0, ((encore_arg_t) {.p = _cursor_6}));
           tuple_set(_tuple_8, 1, ((encore_arg_t) {.p = _option_9}));
           int64_t _match_7;
           _enc__class_LinkedList_LinkedNode_t* __fst_10;
           _enc__class_LinkedList_LinkedNode_t* __snd_11;
           if ((({int64_t _tupleCheck_26;
                  _tupleCheck_26 = 1;
                  option_t* _tupleAccess_27 = tuple_get(_tuple_8, 0).p;
                  int64_t _optionCheck_29;
                  _optionCheck_29 = ((JUST == (*_tupleAccess_27).tag) && ({int64_t _varBinding_30;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_28 = (*_tupleAccess_27).val.p;
                                                                           __fst_10 = _optionVal_28;
                                                                           _varBinding_30 = 1; _varBinding_30;}));
                  _tupleCheck_26 = (_tupleCheck_26 && _optionCheck_29);
                  option_t* _tupleAccess_31 = tuple_get(_tuple_8, 1).p;
                  int64_t _optionCheck_33;
                  _optionCheck_33 = ((JUST == (*_tupleAccess_31).tag) && ({int64_t _varBinding_34;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_32 = (*_tupleAccess_31).val.p;
                                                                           __snd_11 = _optionVal_32;
                                                                           _varBinding_34 = 1; _varBinding_34;}));
                  _tupleCheck_26 = (_tupleCheck_26 && _optionCheck_33); _tupleCheck_26;}) && ({int64_t _binop_35 = (({ __fst_10;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_11;}))); _binop_35;})))
           {
             _match_7 = ((int64_t) ({int64_t _literal_12 = 1/*True*/; _literal_12;}));
           }
           else
           {
             if ((({int64_t _tupleCheck_18;
                    _tupleCheck_18 = 1;
                    option_t* _tupleAccess_19 = tuple_get(_tuple_8, 0).p;
                    int64_t _valueCheck_20;
                    _valueCheck_20 = (({option_t* _option_21 = (&(DEFAULT_NOTHING)); _option_21;}) == _tupleAccess_19);
                    _tupleCheck_18 = (_tupleCheck_18 && _valueCheck_20);
                    option_t* _tupleAccess_22 = tuple_get(_tuple_8, 1).p;
                    int64_t _valueCheck_23;
                    _valueCheck_23 = (({option_t* _option_24 = (&(DEFAULT_NOTHING)); _option_24;}) == _tupleAccess_22);
                    _tupleCheck_18 = (_tupleCheck_18 && _valueCheck_23); _tupleCheck_18;}) && ({int64_t _literal_25 = 1/*True*/; _literal_25;})))
             {
               _match_7 = ((int64_t) ({int64_t _literal_13 = 1/*True*/; _literal_13;}));
             }
             else
             {
               tuple_t* ___14;
               if ((({int64_t _varBinding_16;
                      ___14 = _tuple_8;
                      _varBinding_16 = 1; _varBinding_16;}) && ({int64_t _literal_17 = 1/*True*/; _literal_17;})))
               {
                 _match_7 = ((int64_t) ({int64_t _literal_15 = 0/*False*/; _literal_15;}));
               }
               else
               {
                 fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 335, column 18) ***\n");
                 exit(1);
               };
             };
           };
           int64_t _unary_36 = (! _match_7); _unary_36;}))
  {
    /* val current = match cursor with
                case Just(n) =>
                  n
                end
              
              end */;
    /* current = match cursor with
  case Just(n) =>
    n
  end

end */;
    _enc__class_LinkedList_LinkedNode_t* _match_37;
    _enc__class_LinkedList_LinkedNode_t* _n_38;
    if ((({int64_t _optionCheck_40;
           _optionCheck_40 = ((JUST == (*_cursor_6).tag) && ({int64_t _varBinding_41;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_39 = (*_cursor_6).val.p;
                                                              _n_38 = _optionVal_39;
                                                              _varBinding_41 = 1; _varBinding_41;})); _optionCheck_40;}) && ({int64_t _literal_42 = 1/*True*/; _literal_42;})))
    {
      _match_37 = ((_enc__class_LinkedList_LinkedNode_t*) ({ _n_38;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 336, column 21) ***\n");
      exit(1);
    };
    _enc__class_LinkedList_LinkedNode_t* _current_44 = _match_37;
    /* result.append(f(current.value)) */;
    check_receiver(_result_3, ".", "result", "append", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 339, column 7)");
    value_t _tmp_47[] = {({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "value");
                           encore_arg_t _fieldacc_46 = (*_current_44)._enc__field_value; _fieldacc_46;})};
    ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
    encore_arg_t _clos_48 = closure_call(_ctx, _enc__arg_f, _tmp_47);
    pony_type_t* _tmp_49[] = {};
    void* _sync_method_call_45 = _enc__method_LinkedList_LinkedList_append(_ctx, _result_3, NULL, _clos_48);
    /* cursor = current.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_44, "next");
    option_t* _fieldacc_50 = (*_current_44)._enc__field_next;
    _cursor_6 = _fieldacc_50;
  };
  /* return(result) */;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "map");
  return _result_3;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "map");
  return ((_enc__class_LinkedList_LinkedList_t*) UNIT);
}


void* _enc__method_LinkedList_LinkedList_foreach(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "foreach");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* var cursor = this.first */;
  /* cursor = this.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_0 = (*_this)._enc__field_first;
  option_t* _cursor_2 = _fieldacc_0;
  /* while cursor != Nothing do
  val current = match cursor with
                  case Just(n) =>
                    n
                  end
                
                end
  f(current.value)
  cursor = current.next
end */;
  while (({tuple_t* _tuple_4 = tuple_mk(_ctx, 2);
           tuple_set_type(_tuple_4, 0, (&(option_type)));
           tuple_set_type(_tuple_4, 1, (&(option_type)));
           option_t* _option_5 = (&(DEFAULT_NOTHING));
           tuple_set(_tuple_4, 0, ((encore_arg_t) {.p = _cursor_2}));
           tuple_set(_tuple_4, 1, ((encore_arg_t) {.p = _option_5}));
           int64_t _match_3;
           _enc__class_LinkedList_LinkedNode_t* __fst_6;
           _enc__class_LinkedList_LinkedNode_t* __snd_7;
           if ((({int64_t _tupleCheck_22;
                  _tupleCheck_22 = 1;
                  option_t* _tupleAccess_23 = tuple_get(_tuple_4, 0).p;
                  int64_t _optionCheck_25;
                  _optionCheck_25 = ((JUST == (*_tupleAccess_23).tag) && ({int64_t _varBinding_26;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_24 = (*_tupleAccess_23).val.p;
                                                                           __fst_6 = _optionVal_24;
                                                                           _varBinding_26 = 1; _varBinding_26;}));
                  _tupleCheck_22 = (_tupleCheck_22 && _optionCheck_25);
                  option_t* _tupleAccess_27 = tuple_get(_tuple_4, 1).p;
                  int64_t _optionCheck_29;
                  _optionCheck_29 = ((JUST == (*_tupleAccess_27).tag) && ({int64_t _varBinding_30;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_28 = (*_tupleAccess_27).val.p;
                                                                           __snd_7 = _optionVal_28;
                                                                           _varBinding_30 = 1; _varBinding_30;}));
                  _tupleCheck_22 = (_tupleCheck_22 && _optionCheck_29); _tupleCheck_22;}) && ({int64_t _binop_31 = (({ __fst_6;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_7;}))); _binop_31;})))
           {
             _match_3 = ((int64_t) ({int64_t _literal_8 = 1/*True*/; _literal_8;}));
           }
           else
           {
             if ((({int64_t _tupleCheck_14;
                    _tupleCheck_14 = 1;
                    option_t* _tupleAccess_15 = tuple_get(_tuple_4, 0).p;
                    int64_t _valueCheck_16;
                    _valueCheck_16 = (({option_t* _option_17 = (&(DEFAULT_NOTHING)); _option_17;}) == _tupleAccess_15);
                    _tupleCheck_14 = (_tupleCheck_14 && _valueCheck_16);
                    option_t* _tupleAccess_18 = tuple_get(_tuple_4, 1).p;
                    int64_t _valueCheck_19;
                    _valueCheck_19 = (({option_t* _option_20 = (&(DEFAULT_NOTHING)); _option_20;}) == _tupleAccess_18);
                    _tupleCheck_14 = (_tupleCheck_14 && _valueCheck_19); _tupleCheck_14;}) && ({int64_t _literal_21 = 1/*True*/; _literal_21;})))
             {
               _match_3 = ((int64_t) ({int64_t _literal_9 = 1/*True*/; _literal_9;}));
             }
             else
             {
               tuple_t* ___10;
               if ((({int64_t _varBinding_12;
                      ___10 = _tuple_4;
                      _varBinding_12 = 1; _varBinding_12;}) && ({int64_t _literal_13 = 1/*True*/; _literal_13;})))
               {
                 _match_3 = ((int64_t) ({int64_t _literal_11 = 0/*False*/; _literal_11;}));
               }
               else
               {
                 fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 321, column 18) ***\n");
                 exit(1);
               };
             };
           };
           int64_t _unary_32 = (! _match_3); _unary_32;}))
  {
    /* val current = match cursor with
                case Just(n) =>
                  n
                end
              
              end */;
    /* current = match cursor with
  case Just(n) =>
    n
  end

end */;
    _enc__class_LinkedList_LinkedNode_t* _match_33;
    _enc__class_LinkedList_LinkedNode_t* _n_34;
    if ((({int64_t _optionCheck_36;
           _optionCheck_36 = ((JUST == (*_cursor_2).tag) && ({int64_t _varBinding_37;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_35 = (*_cursor_2).val.p;
                                                              _n_34 = _optionVal_35;
                                                              _varBinding_37 = 1; _varBinding_37;})); _optionCheck_36;}) && ({int64_t _literal_38 = 1/*True*/; _literal_38;})))
    {
      _match_33 = ((_enc__class_LinkedList_LinkedNode_t*) ({ _n_34;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 322, column 21) ***\n");
      exit(1);
    };
    _enc__class_LinkedList_LinkedNode_t* _current_40 = _match_33;
    /* f(current.value) */;
    value_t _tmp_42[] = {({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_40, "value");
                           encore_arg_t _fieldacc_41 = (*_current_40)._enc__field_value; _fieldacc_41;})};
    ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
    closure_call(_ctx, _enc__arg_f, _tmp_42).p;
    /* cursor = current.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_40, "next");
    option_t* _fieldacc_43 = (*_current_40)._enc__field_next;
    _cursor_2 = _fieldacc_43;
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "foreach");
  return UNIT;
}


_enc__class_LinkedList_LinkedListIterator_t* _enc__method_LinkedList_LinkedList_iterator(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "iterator");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  _enc__class_LinkedList_LinkedListIterator_t* _new_0 = _enc__constructor_LinkedList_LinkedListIterator(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init_LinkedList_LinkedListIterator(_new_0, _enc__type_t);
  _enc__method_LinkedList_LinkedListIterator_init(_ctx, _new_0, NULL, _this);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "iterator");
  return ((_enc__class_LinkedList_LinkedListIterator_t*) _new_0);
}


int64_t _enc__method_LinkedList_LinkedList_size(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
  int64_t _fieldacc_0 = (*_this)._enc__field_size;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
  return ((int64_t) _fieldacc_0);
}


void* _enc__method_LinkedList_LinkedList_drop(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, _enc__class_LinkedList_LinkedNode_t* _enc__arg_node)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "drop");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* match node.prev with
  case Just(n) =>
    n.next = node.next
  end
  case Nothing =>
    this.first = node.next
  end

end */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "prev");
  option_t* _fieldacc_1 = (*_enc__arg_node)._enc__field_prev;
  void* _match_0;
  _enc__class_LinkedList_LinkedNode_t* _n_2;
  if ((({int64_t _optionCheck_9;
         _optionCheck_9 = ((JUST == (*_fieldacc_1).tag) && ({int64_t _varBinding_10;
                                                             _enc__class_LinkedList_LinkedNode_t* _optionVal_8 = (*_fieldacc_1).val.p;
                                                             _n_2 = _optionVal_8;
                                                             _varBinding_10 = 1; _varBinding_10;})); _optionCheck_9;}) && ({int64_t _literal_11 = 1/*True*/; _literal_11;})))
  {
    _match_0 = ((void*) ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "next");
                          option_t* _fieldacc_3 = (*_enc__arg_node)._enc__field_next;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_n_2, "next"); _n_2;}))._enc__field_next = _fieldacc_3; UNIT;}));
  }
  else
  {
    if ((({int64_t _valueCheck_5;
           _valueCheck_5 = (({option_t* _option_6 = (&(DEFAULT_NOTHING)); _option_6;}) == _fieldacc_1); _valueCheck_5;}) && ({int64_t _literal_7 = 1/*True*/; _literal_7;})))
    {
      _match_0 = ((void*) ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "next");
                            option_t* _fieldacc_4 = (*_enc__arg_node)._enc__field_next;
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "first"); _this;}))._enc__field_first = _fieldacc_4; UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 288, column 5) ***\n");
      exit(1);
    };
  };
  /* match node.next with
  case Just(n) =>
    n.prev = node.prev
  end
  case Nothing =>
    this.last = node.prev
  end

end */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "next");
  option_t* _fieldacc_13 = (*_enc__arg_node)._enc__field_next;
  void* _match_12;
  _enc__class_LinkedList_LinkedNode_t* _n_14;
  if ((({int64_t _optionCheck_21;
         _optionCheck_21 = ((JUST == (*_fieldacc_13).tag) && ({int64_t _varBinding_22;
                                                               _enc__class_LinkedList_LinkedNode_t* _optionVal_20 = (*_fieldacc_13).val.p;
                                                               _n_14 = _optionVal_20;
                                                               _varBinding_22 = 1; _varBinding_22;})); _optionCheck_21;}) && ({int64_t _literal_23 = 1/*True*/; _literal_23;})))
  {
    _match_12 = ((void*) ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "prev");
                           option_t* _fieldacc_15 = (*_enc__arg_node)._enc__field_prev;
                           (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_n_14, "prev"); _n_14;}))._enc__field_prev = _fieldacc_15; UNIT;}));
  }
  else
  {
    if ((({int64_t _valueCheck_17;
           _valueCheck_17 = (({option_t* _option_18 = (&(DEFAULT_NOTHING)); _option_18;}) == _fieldacc_13); _valueCheck_17;}) && ({int64_t _literal_19 = 1/*True*/; _literal_19;})))
    {
      _match_12 = ((void*) ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "prev");
                             option_t* _fieldacc_16 = (*_enc__arg_node)._enc__field_prev;
                             (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "last"); _this;}))._enc__field_last = _fieldacc_16; UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 296, column 5) ***\n");
      exit(1);
    };
  };
  /* this.size = this.size - 1 */;
  int64_t _binop_26 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
                         int64_t _fieldacc_24 = (*_this)._enc__field_size; _fieldacc_24;}) - ({int64_t _literal_25 = 1; _literal_25;}));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "size"); _this;}))._enc__field_size = _binop_26;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "drop");
  return UNIT;
}


option_t* _enc__method_LinkedList_LinkedList_remove_front(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_front");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_1 = (*_this)._enc__field_first;
  option_t* _match_0;
  _enc__class_LinkedList_LinkedNode_t* _f_2;
  if ((({int64_t _optionCheck_12;
         _optionCheck_12 = ((JUST == (*_fieldacc_1).tag) && ({int64_t _varBinding_13;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_11 = (*_fieldacc_1).val.p;
                                                              _f_2 = _optionVal_11;
                                                              _varBinding_13 = 1; _varBinding_13;})); _optionCheck_12;}) && ({int64_t _literal_14 = 1/*True*/; _literal_14;})))
  {
    _match_0 = ((option_t*) ({/* this.drop(f) */;
                              check_receiver(_this, ".", "this", "drop", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 280, column 9)");
                              pony_type_t* _tmp_4[] = {};
                              void* _sync_method_call_3 = _enc__method_LinkedList_LinkedList_drop(_ctx, _this, NULL, _f_2);
                              /* return(Just(f.value)) */;
                              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_f_2, "value");
                              encore_arg_t _fieldacc_5 = (*_f_2)._enc__field_value;
                              option_t* _option_6 = option_mk(_ctx, JUST, _fieldacc_5, _enc__type_t);
                              ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_front");
                              return _option_6; UNIT;}));
  }
  else
  {
    if ((({int64_t _valueCheck_8;
           _valueCheck_8 = (({option_t* _option_9 = (&(DEFAULT_NOTHING)); _option_9;}) == _fieldacc_1); _valueCheck_8;}) && ({int64_t _literal_10 = 1/*True*/; _literal_10;})))
    {
      _match_0 = ((option_t*) ({option_t* _option_7 = (&(DEFAULT_NOTHING));
                                ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_front");
                                return _option_7; UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 278, column 5) ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_front");
  return ((option_t*) _match_0);
}


option_t* _enc__method_LinkedList_LinkedList_remove_back(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_back");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "last");
  option_t* _fieldacc_1 = (*_this)._enc__field_last;
  option_t* _match_0;
  _enc__class_LinkedList_LinkedNode_t* _l_2;
  if ((({int64_t _optionCheck_12;
         _optionCheck_12 = ((JUST == (*_fieldacc_1).tag) && ({int64_t _varBinding_13;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_11 = (*_fieldacc_1).val.p;
                                                              _l_2 = _optionVal_11;
                                                              _varBinding_13 = 1; _varBinding_13;})); _optionCheck_12;}) && ({int64_t _literal_14 = 1/*True*/; _literal_14;})))
  {
    _match_0 = ((option_t*) ({/* this.drop(l) */;
                              check_receiver(_this, ".", "this", "drop", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 270, column 9)");
                              pony_type_t* _tmp_4[] = {};
                              void* _sync_method_call_3 = _enc__method_LinkedList_LinkedList_drop(_ctx, _this, NULL, _l_2);
                              /* return(Just(l.value)) */;
                              ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_l_2, "value");
                              encore_arg_t _fieldacc_5 = (*_l_2)._enc__field_value;
                              option_t* _option_6 = option_mk(_ctx, JUST, _fieldacc_5, _enc__type_t);
                              ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_back");
                              return _option_6; UNIT;}));
  }
  else
  {
    if ((({int64_t _valueCheck_8;
           _valueCheck_8 = (({option_t* _option_9 = (&(DEFAULT_NOTHING)); _option_9;}) == _fieldacc_1); _valueCheck_8;}) && ({int64_t _literal_10 = 1/*True*/; _literal_10;})))
    {
      _match_0 = ((option_t*) ({option_t* _option_7 = (&(DEFAULT_NOTHING));
                                ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_back");
                                return _option_7; UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 268, column 5) ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_back");
  return ((option_t*) _match_0);
}


void* _enc__method_LinkedList_LinkedList_remove_all(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, encore_arg_t _enc__arg_value)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_all");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* var cursor = this.first */;
  /* cursor = this.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_0 = (*_this)._enc__field_first;
  option_t* _cursor_2 = _fieldacc_0;
  /* while cursor != Nothing do
  val current = match cursor with
                  case Just(n) =>
                    n
                  end
                
                end
  cursor = current.next
  if current.value == value then
    this.drop(current)
  end
end */;
  while (({tuple_t* _tuple_4 = tuple_mk(_ctx, 2);
           tuple_set_type(_tuple_4, 0, (&(option_type)));
           tuple_set_type(_tuple_4, 1, (&(option_type)));
           option_t* _option_5 = (&(DEFAULT_NOTHING));
           tuple_set(_tuple_4, 0, ((encore_arg_t) {.p = _cursor_2}));
           tuple_set(_tuple_4, 1, ((encore_arg_t) {.p = _option_5}));
           int64_t _match_3;
           _enc__class_LinkedList_LinkedNode_t* __fst_6;
           _enc__class_LinkedList_LinkedNode_t* __snd_7;
           if ((({int64_t _tupleCheck_22;
                  _tupleCheck_22 = 1;
                  option_t* _tupleAccess_23 = tuple_get(_tuple_4, 0).p;
                  int64_t _optionCheck_25;
                  _optionCheck_25 = ((JUST == (*_tupleAccess_23).tag) && ({int64_t _varBinding_26;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_24 = (*_tupleAccess_23).val.p;
                                                                           __fst_6 = _optionVal_24;
                                                                           _varBinding_26 = 1; _varBinding_26;}));
                  _tupleCheck_22 = (_tupleCheck_22 && _optionCheck_25);
                  option_t* _tupleAccess_27 = tuple_get(_tuple_4, 1).p;
                  int64_t _optionCheck_29;
                  _optionCheck_29 = ((JUST == (*_tupleAccess_27).tag) && ({int64_t _varBinding_30;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_28 = (*_tupleAccess_27).val.p;
                                                                           __snd_7 = _optionVal_28;
                                                                           _varBinding_30 = 1; _varBinding_30;}));
                  _tupleCheck_22 = (_tupleCheck_22 && _optionCheck_29); _tupleCheck_22;}) && ({int64_t _binop_31 = (({ __fst_6;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_7;}))); _binop_31;})))
           {
             _match_3 = ((int64_t) ({int64_t _literal_8 = 1/*True*/; _literal_8;}));
           }
           else
           {
             if ((({int64_t _tupleCheck_14;
                    _tupleCheck_14 = 1;
                    option_t* _tupleAccess_15 = tuple_get(_tuple_4, 0).p;
                    int64_t _valueCheck_16;
                    _valueCheck_16 = (({option_t* _option_17 = (&(DEFAULT_NOTHING)); _option_17;}) == _tupleAccess_15);
                    _tupleCheck_14 = (_tupleCheck_14 && _valueCheck_16);
                    option_t* _tupleAccess_18 = tuple_get(_tuple_4, 1).p;
                    int64_t _valueCheck_19;
                    _valueCheck_19 = (({option_t* _option_20 = (&(DEFAULT_NOTHING)); _option_20;}) == _tupleAccess_18);
                    _tupleCheck_14 = (_tupleCheck_14 && _valueCheck_19); _tupleCheck_14;}) && ({int64_t _literal_21 = 1/*True*/; _literal_21;})))
             {
               _match_3 = ((int64_t) ({int64_t _literal_9 = 1/*True*/; _literal_9;}));
             }
             else
             {
               tuple_t* ___10;
               if ((({int64_t _varBinding_12;
                      ___10 = _tuple_4;
                      _varBinding_12 = 1; _varBinding_12;}) && ({int64_t _literal_13 = 1/*True*/; _literal_13;})))
               {
                 _match_3 = ((int64_t) ({int64_t _literal_11 = 0/*False*/; _literal_11;}));
               }
               else
               {
                 fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 251, column 18) ***\n");
                 exit(1);
               };
             };
           };
           int64_t _unary_32 = (! _match_3); _unary_32;}))
  {
    /* val current = match cursor with
                case Just(n) =>
                  n
                end
              
              end */;
    /* current = match cursor with
  case Just(n) =>
    n
  end

end */;
    _enc__class_LinkedList_LinkedNode_t* _match_33;
    _enc__class_LinkedList_LinkedNode_t* _n_34;
    if ((({int64_t _optionCheck_36;
           _optionCheck_36 = ((JUST == (*_cursor_2).tag) && ({int64_t _varBinding_37;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_35 = (*_cursor_2).val.p;
                                                              _n_34 = _optionVal_35;
                                                              _varBinding_37 = 1; _varBinding_37;})); _optionCheck_36;}) && ({int64_t _literal_38 = 1/*True*/; _literal_38;})))
    {
      _match_33 = ((_enc__class_LinkedList_LinkedNode_t*) ({ _n_34;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 253, column 21) ***\n");
      exit(1);
    };
    _enc__class_LinkedList_LinkedNode_t* _current_40 = _match_33;
    /* cursor = current.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_40, "next");
    option_t* _fieldacc_41 = (*_current_40)._enc__field_next;
    _cursor_2 = _fieldacc_41;
    /* if current.value == value then
  this.drop(current)
end */;
    void* _ite_42;
    if (({int64_t _binop_44 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_40, "value");
                                 encore_arg_t _fieldacc_43 = (*_current_40)._enc__field_value; _fieldacc_43.p;}) == ({ _enc__arg_value.p;})); _binop_44;}))
    {
      check_receiver(_this, ".", "this", "drop", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 262, column 9)");
      pony_type_t* _tmp_46[] = {};
      void* _sync_method_call_45 = _enc__method_LinkedList_LinkedList_drop(_ctx, _this, NULL, _current_40);
      _ite_42 = ((void*) _sync_method_call_45);
    }
    else
    {
      UNIT;
      _ite_42 = ((void*) UNIT);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove_all");
  return UNIT;
}


option_t* _enc__method_LinkedList_LinkedList_remove(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedList_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_index)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedList_t*) _this))._enc__type_t;
  /* var counter = 0 */;
  /* counter = 0 */;
  int64_t _literal_0 = 0;
  int64_t _counter_2 = _literal_0;
  /* var cursor = this.first */;
  /* cursor = this.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "first");
  option_t* _fieldacc_3 = (*_this)._enc__field_first;
  option_t* _cursor_5 = _fieldacc_3;
  /* while cursor != Nothing do
  val current = unjust(cursor)
  if counter == index then
    this.drop(current)
    return(Just(current.value))
  end
  cursor = current.next
  counter += 1
end */;
  while (({tuple_t* _tuple_7 = tuple_mk(_ctx, 2);
           tuple_set_type(_tuple_7, 0, (&(option_type)));
           tuple_set_type(_tuple_7, 1, (&(option_type)));
           option_t* _option_8 = (&(DEFAULT_NOTHING));
           tuple_set(_tuple_7, 0, ((encore_arg_t) {.p = _cursor_5}));
           tuple_set(_tuple_7, 1, ((encore_arg_t) {.p = _option_8}));
           int64_t _match_6;
           _enc__class_LinkedList_LinkedNode_t* __fst_9;
           _enc__class_LinkedList_LinkedNode_t* __snd_10;
           if ((({int64_t _tupleCheck_25;
                  _tupleCheck_25 = 1;
                  option_t* _tupleAccess_26 = tuple_get(_tuple_7, 0).p;
                  int64_t _optionCheck_28;
                  _optionCheck_28 = ((JUST == (*_tupleAccess_26).tag) && ({int64_t _varBinding_29;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_27 = (*_tupleAccess_26).val.p;
                                                                           __fst_9 = _optionVal_27;
                                                                           _varBinding_29 = 1; _varBinding_29;}));
                  _tupleCheck_25 = (_tupleCheck_25 && _optionCheck_28);
                  option_t* _tupleAccess_30 = tuple_get(_tuple_7, 1).p;
                  int64_t _optionCheck_32;
                  _optionCheck_32 = ((JUST == (*_tupleAccess_30).tag) && ({int64_t _varBinding_33;
                                                                           _enc__class_LinkedList_LinkedNode_t* _optionVal_31 = (*_tupleAccess_30).val.p;
                                                                           __snd_10 = _optionVal_31;
                                                                           _varBinding_33 = 1; _varBinding_33;}));
                  _tupleCheck_25 = (_tupleCheck_25 && _optionCheck_32); _tupleCheck_25;}) && ({int64_t _binop_34 = (({ __fst_9;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_10;}))); _binop_34;})))
           {
             _match_6 = ((int64_t) ({int64_t _literal_11 = 1/*True*/; _literal_11;}));
           }
           else
           {
             if ((({int64_t _tupleCheck_17;
                    _tupleCheck_17 = 1;
                    option_t* _tupleAccess_18 = tuple_get(_tuple_7, 0).p;
                    int64_t _valueCheck_19;
                    _valueCheck_19 = (({option_t* _option_20 = (&(DEFAULT_NOTHING)); _option_20;}) == _tupleAccess_18);
                    _tupleCheck_17 = (_tupleCheck_17 && _valueCheck_19);
                    option_t* _tupleAccess_21 = tuple_get(_tuple_7, 1).p;
                    int64_t _valueCheck_22;
                    _valueCheck_22 = (({option_t* _option_23 = (&(DEFAULT_NOTHING)); _option_23;}) == _tupleAccess_21);
                    _tupleCheck_17 = (_tupleCheck_17 && _valueCheck_22); _tupleCheck_17;}) && ({int64_t _literal_24 = 1/*True*/; _literal_24;})))
             {
               _match_6 = ((int64_t) ({int64_t _literal_12 = 1/*True*/; _literal_12;}));
             }
             else
             {
               tuple_t* ___13;
               if ((({int64_t _varBinding_15;
                      ___13 = _tuple_7;
                      _varBinding_15 = 1; _varBinding_15;}) && ({int64_t _literal_16 = 1/*True*/; _literal_16;})))
               {
                 _match_6 = ((int64_t) ({int64_t _literal_14 = 0/*False*/; _literal_14;}));
               }
               else
               {
                 fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 234, column 18) ***\n");
                 exit(1);
               };
             };
           };
           int64_t _unary_35 = (! _match_6); _unary_35;}))
  {
    /* val current = unjust(cursor) */;
    /* current = unjust(cursor) */;
    ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Data.Maybe.unjust");
    pony_type_t* _tmp_36[] = {(&(_enc__class_LinkedList_LinkedNode_type))};
    _enc__class_LinkedList_LinkedNode_t* _fun_call_37 = _enc__global_fun_Maybeunjust(_ctx, _tmp_36, ((option_t*) _cursor_5)).p;
    _enc__class_LinkedList_LinkedNode_t* _current_39 = _fun_call_37;
    /* if counter == index then
  this.drop(current)
  return(Just(current.value))
end */;
    void* _ite_40;
    if (({int64_t _binop_41 = (({ _counter_2;}) == ({ _enc__arg_index;})); _binop_41;}))
    {
      /* this.drop(current) */;
      check_receiver(_this, ".", "this", "drop", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 238, column 9)");
      pony_type_t* _tmp_43[] = {};
      void* _sync_method_call_42 = _enc__method_LinkedList_LinkedList_drop(_ctx, _this, NULL, _current_39);
      /* return(Just(current.value)) */;
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_39, "value");
      encore_arg_t _fieldacc_44 = (*_current_39)._enc__field_value;
      option_t* _option_45 = option_mk(_ctx, JUST, _fieldacc_44, _enc__type_t);
      ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
      return _option_45;
      _ite_40 = ((void*) UNIT);
    }
    else
    {
      UNIT;
      _ite_40 = ((void*) UNIT);
    };
    /* cursor = current.next */;
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_current_39, "next");
    option_t* _fieldacc_46 = (*_current_39)._enc__field_next;
    _cursor_5 = _fieldacc_46;
    /* counter += 1 */;
    int64_t _binop_48 = (({ _counter_2;}) + ({int64_t _literal_47 = 1; _literal_47;}));
    _counter_2 = _binop_48;
  };
  /* Nothing */;
  option_t* _option_49 = (&(DEFAULT_NOTHING));
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
  return ((option_t*) _option_49);
}


struct _enc__env_closure14
{
  _enc__class_LinkedList_LinkedList_t* _enc__field_this;
  po