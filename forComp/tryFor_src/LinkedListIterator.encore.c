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


void _enc__type_init_LinkedList_LinkedListIterator(_enc__class_LinkedList_LinkedListIterator_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  _this->_enc__type_t = va_arg(params, pony_type_t *);
  va_end(params);
}


void _enc__trace_LinkedList_LinkedListIterator(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class_LinkedList_LinkedListIterator_t* _this = p;
  pony_type_t* _enc__type_t = _this->_enc__type_t;
  option_t* _enc__field_next_node = _this->_enc__field_next_node;
  encore_trace_object((*_ctx), _enc__field_next_node, option_trace);
  _enc__class_LinkedList_LinkedList_t* _enc__field_internal_list = _this->_enc__field_internal_list;
  encore_trace_object((*_ctx), _enc__field_internal_list, _enc__trace_LinkedList_LinkedList);
}


_enc__class_LinkedList_LinkedListIterator_t* _enc__constructor_LinkedList_LinkedListIterator(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class_LinkedList_LinkedListIterator_t* _this = ((_enc__class_LinkedList_LinkedListIterator_t*) encore_alloc((*_ctx), sizeof(_enc__class_LinkedList_LinkedListIterator_t)));
  _this->_enc__self_type = (&(_enc__class_LinkedList_LinkedListIterator_type));
  return _this;
}


encore_arg_t _enc__method_LinkedList_LinkedListIterator_next(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedListIterator_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedListIterator_t*) _this))._enc__type_t;
  /* match this.next_node with
  case Just(n) =>
    val next = this.next_node
    this.next_node = n.next
    match next with
      case Just(n) =>
        return(n.value)
      end
    
    end
  end

end */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "next_node");
  option_t* _fieldacc_1 = (*_this)._enc__field_next_node;
  void* _match_0;
  _enc__class_LinkedList_LinkedNode_t* _n_2;
  if ((({int64_t _optionCheck_15;
         _optionCheck_15 = ((JUST == (*_fieldacc_1).tag) && ({int64_t _varBinding_16;
                                                              _enc__class_LinkedList_LinkedNode_t* _optionVal_14 = (*_fieldacc_1).val.p;
                                                              _n_2 = _optionVal_14;
                                                              _varBinding_16 = 1; _varBinding_16;})); _optionCheck_15;}) && ({int64_t _literal_17 = 1/*True*/; _literal_17;})))
  {
    _match_0 = ((void*) ({/* val next = this.next_node */;
                          /* next = this.next_node */;
                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "next_node");
                          option_t* _fieldacc_3 = (*_this)._enc__field_next_node;
                          option_t* _next_5 = _fieldacc_3;
                          /* this.next_node = n.next */;
                          ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_n_2, "next");
                          option_t* _fieldacc_6 = (*_n_2)._enc__field_next;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "next_node"); _this;}))._enc__field_next_node = _fieldacc_6;
                          /* match next with
  case Just(n) =>
    return(n.value)
  end

end */;
                          void* _match_7;
                          _enc__class_LinkedList_LinkedNode_t* _n_8;
                          if ((({int64_t _optionCheck_11;
                                 _optionCheck_11 = ((JUST == (*_next_5).tag) && ({int64_t _varBinding_12;
                                                                                  _enc__class_LinkedList_LinkedNode_t* _optionVal_10 = (*_next_5).val.p;
                                                                                  _n_8 = _optionVal_10;
                                                                                  _varBinding_12 = 1; _varBinding_12;})); _optionCheck_11;}) && ({int64_t _literal_13 = 1/*True*/; _literal_13;})))
                          {
                            _match_7 = ((void*) ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_n_8, "value");
                                                  encore_arg_t _fieldacc_9 = (*_n_8)._enc__field_value;
                                                  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
                                                  return _fieldacc_9; UNIT;}));
                          }
                          else
                          {
                            fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 401, column 9) ***\n");
                            exit(1);
                          }; _match_7;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 397, column 5) ***\n");
    exit(1);
  };
  /* abort("No next element") */;
  /* abort("No next element") */;
  char* _literal_18 = "No next element";
  fprintf(stderr, "%s\n", _literal_18);
  /* abort("No next element") */;
  fprintf(stderr, "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 406, column 5)\n");
  /* abort("No next element") */;
  abort();
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
  return ((encore_arg_t) UNIT);
}


int64_t _enc__method_LinkedList_LinkedListIterator_has_next(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedListIterator_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "has_next");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedListIterator_t*) _this))._enc__type_t;
  tuple_t* _tuple_1 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_1, 0, (&(option_type)));
  tuple_set_type(_tuple_1, 1, (&(option_type)));
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "next_node");
  option_t* _fieldacc_2 = (*_this)._enc__field_next_node;
  option_t* _option_3 = (&(DEFAULT_NOTHING));
  tuple_set(_tuple_1, 0, ((encore_arg_t) {.p = _fieldacc_2}));
  tuple_set(_tuple_1, 1, ((encore_arg_t) {.p = _option_3}));
  int64_t _match_0;
  _enc__class_LinkedList_LinkedNode_t* __fst_4;
  _enc__class_LinkedList_LinkedNode_t* __snd_5;
  if ((({int64_t _tupleCheck_20;
         _tupleCheck_20 = 1;
         option_t* _tupleAccess_21 = tuple_get(_tuple_1, 0).p;
         int64_t _optionCheck_23;
         _optionCheck_23 = ((JUST == (*_tupleAccess_21).tag) && ({int64_t _varBinding_24;
                                                                  _enc__class_LinkedList_LinkedNode_t* _optionVal_22 = (*_tupleAccess_21).val.p;
                                                                  __fst_4 = _optionVal_22;
                                                                  _varBinding_24 = 1; _varBinding_24;}));
         _tupleCheck_20 = (_tupleCheck_20 && _optionCheck_23);
         option_t* _tupleAccess_25 = tuple_get(_tuple_1, 1).p;
         int64_t _optionCheck_27;
         _optionCheck_27 = ((JUST == (*_tupleAccess_25).tag) && ({int64_t _varBinding_28;
                                                                  _enc__class_LinkedList_LinkedNode_t* _optionVal_26 = (*_tupleAccess_25).val.p;
                                                                  __snd_5 = _optionVal_26;
                                                                  _varBinding_28 = 1; _varBinding_28;}));
         _tupleCheck_20 = (_tupleCheck_20 && _optionCheck_27); _tupleCheck_20;}) && ({int64_t _binop_29 = (({ __fst_4;}) != ((_enc__class_LinkedList_LinkedNode_t*) ({ __snd_5;}))); _binop_29;})))
  {
    _match_0 = ((int64_t) ({int64_t _literal_6 = 1/*True*/; _literal_6;}));
  }
  else
  {
    if ((({int64_t _tupleCheck_12;
           _tupleCheck_12 = 1;
           option_t* _tupleAccess_13 = tuple_get(_tuple_1, 0).p;
           int64_t _valueCheck_14;
           _valueCheck_14 = (({option_t* _option_15 = (&(DEFAULT_NOTHING)); _option_15;}) == _tupleAccess_13);
           _tupleCheck_12 = (_tupleCheck_12 && _valueCheck_14);
           option_t* _tupleAccess_16 = tuple_get(_tuple_1, 1).p;
           int64_t _valueCheck_17;
           _valueCheck_17 = (({option_t* _option_18 = (&(DEFAULT_NOTHING)); _option_18;}) == _tupleAccess_16);
           _tupleCheck_12 = (_tupleCheck_12 && _valueCheck_17); _tupleCheck_12;}) && ({int64_t _literal_19 = 1/*True*/; _literal_19;})))
    {
      _match_0 = ((int64_t) ({int64_t _literal_7 = 1/*True*/; _literal_7;}));
    }
    else
    {
      tuple_t* ___8;
      if ((({int64_t _varBinding_10;
             ___8 = _tuple_1;
             _varBinding_10 = 1; _varBinding_10;}) && ({int64_t _literal_11 = 1/*True*/; _literal_11;})))
      {
        _match_0 = ((int64_t) ({int64_t _literal_9 = 0/*False*/; _literal_9;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 393, column 27) ***\n");
        exit(1);
      };
    };
  };
  int64_t _unary_30 = (! _match_0);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "has_next");
  return _unary_30;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "has_next");
  return ((int64_t) UNIT);
}


void* _enc__method_LinkedList_LinkedListIterator_init(pony_ctx_t** _ctx, _enc__class_LinkedList_LinkedListIterator_t* _this, pony_type_t** runtimeType, _enc__class_LinkedList_LinkedList_t* _enc__arg_list)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  pony_type_t* _enc__type_t = (*((_enc__class_LinkedList_LinkedListIterator_t*) _this))._enc__type_t;
  /* do
  this.internal_list = list.clone()
  this.next_node = this.internal_list.first
end */;
  /* this.internal_list = list.clone() */;
  check_receiver(_enc__arg_list, ".", "list", "clone", "\"/home/joy/encore/modules/standard/Collections/Mutable/LinkedList.enc\" (line 388, column 26)");
  pony_type_t* _tmp_1[] = {};
  _enc__class_LinkedList_LinkedList_t* _sync_method_call_0 = _enc__method_LinkedList_LinkedList_clone(_ctx, _enc__arg_list, NULL);
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "internal_list"); _this;}))._enc__field_internal_list = _sync_method_call_0;
  /* this.next_node = this.internal_list.first */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "internal_list");
  _enc__class_LinkedList_LinkedList_t* _fieldacc_2 = (*_this)._enc__field_internal_list;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_2, "first");
  option_t* _fieldacc_3 = (*_fieldacc_2)._enc__field_first;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "next_node"); _this;}))._enc__field_next_node = _fieldacc_3;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class_LinkedList_LinkedListIterator_type = {.id=_ENC__ID_LinkedList_LinkedListIterator, .size=sizeof(_enc__class_LinkedList_LinkedListIterator_t), .trace=_enc__trace_LinkedList_LinkedListIterator, .vtable=trait_method_selector};
