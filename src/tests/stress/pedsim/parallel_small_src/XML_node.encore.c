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


void _enc__type_init__Ped_util_XML_XML_node(_enc__class__Ped_util_XML_XML_node_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_XML_XML_node(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_XML_XML_node_t* _this = p;
  array_t* _enc__field_atribs = _this->_enc__field_atribs;
  encore_trace_object((*_ctx), _enc__field_atribs, array_trace);
  _enc__class_String_String_t* _enc__field_name = _this->_enc__field_name;
  encore_trace_object((*_ctx), _enc__field_name, _enc__trace_String_String);
  array_t* _enc__field_comments = _this->_enc__field_comments;
  encore_trace_object((*_ctx), _enc__field_comments, array_trace);
  array_t* _enc__field_children = _this->_enc__field_children;
  encore_trace_object((*_ctx), _enc__field_children, array_trace);
  _enc__class_String_String_t* _enc__field_s_rep = _this->_enc__field_s_rep;
  encore_trace_object((*_ctx), _enc__field_s_rep, _enc__trace_String_String);
}


_enc__class__Ped_util_XML_XML_node_t* _enc__constructor__Ped_util_XML_XML_node(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_XML_XML_node_t* _this = ((_enc__class__Ped_util_XML_XML_node_t*) encore_alloc((*_ctx), sizeof(_enc__class__Ped_util_XML_XML_node_t)));
  _this->_enc__self_type = (&(_enc__class__Ped_util_XML_XML_node_type));
  return _this;
}


_enc__class_String_String_t* _enc__method__Ped_util_XML_XML_node_attribute_value(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_node_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_name)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "attribute_value");
  /* var ret = "" */;
  /* ret = "" */;
  _enc__class_String_String_t* _new_0 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_1 = ({"";});
  pony_type_t* _tmp_2[] = {};
  _enc__type_init_String_String(_new_0);
  _enc__method_String_String_init(_ctx, _new_0, NULL, _embed_1);
  _enc__class_String_String_t* _ret_4 = _new_0;
  /* for a <- this.atribs do
  match a with
    case (key, value) when key.contains(name) =>
      ret = value
      ()
    end
    case _ =>
      ()
    end
  
  end
end */;
  void* _for_5;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "atribs");
  array_t* _fieldacc_12 = (*_this)._enc__field_atribs;
  int64_t _start_8 = 0;
  int64_t _stop_9 = (array_size(_fieldacc_12) - 1);
  int64_t _src_step_11 = 1;
  int64_t _literal_13 = 1;
  int64_t _step_10 = (_literal_13 * _src_step_11);
  range_assert_step(_step_10);
  int64_t _index_6;
  if ((_step_10 > 0))
  {
    _index_6 = _start_8;
  }
  else
  {
    _index_6 = _stop_9;
  };
  while (((_index_6 >= _start_8) && (_index_6 <= _stop_9)))
  {
    tuple_t* _a_7 = array_get(_fieldacc_12, _index_6).p;
    void* _match_14;
    _enc__class_String_String_t* _key_15;
    _enc__class_String_String_t* _value_16;
    if ((({int64_t _tupleCheck_20;
           _tupleCheck_20 = 1;
           _enc__class_String_String_t* _tupleAccess_21 = tuple_get(_a_7, 0).p;
           int64_t _varBinding_22;
           _key_15 = _tupleAccess_21;
           _varBinding_22 = 1;
           _tupleCheck_20 = (_tupleCheck_20 && _varBinding_22);
           _enc__class_String_String_t* _tupleAccess_23 = tuple_get(_a_7, 1).p;
           int64_t _varBinding_24;
           _value_16 = _tupleAccess_23;
           _varBinding_24 = 1;
           _tupleCheck_20 = (_tupleCheck_20 && _varBinding_24); _tupleCheck_20;}) && ({check_receiver(_key_15, ".", "key", "contains", "\"./Ped_util/XML.enc\" (line 35, column 32)");
                                                                                       pony_type_t* _tmp_26[] = {};
                                                                                       int64_t _sync_method_call_25 = _enc__method_String_String_contains(_ctx, _key_15, NULL, _enc__arg_name); _sync_method_call_25;})))
    {
      _match_14 = ((void*) ({/* ret = value */;
                             _ret_4 = _value_16;
                             /* () */;
                             UNIT; UNIT;}));
    }
    else
    {
      tuple_t* ___17;
      if ((({int64_t _varBinding_18;
             ___17 = _a_7;
             _varBinding_18 = 1; _varBinding_18;}) && ({int64_t _literal_19 = 1/*True*/; _literal_19;})))
      {
        _match_14 = ((void*) ({UNIT; UNIT;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
    _for_5 = _match_14;
    _index_6 = (_index_6 + _step_10);
  };
  /* ret */;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "attribute_value");
  return ((_enc__class_String_String_t*) _ret_4);
}


array_t* _enc__method__Ped_util_XML_XML_node_children_named(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_node_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_name)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "children_named");
  /* var cn = new [XML_node](|this.children|) */;
  /* cn = new [XML_node](|this.children|) */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "children");
  array_t* _fieldacc_1 = (*_this)._enc__field_children;
  int64_t _size_2 = array_size(_fieldacc_1);
  array_t* _array_0 = array_mk(_ctx, _size_2, (&(_enc__class__Ped_util_XML_XML_node_type)));
  array_t* _cn_4 = _array_0;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_5 = 0;
  int64_t _i_7 = _literal_5;
  /* for a <- this.children do
  if a.name.contains(name) then
    cn(i) = a
    i = i + 1
  end
end */;
  void* _for_8;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "children");
  array_t* _fieldacc_15 = (*_this)._enc__field_children;
  int64_t _start_11 = 0;
  int64_t _stop_12 = (array_size(_fieldacc_15) - 1);
  int64_t _src_step_14 = 1;
  int64_t _literal_16 = 1;
  int64_t _step_13 = (_literal_16 * _src_step_14);
  range_assert_step(_step_13);
  int64_t _index_9;
  if ((_step_13 > 0))
  {
    _index_9 = _start_11;
  }
  else
  {
    _index_9 = _stop_12;
  };
  while (((_index_9 >= _start_11) && (_index_9 <= _stop_12)))
  {
    _enc__class__Ped_util_XML_XML_node_t* _a_10 = array_get(_fieldacc_15, _index_9).p;
    void* _ite_17;
    if (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_10, "name");
          _enc__class_String_String_t* _fieldacc_19 = (*_a_10)._enc__field_name;
          check_receiver(_fieldacc_19, ".", "a.name", "contains", "\"./Ped_util/XML.enc\" (line 18, column 10)");
          pony_type_t* _tmp_20[] = {};
          int64_t _sync_method_call_18 = _enc__method_String_String_contains(_ctx, _fieldacc_19, NULL, _enc__arg_name); _sync_method_call_18;}))
    {
      /* cn(i) = a */;
      array_set(_cn_4, _i_7, ((encore_arg_t) {.p = _a_10}));
      /* i = i + 1 */;
      int64_t _binop_22 = (({ _i_7;}) + ({int64_t _literal_21 = 1; _literal_21;}));
      _i_7 = _binop_22;
      _ite_17 = ((void*) UNIT);
    }
    else
    {
      UNIT;
      _ite_17 = ((void*) UNIT);
    };
    _for_8 = _ite_17;
    _index_9 = (_index_9 + _step_13);
  };
  /* var cn2 = new [XML_node](i) */;
  /* cn2 = new [XML_node](i) */;
  array_t* _array_23 = array_mk(_ctx, _i_7, (&(_enc__class__Ped_util_XML_XML_node_type)));
  array_t* _cn2_25 = _array_23;
  /* i = 0 */;
  int64_t _literal_26 = 0;
  _i_7 = _literal_26;
  /* for a <- cn2 do
  cn2(i) = cn(i)
  i = i + 1
end */;
  void* _for_27;
  int64_t _start_30 = 0;
  int64_t _stop_31 = (array_size(_cn2_25) - 1);
  int64_t _src_step_33 = 1;
  int64_t _literal_34 = 1;
  int64_t _step_32 = (_literal_34 * _src_step_33);
  range_assert_step(_step_32);
  int64_t _index_28;
  if ((_step_32 > 0))
  {
    _index_28 = _start_30;
  }
  else
  {
    _index_28 = _stop_31;
  };
  while (((_index_28 >= _start_30) && (_index_28 <= _stop_31)))
  {
    _enc__class__Ped_util_XML_XML_node_t* _a_29 = array_get(_cn2_25, _index_28).p;
    /* cn2(i) = cn(i) */;
    _enc__class__Ped_util_XML_XML_node_t* _access_35 = array_get(_cn_4, _i_7).p;
    array_set(_cn2_25, _i_7, ((encore_arg_t) {.p = _access_35}));
    /* i = i + 1 */;
    int64_t _binop_37 = (({ _i_7;}) + ({int64_t _literal_36 = 1; _literal_36;}));
    _i_7 = _binop_37;
    _for_27 = UNIT;
    _index_28 = (_index_28 + _step_32);
  };
  /* cn2 */;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "children_named");
  return ((array_t*) _cn2_25);
}


void* _enc__method__Ped_util_XML_XML_node_init(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_node_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  UNIT;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class__Ped_util_XML_XML_node_type = {.id=_ENC__ID__Ped_util_XML_XML_node, .size=sizeof(_enc__class__Ped_util_XML_XML_node_t), .trace=_enc__trace__Ped_util_XML_XML_node, .vtable=trait_method_selector};
