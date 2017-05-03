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


void _enc__type_init__Ped_util_XML_XML_lib(_enc__class__Ped_util_XML_XML_lib_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_XML_XML_lib(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_XML_XML_lib_t* _this = p;
}


_enc__class__Ped_util_XML_XML_lib_t* _enc__constructor__Ped_util_XML_XML_lib(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_XML_XML_lib_t* _this = ((_enc__class__Ped_util_XML_XML_lib_t*) encore_alloc((*_ctx), sizeof(_enc__class__Ped_util_XML_XML_lib_t)));
  _this->_enc__self_type = (&(_enc__class__Ped_util_XML_XML_lib_type));
  return _this;
}


option_t* _enc__method__Ped_util_XML_XML_lib_file_to_xml(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_fname)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "file_to_xml");
  /* var f = new File("", "") */;
  /* f = new File("", "") */;
  _enc__class__Ped_util_IO_File_t* _new_0 = _enc__constructor__Ped_util_IO_File(_ctx, NULL);
  _enc__class_String_String_t* _new_1 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_2 = ({"";});
  pony_type_t* _tmp_3[] = {};
  _enc__type_init_String_String(_new_1);
  _enc__method_String_String_init(_ctx, _new_1, NULL, _embed_2);
  _enc__class_String_String_t* _new_4 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_5 = ({"";});
  pony_type_t* _tmp_6[] = {};
  _enc__type_init_String_String(_new_4);
  _enc__method_String_String_init(_ctx, _new_4, NULL, _embed_5);
  pony_type_t* _tmp_7[] = {};
  _enc__type_init__Ped_util_IO_File(_new_0);
  _enc__method__Ped_util_IO_File_init(_ctx, _new_0, NULL, _new_1, _new_4);
  _enc__class__Ped_util_IO_File_t* _f_9 = _new_0;
  /* var s = "" */;
  /* s = "" */;
  _enc__class_String_String_t* _new_10 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_11 = ({"";});
  pony_type_t* _tmp_12[] = {};
  _enc__type_init_String_String(_new_10);
  _enc__method_String_String_init(_ctx, _new_10, NULL, _embed_11);
  _enc__class_String_String_t* _s_14 = _new_10;
  /* f.open(fname, "r") */;
  check_receiver(_f_9, ".", "f", "open", "\"./Ped_util/XML.enc\" (line 224, column 5)");
  _enc__class_String_String_t* _new_16 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_17 = ({"r";});
  pony_type_t* _tmp_18[] = {};
  _enc__type_init_String_String(_new_16);
  _enc__method_String_String_init(_ctx, _new_16, NULL, _embed_17);
  pony_type_t* _tmp_19[] = {};
  void* _sync_method_call_15 = _enc__method__Ped_util_IO_File_open(_ctx, _f_9, NULL, _enc__arg_fname, _new_16);
  /* while f.eof() != true do
  s = s.concatenate(f.readline())
end */;
  void* _while_28;
  while (({int64_t _binop_23 = (({check_receiver(_f_9, ".", "f", "eof", "\"./Ped_util/XML.enc\" (line 225, column 11)");
                                  pony_type_t* _tmp_21[] = {};
                                  int64_t _sync_method_call_20 = _enc__method__Ped_util_IO_File_eof(_ctx, _f_9, NULL); _sync_method_call_20;}) != ({int64_t _literal_22 = 1/*True*/; _literal_22;})); _binop_23;}))
  {
    check_receiver(_s_14, ".", "s", "concatenate", "\"./Ped_util/XML.enc\" (line 226, column 11)");
    check_receiver(_f_9, ".", "f", "readline", "\"./Ped_util/XML.enc\" (line 226, column 25)");
    pony_type_t* _tmp_26[] = {};
    _enc__class_String_String_t* _sync_method_call_25 = _enc__method__Ped_util_IO_File_readline(_ctx, _f_9, NULL);
    pony_type_t* _tmp_27[] = {};
    _enc__class_String_String_t* _sync_method_call_24 = _enc__method_String_String_concatenate(_ctx, _s_14, NULL, _sync_method_call_25);
    _s_14 = _sync_method_call_24;
    _while_28 = UNIT;
  };
  /* this.new_XML_node(s) */;
  check_receiver(_this, ".", "this", "new_XML_node", "\"./Ped_util/XML.enc\" (line 228, column 5)");
  pony_type_t* _tmp_30[] = {};
  option_t* _sync_method_call_29 = _enc__method__Ped_util_XML_XML_lib_new_XML_node(_ctx, _this, NULL, _s_14);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "file_to_xml");
  return ((option_t*) _sync_method_call_29);
}


int64_t _enc__method__Ped_util_XML_XML_lib_pair(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_head, _enc__class_String_String_t* _enc__arg_tail)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "pair");
  int64_t _literal_0 = 1/*True*/;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "pair");
  return ((int64_t) _literal_0);
}


int64_t _enc__method__Ped_util_XML_XML_lib_or_find_from(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_a, _enc__class_String_String_t* _enc__arg_b, int64_t _enc__arg_i, _enc__class_String_String_t* _enc__arg_s)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "or_find_from");
  /* var w = s.find_from(a, i) */;
  /* w = s.find_from(a, i) */;
  check_receiver(_enc__arg_s, ".", "s", "find_from", "\"./Ped_util/XML.enc\" (line 210, column 13)");
  pony_type_t* _tmp_1[] = {};
  int64_t _sync_method_call_0 = _enc__method_String_String_find_from(_ctx, _enc__arg_s, NULL, _enc__arg_a, _enc__arg_i);
  int64_t _w_3 = _sync_method_call_0;
  /* var q = s.find_from(b, i) */;
  /* q = s.find_from(b, i) */;
  check_receiver(_enc__arg_s, ".", "s", "find_from", "\"./Ped_util/XML.enc\" (line 211, column 13)");
  pony_type_t* _tmp_5[] = {};
  int64_t _sync_method_call_4 = _enc__method_String_String_find_from(_ctx, _enc__arg_s, NULL, _enc__arg_b, _enc__arg_i);
  int64_t _q_7 = _sync_method_call_4;
  /* if w < q && w > -(1) then
  w
else
  q
end */;
  int64_t _ite_8;
  if (({int64_t _binop_13 = (({int64_t _binop_9 = (({ _w_3;}) < ({ _q_7;})); _binop_9;}) && ({int64_t _binop_12 = (({ _w_3;}) > ({int64_t _literal_10 = 1;
                                                                                                                                  int64_t _unary_11 = (- _literal_10); _unary_11;})); _binop_12;})); _binop_13;}))
  {
    _ite_8 = ((int64_t) _w_3);
  }
  else
  {
    _ite_8 = ((int64_t) _q_7);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "or_find_from");
  return ((int64_t) _ite_8);
}


void* _enc__method__Ped_util_XML_XML_lib_ext_atribs(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_XML_XML_node_t* _enc__arg_node, _enc__class_String_String_t* _enc__arg_s)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "ext_atribs");
  /* node.atribs = new [(String, String)](s.occurrences("=")) */;
  check_receiver(_enc__arg_s, ".", "s", "occurrences", "\"./Ped_util/XML.enc\" (line 180, column 42)");
  _enc__class_String_String_t* _new_2 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_3 = ({"=";});
  pony_type_t* _tmp_4[] = {};
  _enc__type_init_String_String(_new_2);
  _enc__method_String_String_init(_ctx, _new_2, NULL, _embed_3);
  pony_type_t* _tmp_5[] = {};
  int64_t _sync_method_call_1 = _enc__method_String_String_occurrences(_ctx, _enc__arg_s, NULL, _new_2);
  array_t* _array_0 = array_mk(_ctx, _sync_method_call_1, (&(tuple_type)));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "atribs"); _enc__arg_node;}))._enc__field_atribs = _array_0;
  /* var eqsign = 0 */;
  /* eqsign = 0 */;
  int64_t _literal_6 = 0;
  int64_t _eqsign_8 = _literal_6;
  /* var space = 0 */;
  /* space = 0 */;
  int64_t _literal_9 = 0;
  int64_t _space_11 = _literal_9;
  /* var value = "" */;
  /* value = "" */;
  _enc__class_String_String_t* _new_12 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_13 = ({"";});
  pony_type_t* _tmp_14[] = {};
  _enc__type_init_String_String(_new_12);
  _enc__method_String_String_init(_ctx, _new_12, NULL, _embed_13);
  _enc__class_String_String_t* _value_16 = _new_12;
  /* var key = "" */;
  /* key = "" */;
  _enc__class_String_String_t* _new_17 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_18 = ({"";});
  pony_type_t* _tmp_19[] = {};
  _enc__type_init_String_String(_new_17);
  _enc__method_String_String_init(_ctx, _new_17, NULL, _embed_18);
  _enc__class_String_String_t* _key_21 = _new_17;
  /* for i <- [0..|node.atribs| - 1] do
  space = s.find_from(" ", eqsign)
  eqsign = s.find_from("=", eqsign + 1)
  value = match s.substring(eqsign + 2, this.or_find_from(" ", ">", eqsign, s) - 1) with
            case Just(derp) =>
              derp
            end
            case Nothing =>
              "MISSFORMEDVALUE"
            end
          
          end
  key = match s.substring(space, eqsign) with
          case Just(derp) =>
            derp
          end
          case Nothing =>
            "MISSFORMEDKEY"
          end
        
        end
  (node.atribs)(i) = (key, value)
end */;
  void* _for_22;
  /* Range not generated */;
  int64_t _literal_29 = 0;
  int64_t _binop_33 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "atribs");
                         array_t* _fieldacc_30 = (*_enc__arg_node)._enc__field_atribs;
                         int64_t _size_31 = array_size(_fieldacc_30); _size_31;}) - ({int64_t _literal_32 = 1; _literal_32;}));
  int64_t _literal_34 = 1;
  int64_t _literal_35 = 1;
  int64_t _step_27 = (_literal_35 * _literal_34);
  range_assert_step(_step_27);
  int64_t _index_23;
  if ((_step_27 > 0))
  {
    _index_23 = _literal_29;
  }
  else
  {
    _index_23 = _binop_33;
  };
  while (((_index_23 >= _literal_29) && (_index_23 <= _binop_33)))
  {
    int64_t _i_24 = _index_23;
    /* space = s.find_from(" ", eqsign) */;
    check_receiver(_enc__arg_s, ".", "s", "find_from", "\"./Ped_util/XML.enc\" (line 186, column 15)");
    _enc__class_String_String_t* _new_37 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_38 = ({" ";});
    pony_type_t* _tmp_39[] = {};
    _enc__type_init_String_String(_new_37);
    _enc__method_String_String_init(_ctx, _new_37, NULL, _embed_38);
    pony_type_t* _tmp_40[] = {};
    int64_t _sync_method_call_36 = _enc__method_String_String_find_from(_ctx, _enc__arg_s, NULL, _new_37, _eqsign_8);
    _space_11 = _sync_method_call_36;
    /* eqsign = s.find_from("=", eqsign + 1) */;
    check_receiver(_enc__arg_s, ".", "s", "find_from", "\"./Ped_util/XML.enc\" (line 187, column 16)");
    _enc__class_String_String_t* _new_42 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_43 = ({"=";});
    pony_type_t* _tmp_44[] = {};
    _enc__type_init_String_String(_new_42);
    _enc__method_String_String_init(_ctx, _new_42, NULL, _embed_43);
    int64_t _binop_46 = (({ _eqsign_8;}) + ({int64_t _literal_45 = 1; _literal_45;}));
    pony_type_t* _tmp_47[] = {};
    int64_t _sync_method_call_41 = _enc__method_String_String_find_from(_ctx, _enc__arg_s, NULL, _new_42, _binop_46);
    _eqsign_8 = _sync_method_call_41;
    /* value = match s.substring(eqsign + 2, this.or_find_from(" ", ">", eqsign, s) - 1) with
          case Just(derp) =>
            derp
          end
          case Nothing =>
            "MISSFORMEDVALUE"
          end
        
        end */;
    check_receiver(_enc__arg_s, ".", "s", "substring", "\"./Ped_util/XML.enc\" (line 188, column 21)");
    int64_t _binop_51 = (({ _eqsign_8;}) + ({int64_t _literal_50 = 2; _literal_50;}));
    int64_t _binop_61 = (({check_receiver(_this, ".", "this", "or_find_from", "\"./Ped_util/XML.enc\" (line 188, column 45)");
                           _enc__class_String_String_t* _new_53 = _enc__constructor_String_String(_ctx, NULL);
                           char* _embed_54 = ({" ";});
                           pony_type_t* _tmp_55[] = {};
                           _enc__type_init_String_String(_new_53);
                           _enc__method_String_String_init(_ctx, _new_53, NULL, _embed_54);
                           _enc__class_String_String_t* _new_56 = _enc__constructor_String_String(_ctx, NULL);
                           char* _embed_57 = ({">";});
                           pony_type_t* _tmp_58[] = {};
                           _enc__type_init_String_String(_new_56);
                           _enc__method_String_String_init(_ctx, _new_56, NULL, _embed_57);
                           pony_type_t* _tmp_59[] = {};
                           int64_t _sync_method_call_52 = _enc__method__Ped_util_XML_XML_lib_or_find_from(_ctx, _this, NULL, _new_53, _new_56, _eqsign_8, _enc__arg_s); _sync_method_call_52;}) - ({int64_t _literal_60 = 1; _literal_60;}));
    pony_type_t* _tmp_62[] = {};
    option_t* _sync_method_call_49 = _enc__method_String_String_substring(_ctx, _enc__arg_s, NULL, _binop_51, _binop_61);
    _enc__class_String_String_t* _match_48;
    _enc__class_String_String_t* _derp_63;
    if ((({int64_t _optionCheck_71;
           _optionCheck_71 = ((JUST == (*_sync_method_call_49).tag) && ({int64_t _varBinding_72;
                                                                         _enc__class_String_String_t* _optionVal_70 = (*_sync_method_call_49).val.p;
                                                                         _derp_63 = _optionVal_70;
                                                                         _varBinding_72 = 1; _varBinding_72;})); _optionCheck_71;}) && ({int64_t _literal_73 = 1/*True*/; _literal_73;})))
    {
      _match_48 = ((_enc__class_String_String_t*) ({ _derp_63;}));
    }
    else
    {
      if ((({int64_t _valueCheck_67;
             _valueCheck_67 = (({option_t* _option_68 = (&(DEFAULT_NOTHING)); _option_68;}) == _sync_method_call_49); _valueCheck_67;}) && ({int64_t _literal_69 = 1/*True*/; _literal_69;})))
      {
        _match_48 = ((_enc__class_String_String_t*) ({_enc__class_String_String_t* _new_64 = _enc__constructor_String_String(_ctx, NULL);
                                                      char* _embed_65 = ({"MISSFORMEDVALUE";});
                                                      pony_type_t* _tmp_66[] = {};
                                                      _enc__type_init_String_String(_new_64);
                                                      _enc__method_String_String_init(_ctx, _new_64, NULL, _embed_65); _new_64;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
    _value_16 = _match_48;
    /* key = match s.substring(space, eqsign) with
        case Just(derp) =>
          derp
        end
        case Nothing =>
          "MISSFORMEDKEY"
        end
      
      end */;
    check_receiver(_enc__arg_s, ".", "s", "substring", "\"./Ped_util/XML.enc\" (line 197, column 19)");
    pony_type_t* _tmp_76[] = {};
    option_t* _sync_method_call_75 = _enc__method_String_String_substring(_ctx, _enc__arg_s, NULL, _space_11, _eqsign_8);
    _enc__class_String_String_t* _match_74;
    _enc__class_String_String_t* _derp_77;
    if ((({int64_t _optionCheck_85;
           _optionCheck_85 = ((JUST == (*_sync_method_call_75).tag) && ({int64_t _varBinding_86;
                                                                         _enc__class_String_String_t* _optionVal_84 = (*_sync_method_call_75).val.p;
                                                                         _derp_77 = _optionVal_84;
                                                                         _varBinding_86 = 1; _varBinding_86;})); _optionCheck_85;}) && ({int64_t _literal_87 = 1/*True*/; _literal_87;})))
    {
      _match_74 = ((_enc__class_String_String_t*) ({ _derp_77;}));
    }
    else
    {
      if ((({int64_t _valueCheck_81;
             _valueCheck_81 = (({option_t* _option_82 = (&(DEFAULT_NOTHING)); _option_82;}) == _sync_method_call_75); _valueCheck_81;}) && ({int64_t _literal_83 = 1/*True*/; _literal_83;})))
      {
        _match_74 = ((_enc__class_String_String_t*) ({_enc__class_String_String_t* _new_78 = _enc__constructor_String_String(_ctx, NULL);
                                                      char* _embed_79 = ({"MISSFORMEDKEY";});
                                                      pony_type_t* _tmp_80[] = {};
                                                      _enc__type_init_String_String(_new_78);
                                                      _enc__method_String_String_init(_ctx, _new_78, NULL, _embed_79); _new_78;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
    _key_21 = _match_74;
    /* (node.atribs)(i) = (key, value) */;
    tuple_t* _tuple_88 = tuple_mk(_ctx, 2);
    tuple_set_type(_tuple_88, 0, (&(_enc__class_String_String_type)));
    tuple_set_type(_tuple_88, 1, (&(_enc__class_String_String_type)));
    tuple_set(_tuple_88, 0, ((encore_arg_t) {.p = _key_21}));
    tuple_set(_tuple_88, 1, ((encore_arg_t) {.p = _value_16}));
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_enc__arg_node, "atribs");
    array_t* _fieldacc_89 = (*_enc__arg_node)._enc__field_atribs;
    array_set(_fieldacc_89, _i_24, ((encore_arg_t) {.p = _tuple_88}));
    _for_22 = UNIT;
    _index_23 = (_index_23 + _step_27);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "ext_atribs");
  return UNIT;
}


char _enc__method__Ped_util_XML_XML_lib_tag_type(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_s)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "tag_type");
  check_receiver(_enc__arg_s, ".", "s", "char_at", "\"./Ped_util/XML.enc\" (line 158, column 11)");
  int64_t _literal_2 = 1;
  pony_type_t* _tmp_3[] = {};
  option_t* _sync_method_call_1 = _enc__method_String_String_char_at(_ctx, _enc__arg_s, NULL, _literal_2);
  char _match_0;
  if ((({int64_t _optionCheck_32;
         _optionCheck_32 = ((JUST == (*_sync_method_call_1).tag) && ({int64_t _valueCheck_33;
                                                                      char _optionVal_31 = (*_sync_method_call_1).val.i;
                                                                      _valueCheck_33 = (({char _literal_34 = '!'; _literal_34;}) == _optionVal_31); _valueCheck_33;})); _optionCheck_32;}) && ({int64_t _literal_35 = 1/*True*/; _literal_35;})))
  {
    _match_0 = ((char) ({char _literal_4 = 'c'; _literal_4;}));
  }
  else
  {
    if ((({int64_t _optionCheck_27;
           _optionCheck_27 = ((JUST == (*_sync_method_call_1).tag) && ({int64_t _valueCheck_28;
                                                                        char _optionVal_26 = (*_sync_method_call_1).val.i;
                                                                        _valueCheck_28 = (({char _literal_29 = '/'; _literal_29;}) == _optionVal_26); _valueCheck_28;})); _optionCheck_27;}) && ({int64_t _literal_30 = 1/*True*/; _literal_30;})))
    {
      _match_0 = ((char) ({char _literal_5 = 'e'; _literal_5;}));
    }
    else
    {
      option_t* ___6;
      if ((({int64_t _varBinding_24;
             ___6 = _sync_method_call_1;
             _varBinding_24 = 1; _varBinding_24;}) && ({int64_t _literal_25 = 1/*True*/; _literal_25;})))
      {
        _match_0 = ((char) ({check_receiver(_enc__arg_s, ".", "s", "char_at", "\"./Ped_util/XML.enc\" (line 166, column 15)");
                             int64_t _binop_12 = (({check_receiver(_enc__arg_s, ".", "s", "size", "\"./Ped_util/XML.enc\" (line 166, column 25)");
                                                    pony_type_t* _tmp_10[] = {};
                                                    int64_t _sync_method_call_9 = _enc__method_String_String_size(_ctx, _enc__arg_s, NULL); _sync_method_call_9;}) - ({int64_t _literal_11 = 2; _literal_11;}));
                             pony_type_t* _tmp_13[] = {};
                             option_t* _sync_method_call_8 = _enc__method_String_String_char_at(_ctx, _enc__arg_s, NULL, _binop_12);
                             char _match_7;
                             if ((({int64_t _optionCheck_20;
                                    _optionCheck_20 = ((JUST == (*_sync_method_call_8).tag) && ({int64_t _valueCheck_21;
                                                                                                 char _optionVal_19 = (*_sync_method_call_8).val.i;
                                                                                                 _valueCheck_21 = (({char _literal_22 = '/'; _literal_22;}) == _optionVal_19); _valueCheck_21;})); _optionCheck_20;}) && ({int64_t _literal_23 = 1/*True*/; _literal_23;})))
                             {
                               _match_7 = ((char) ({char _literal_14 = 't'; _literal_14;}));
                             }
                             else
                             {
                               option_t* ___15;
                               if ((({int64_t _varBinding_17;
                                      ___15 = _sync_method_call_8;
                                      _varBinding_17 = 1; _varBinding_17;}) && ({int64_t _literal_18 = 1/*True*/; _literal_18;})))
                               {
                                 _match_7 = ((char) ({char _literal_16 = 's'; _literal_16;}));
                               }
                               else
                               {
                                 fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                 exit(1);
                               };
                             }; _match_7;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "tag_type");
  return ((char) _match_0);
}


option_t* _enc__method__Ped_util_XML_XML_lib_new_XML_node(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_s)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "new_XML_node");
  /* var node = new XML_node() */;
  /* node = new XML_node() */;
  _enc__class__Ped_util_XML_XML_node_t* _new_0 = _enc__constructor__Ped_util_XML_XML_node(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init__Ped_util_XML_XML_node(_new_0);
  _enc__method__Ped_util_XML_XML_node_init(_ctx, _new_0, NULL);
  _enc__class__Ped_util_XML_XML_node_t* _node_3 = _new_0;
  /* var bad = false */;
  /* bad = false */;
  int64_t _literal_4 = 0/*False*/;
  int64_t _bad_6 = _literal_4;
  /* s.trim() */;
  check_receiver(_enc__arg_s, ".", "s", "trim", "\"./Ped_util/XML.enc\" (line 55, column 5)");
  pony_type_t* _tmp_8[] = {};
  _enc__class_String_String_t* _sync_method_call_7 = _enc__method_String_String_trim(_ctx, _enc__arg_s, NULL);
  /* node.s_rep = s */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_node_3, "s_rep"); _node_3;}))._enc__field_s_rep = _enc__arg_s;
  /* var cont = new [String](s.occurrences("<")) */;
  /* cont = new [String](s.occurrences("<")) */;
  check_receiver(_enc__arg_s, ".", "s", "occurrences", "\"./Ped_util/XML.enc\" (line 57, column 29)");
  _enc__class_String_String_t* _new_11 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_12 = ({"<";});
  pony_type_t* _tmp_13[] = {};
  _enc__type_init_String_String(_new_11);
  _enc__method_String_String_init(_ctx, _new_11, NULL, _embed_12);
  pony_type_t* _tmp_14[] = {};
  int64_t _sync_method_call_10 = _enc__method_String_String_occurrences(_ctx, _enc__arg_s, NULL, _new_11);
  array_t* _array_9 = array_mk(_ctx, _sync_method_call_10, (&(_enc__class_String_String_type)));
  array_t* _cont_16 = _array_9;
  /* if |cont| == 1 then
  var r = match (s.char_at(0), s.char_at(s.size() - 2), s.char_at(s.size() - 1)) with
            case (Just('<'), Just('/'), Just('>')) =>
              node.name = s
              this.ext_atribs(node, node.name)
              Just(node)
            end
            case _ =>
              Nothing
            end
          
          end
  EMBED (unit)
    return #{r};
  END
end */;
  void* _ite_17;
  if (({int64_t _binop_20 = (({int64_t _size_18 = array_size(_cont_16); _size_18;}) == ({int64_t _literal_19 = 1; _literal_19;})); _binop_20;}))
  {
    /* var r = match (s.char_at(0), s.char_at(s.size() - 2), s.char_at(s.size() - 1)) with
          case (Just('<'), Just('/'), Just('>')) =>
            node.name = s
            this.ext_atribs(node, node.name)
            Just(node)
          end
          case _ =>
            Nothing
          end
        
        end */;
    /* r = match (s.char_at(0), s.char_at(s.size() - 2), s.char_at(s.size() - 1)) with
  case (Just('<'), Just('/'), Just('>')) =>
    node.name = s
    this.ext_atribs(node, node.name)
    Just(node)
  end
  case _ =>
    Nothing
  end

end */;
    tuple_t* _tuple_22 = tuple_mk(_ctx, 3);
    tuple_set_type(_tuple_22, 0, (&(option_type)));
    tuple_set_type(_tuple_22, 1, (&(option_type)));
    tuple_set_type(_tuple_22, 2, (&(option_type)));
    check_receiver(_enc__arg_s, ".", "s", "char_at", "\"./Ped_util/XML.enc\" (line 59, column 22)");
    int64_t _literal_24 = 0;
    pony_type_t* _tmp_25[] = {};
    option_t* _sync_method_call_23 = _enc__method_String_String_char_at(_ctx, _enc__arg_s, NULL, _literal_24);
    check_receiver(_enc__arg_s, ".", "s", "char_at", "\"./Ped_util/XML.enc\" (line 59, column 36)");
    int64_t _binop_30 = (({check_receiver(_enc__arg_s, ".", "s", "size", "\"./Ped_util/XML.enc\" (line 59, column 46)");
                           pony_type_t* _tmp_28[] = {};
                           int64_t _sync_method_call_27 = _enc__method_String_String_size(_ctx, _enc__arg_s, NULL); _sync_method_call_27;}) - ({int64_t _literal_29 = 2; _literal_29;}));
    pony_type_t* _tmp_31[] = {};
    option_t* _sync_method_call_26 = _enc__method_String_String_char_at(_ctx, _enc__arg_s, NULL, _binop_30);
    check_receiver(_enc__arg_s, ".", "s", "char_at", "\"./Ped_util/XML.enc\" (line 59, column 61)");
    int64_t _binop_36 = (({check_receiver(_enc__arg_s, ".", "s", "size", "\"./Ped_util/XML.enc\" (line 59, column 71)");
                           pony_type_t* _tmp_34[] = {};
                           int64_t _sync_method_call_33 = _enc__method_String_String_size(_ctx, _enc__arg_s, NULL); _sync_method_call_33;}) - ({int64_t _literal_35 = 1; _literal_35;}));
    pony_type_t* _tmp_37[] = {};
    option_t* _sync_method_call_32 = _enc__method_String_String_char_at(_ctx, _enc__arg_s, NULL, _binop_36);
    tuple_set(_tuple_22, 0, ((encore_arg_t) {.p = _sync_method_call_23}));
    tuple_set(_tuple_22, 1, ((encore_arg_t) {.p = _sync_method_call_26}));
    tuple_set(_tuple_22, 2, ((encore_arg_t) {.p = _sync_method_call_32}));
    option_t* _match_21;
    if ((({int64_t _tupleCheck_46;
           _tupleCheck_46 = 1;
           option_t* _tupleAccess_47 = tuple_get(_tuple_22, 0).p;
           int64_t _optionCheck_49;
           _optionCheck_49 = ((JUST == (*_tupleAccess_47).tag) && ({int64_t _valueCheck_50;
                                                                    char _optionVal_48 = (*_tupleAccess_47).val.i;
                                                                    _valueCheck_50 = (({char _literal_51 = '<'; _literal_51;}) == _optionVal_48); _valueCheck_50;}));
           _tupleCheck_46 = (_tupleCheck_46 && _optionCheck_49);
           option_t* _tupleAccess_52 = tuple_get(_tuple_22, 1).p;
           int64_t _optionCheck_54;
           _optionCheck_54 = ((JUST == (*_tupleAccess_52).tag) && ({int64_t _valueCheck_55;
                                                                    char _optionVal_53 = (*_tupleAccess_52).val.i;
                                                                    _valueCheck_55 = (({char _literal_56 = '/'; _literal_56;}) == _optionVal_53); _valueCheck_55;}));
           _tupleCheck_46 = (_tupleCheck_46 && _optionCheck_54);
           option_t* _tupleAccess_57 = tuple_get(_tuple_22, 2).p;
           int64_t _optionCheck_59;
           _optionCheck_59 = ((JUST == (*_tupleAccess_57).tag) && ({int64_t _valueCheck_60;
                                                                    char _optionVal_58 = (*_tupleAccess_57).val.i;
                                                                    _valueCheck_60 = (({char _literal_61 = '>'; _literal_61;}) == _optionVal_58); _valueCheck_60;}));
           _tupleCheck_46 = (_tupleCheck_46 && _optionCheck_59); _tupleCheck_46;}) && ({int64_t _literal_62 = 1/*True*/; _literal_62;})))
    {
      _match_21 = ((option_t*) ({/* node.name = s */;
                                 (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_node_3, "name"); _node_3;}))._enc__field_name = _enc__arg_s;
                                 /* this.ext_atribs(node, node.name) */;
                                 check_receiver(_this, ".", "this", "ext_atribs", "\"./Ped_util/XML.enc\" (line 62, column 19)");
                                 ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_node_3, "name");
                                 _enc__class_String_String_t* _fieldacc_39 = (*_node_3)._enc__field_name;
                                 pony_type_t* _tmp_40[] = {};
                                 void* _sync_method_call_38 = _enc__method__Ped_util_XML_XML_lib_ext_atribs(_ctx, _this, NULL, _node_3, _fieldacc_39);
                                 /* Just(node) */;
                                 option_t* _option_41 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _node_3}), (&(_enc__class__Ped_util_XML_XML_node_type))); _option_41;}));
    }
    else
    {
      tuple_t* ___42;
      if ((({int64_t _varBinding_44;
             ___42 = _tuple_22;
             _varBinding_44 = 1; _varBinding_44;}) && ({int64_t _literal_45 = 1/*True*/; _literal_45;})))
      {
        _match_21 = ((option_t*) ({option_t* _option_43 = (&(DEFAULT_NOTHING)); _option_43;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
    option_t* _r_64 = _match_21;
    /* EMBED (unit)
  return #{r};
END */;
    ({return _r_64;});
    _ite_17 = ((void*) UNIT);
  }
  else
  {
    UNIT;
    _ite_17 = ((void*) UNIT);
  };
  /* var start = s.find("<") */;
  /* start = s.find("<") */;
  check_receiver(_enc__arg_s, ".", "s", "find", "\"./Ped_util/XML.enc\" (line 74, column 17)");
  _enc__class_String_String_t* _new_66 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_67 = ({"<";});
  pony_type_t* _tmp_68[] = {};
  _enc__type_init_String_String(_new_66);
  _enc__method_String_String_init(_ctx, _new_66, NULL, _embed_67);
  pony_type_t* _tmp_69[] = {};
  int64_t _sync_method_call_65 = _enc__method_String_String_find(_ctx, _enc__arg_s, NULL, _new_66);
  int64_t _start_71 = _sync_method_call_65;
  /* var finnish = 0 */;
  /* finnish = 0 */;
  int64_t _literal_72 = 0;
  int64_t _finnish_74 = _literal_72;
  /* for i <- [0..|cont| - 1] do
  finnish = s.find_from(">", start)
  cont(i) = match s.substring(start, finnish + 1) with
              case Just(sub) =>
                sub
              end
              case Nothing =>
                ""
              end
            
            end
  start = s.find_from("<", finnish)
end */;
  void* _for_75;
  /* Range not generated */;
  int64_t _literal_82 = 0;
  int64_t _binop_85 = (({int64_t _size_83 = array_size(_cont_16); _size_83;}) - ({int64_t _literal_84 = 1; _literal_84;}));
  int64_t _literal_86 = 1;
  int64_t _literal_87 = 1;
  int64_t _step_80 = (_literal_87 * _literal_86);
  range_assert_step(_step_80);
  int64_t _index_76;
  if ((_step_80 > 0))
  {
    _index_76 = _literal_82;
  }
  else
  {
    _index_76 = _binop_85;
  };
  while (((_index_76 >= _literal_82) && (_index_76 <= _binop_85)))
  {
    int64_t _i_77 = _index_76;
    /* finnish = s.find_from(">", start) */;
    check_receiver(_enc__arg_s, ".", "s", "find_from", "\"./Ped_util/XML.enc\" (line 77, column 17)");
    _enc__class_String_String_t* _new_89 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_90 = ({">";});
    pony_type_t* _tmp_91[] = {};
    _enc__type_init_String_String(_new_89);
    _enc__method_String_String_init(_ctx, _new_89, NULL, _embed_90);
    pony_type_t* _tmp_92[] = {};
    int64_t _sync_method_call_88 = _enc__method_String_String_find_from(_ctx, _enc__arg_s, NULL, _new_89, _start_71);
    _finnish_74 = _sync_method_call_88;
    /* cont(i) = match s.substring(start, finnish + 1) with
            case Just(sub) =>
              sub
            end
            case Nothing =>
              ""
            end
          
          end */;
    check_receiver(_enc__arg_s, ".", "s", "substring", "\"./Ped_util/XML.enc\" (line 78, column 23)");
    int64_t _binop_96 = (({ _finnish_74;}) + ({int64_t _literal_95 = 1; _literal_95;}));
    pony_type_t* _tmp_97[] = {};
    option_t* _sync_method_call_94 = _enc__method_String_String_substring(_ctx, _enc__arg_s, NULL, _start_71, _binop_96);
    _enc__class_String_String_t* _match_93;
    _enc__class_String_String_t* _sub_98;
    if ((({int64_t _optionCheck_106;
           _optionCheck_106 = ((JUST == (*_sync_method_call_94).tag) && ({int64_t _varBinding_107;
                                                                          _enc__class_String_String_t* _optionVal_105 = (*_sync_method_call_94).val.p;
                                                                          _sub_98 = _optionVal_105;
                                                                          _varBinding_107 = 1; _varBinding_107;})); _optionCheck_106;}) && ({int64_t _literal_108 = 1/*True*/; _literal_108;})))
    {
      _match_93 = ((_enc__class_String_String_t*) ({ _sub_98;}));
    }
    else
    {
      if ((({int64_t _valueCheck_102;
             _valueCheck_102 = (({option_t* _option_103 = (&(DEFAULT_NOTHING)); _option_103;}) == _sync_method_call_94); _valueCheck_102;}) && ({int64_t _literal_104 = 1/*True*/; _literal_104;})))
      {
        _match_93 = ((_enc__class_String_String_t*) ({_enc__class_String_String_t* _new_99 = _enc__constructor_String_String(_ctx, NULL);
                                                      char* _embed_100 = ({"";});
                                                      pony_type_t* _tmp_101[] = {};
                                                      _enc__type_init_String_String(_new_99);
                                                      _enc__method_String_String_init(_ctx, _new_99, NULL, _embed_100); _new_99;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
    array_set(_cont_16, _i_77, ((encore_arg_t) {.p = _match_93}));
    /* start = s.find_from("<", finnish) */;
    check_receiver(_enc__arg_s, ".", "s", "find_from", "\"./Ped_util/XML.enc\" (line 87, column 15)");
    _enc__class_String_String_t* _new_110 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_111 = ({"<";});
    pony_type_t* _tmp_112[] = {};
    _enc__type_init_String_String(_new_110);
    _enc__method_String_String_init(_ctx, _new_110, NULL, _embed_111);
    pony_type_t* _tmp_113[] = {};
    int64_t _sync_method_call_109 = _enc__method_String_String_find_from(_ctx, _enc__arg_s, NULL, _new_110, _finnish_74);
    _start_71 = _sync_method_call_109;
    _for_75 = UNIT;
    _index_76 = (_index_76 + _step_80);
  };
  /* node.name = cont(0) */;
  int64_t _literal_114 = 0;
  _enc__class_String_String_t* _access_115 = array_get(_cont_16, _literal_114).p;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_node_3, "name"); _node_3;}))._enc__field_name = _access_115;
  /* this.ext_atribs(node, node.name) */;
  check_receiver(_this, ".", "this", "ext_atribs", "\"./Ped_util/XML.enc\" (line 90, column 5)");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_node_3, "name");
  _enc__class_String_String_t* _fieldacc_117 = (*_node_3)._enc__field_name;
  pony_type_t* _tmp_118[] = {};
  void* _sync_method_call_116 = _enc__method__Ped_util_XML_XML_lib_ext_atribs(_ctx, _this, NULL, _node_3, _fieldacc_117);
  /* if this.pair(node.name, cont(|cont| - 1)) then
  node.comments = new [String](|cont|)
  var ci = 0
  var babies = new [String](|cont|)
  var j = 0
  var temp = ""
  var level = 0
  for i <- [1..|cont| - 2] do
    match (level, this.tag_type(cont(i))) with
      case (0, 'c') =>
        (node.comments)(ci) = cont(i)
        ci = ci + 1
        ()
      end
      case (0, 't') =>
        babies(j) = cont(i)
        j = j + 1
        ()
      end
      case (1, 'e') =>
        babies(j) = temp.concatenate(cont(i))
        temp = ""
        j = j + 1
        level = 0
        ()
      end
      case (X, 'e') =>
        temp = temp.concatenate(cont(i))
        level = X - 1
        ()
      end
      case (X, 's') =>
        temp = temp.concatenate(cont(i))
        level = X + 1
        ()
      end
      case _ =>
        temp = temp.concatenate(cont(i))
        ()
      end
    
    end
  end
  node.children = new [XML_node](j)
  for i <- [0..j - 1] do
    match this.new_XML_node(babies(i)) with
      case Just(xml) =>
        (node.children)(i) = xml
        ()
      end
      case Nothing =>
        bad = true
        ()
      end
    
    end
  end
  if bad == false then
    Just(node)
  else
    Nothing
  end
else
  Nothing
end */;
  option_t* _ite_119;
  if (({check_receiver(_this, ".", "this", "pair", "\"./Ped_util/XML.enc\" (line 91, column 8)");
        ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_node_3, "name");
        _enc__class_String_String_t* _fieldacc_121 = (*_node_3)._enc__field_name;
        int64_t _binop_124 = (({int64_t _size_122 = array_size(_cont_16); _size_122;}) - ({int64_t _literal_123 = 1; _literal_123;}));
        _enc__class_String_String_t* _access_125 = array_get(_cont_16, _binop_124).p;
        pony_type_t* _tmp_126[] = {};
        int64_t _sync_method_call_120 = _enc__method__Ped_util_XML_XML_lib_pair(_ctx, _this, NULL, _fieldacc_121, _access_125); _sync_method_call_120;}))
  {
    /* node.comments = new [String](|cont|) */;
    int64_t _size_128 = array_size(_cont_16);
    array_t* _array_127 = array_mk(_ctx, _size_128, (&(_enc__class_String_String_type)));
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_node_3, "comments"); _node_3;}))._enc__field_comments = _array_127;
    /* var ci = 0 */;
    /* ci = 0 */;
    int64_t _literal_129 = 0;
    int64_t _ci_131 = _literal_129;
    /* var babies = new [String](|cont|) */;
    /* babies = new [String](|cont|) */;
    int64_t _size_133 = array_size(_cont_16);
    array_t* _array_132 = array_mk(_ctx, _size_133, (&(_enc__class_String_String_type)));
    array_t* _babies_135 = _array_132;
    /* var j = 0 */;
    /* j = 0 */;
    int64_t _literal_136 = 0;
    int64_t _j_138 = _literal_136;
    /* var temp = "" */;
    /* temp = "" */;
    _enc__class_String_String_t* _new_139 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_140 = ({"";});
    pony_type_t* _tmp_141[] = {};
    _enc__type_init_String_String(_new_139);
    _enc__method_String_String_init(_ctx, _new_139, NULL, _embed_140);
    _enc__class_String_String_t* _temp_143 = _new_139;
    /* var level = 0 */;
    /* level = 0 */;
    int64_t _literal_144 = 0;
    int64_t _level_146 = _literal_144;
    /* for i <- [1..|cont| - 2] do
  match (level, this.tag_type(cont(i))) with
    case (0, 'c') =>
      (node.comments)(ci) = cont(i)
      ci = ci + 1
      ()
    end
    case (0, 't') =>
      babies(j) = cont(i)
      j = j + 1
      ()
    end
    case (1, 'e') =>
      babies(j) = temp.concatenate(cont(i))
      temp = ""
      j = j + 1
      level = 0
      ()
    end
    case (X, 'e') =>
      temp = temp.concatenate(cont(i))
      level = X - 1
      ()
    end
    case (X, 's') =>
      temp = temp.concatenate(cont(i))
      level = X + 1
      ()
    end
    case _ =>
      temp = temp.concatenate(cont(i))
      ()
    end
  
  end
end */;
    void* _for_147;
    /* Range not generated */;
    int64_t _literal_154 = 1;
    int64_t _binop_157 = (({int64_t _size_155 = array_size(_cont_16); _size_155;}) - ({int64_t _literal_156 = 2; _literal_156;}));
    int64_t _literal_158 = 1;
    int64_t _literal_159 = 1;
    int64_t _step_152 = (_literal_159 * _literal_158);
    range_assert_step(_step_152);
    int64_t _index_148;
    if ((_step_152 > 0))
    {
      _index_148 = _literal_154;
    }
    else
    {
      _index_148 = _binop_157;
    };
    while (((_index_148 >= _literal_154) && (_index_148 <= _binop_157)))
    {
      int64_t _i_149 = _index_148;
      tuple_t* _tuple_161 = tuple_mk(_ctx, 2);
      tuple_set_type(_tuple_161, 0, ENCORE_PRIMITIVE);
      tuple_set_type(_tuple_161, 1, ENCORE_PRIMITIVE);
      check_receiver(_this, ".", "this", "tag_type", "\"./Ped_util/XML.enc\" (line 99, column 23)");
      _enc__class_String_String_t* _access_163 = array_get(_cont_16, _i_149).p;
      pony_type_t* _tmp_164[] = {};
      char _sync_method_call_162 = _enc__method__Ped_util_XML_XML_lib_tag_type(_ctx, _this, NULL, _access_163);
      tuple_set(_tuple_161, 0, ((encore_arg_t) {.i = _level_146}));
      tuple_set(_tuple_161, 1, ((encore_arg_t) {.i = _sync_method_call_162}));
      void* _match_160;
      if ((({int64_t _tupleCheck_229;
             _tupleCheck_229 = 1;
             int64_t _tupleAccess_230 = tuple_get(_tuple_161, 0).i;
             int64_t _valueCheck_231;
             _valueCheck_231 = (({int64_t _literal_232 = 0; _literal_232;}) == _tupleAccess_230);
             _tupleCheck_229 = (_tupleCheck_229 && _valueCheck_231);
             char _tupleAccess_233 = tuple_get(_tuple_161, 1).i;
             int64_t _valueCheck_234;
             _valueCheck_234 = (({char _literal_235 = 'c'; _literal_235;}) == _tupleAccess_233);
             _tupleCheck_229 = (_tupleCheck_229 && _valueCheck_234); _tupleCheck_229;}) && ({int64_t _literal_236 = 1/*True*/; _literal_236;})))
      {
        _match_160 = ((void*) ({/* (node.comments)(ci) = cont(i) */;
                                _enc__class_String_String_t* _access_165 = array_get(_cont_16, _i_149).p;
                                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_node_3, "comments");
                                array_t* _fieldacc_166 = (*_node_3)._enc__field_comments;
                                array_set(_fieldacc_166, _ci_131, ((encore_arg_t) {.p = _access_165}));
                                /* ci = ci + 1 */;
                                int64_t _binop_168 = (({ _ci_131;}) + ({int64_t _literal_167 = 1; _literal_167;}));
                                _ci_131 = _binop_168;
                                /* () */;
                                UNIT; UNIT;}));
      }
      else
      {
        if ((({int64_t _tupleCheck_221;
               _tupleCheck_221 = 1;
               int64_t _tupleAccess_222 = tuple_get(_tuple_161, 0).i;
               int64_t _valueCheck_223;
               _valueCheck_223 = (({int64_t _literal_224 = 0; _literal_224;}) == _tupleAccess_222);
               _tupleCheck_221 = (_tupleCheck_221 && _valueCheck_223);
               char _tupleAccess_225 = tuple_get(_tuple_161, 1).i;
               int64_t _valueCheck_226;
               _valueCheck_226 = (({char _literal_227 = 't'; _literal_227;}) == _tupleAccess_225);
               _tupleCheck_221 = (_tupleCheck_221 && _valueCheck_226); _tupleCheck_221;}) && ({int64_t _literal_228 = 1/*True*/; _literal_228;})))
        {
          _match_160 = ((void*) ({/* babies(j) = cont(i) */;
                                  _enc__class_String_String_t* _access_169 = array_get(_cont_16, _i_149).p;
                                  array_set(_babies_135, _j_138, ((encore_arg_t) {.p = _access_169}));
                                  /* j = j + 1 */;
                                  int64_t _binop_171 = (({ _j_138;}) + ({int64_t _literal_170 = 1; _literal_170;}));
                                  _j_138 = _binop_171;
                                  /* () */;
                                  UNIT; UNIT;}));
        }
        else
        {
          if ((({int64_t _tupleCheck_213;
                 _tupleCheck_213 = 1;
                 int64_t _tupleAccess_214 = tuple_get(_tuple_161, 0).i;
                 int64_t _valueCheck_215;
                 _valueCheck_215 = (({int64_t _literal_216 = 1; _literal_216;}) == _tupleAccess_214);
                 _tupleCheck_213 = (_tupleCheck_213 && _valueCheck_215);
                 char _tupleAccess_217 = tuple_get(_tuple_161, 1).i;
                 int64_t _valueCheck_218;
                 _valueCheck_218 = (({char _literal_219 = 'e'; _literal_219;}) == _tupleAccess_217);
                 _tupleCheck_213 = (_tupleCheck_213 && _valueCheck_218); _tupleCheck_213;}) && ({int64_t _literal_220 = 1/*True*/; _literal_220;})))
          {
            _match_160 = ((void*) ({/* babies(j) = temp.concatenate(cont(i)) */;
                                    check_receiver(_temp_143, ".", "temp", "concatenate", "\"./Ped_util/XML.enc\" (line 111, column 25)");
                                    _enc__class_String_String_t* _access_173 = array_get(_cont_16, _i_149).p;
                                    pony_type_t* _tmp_174[] = {};
                                    _enc__class_String_String_t* _sync_method_call_172 = _enc__method_String_String_concatenate(_ctx, _temp_143, NULL, _access_173);
                                    array_set(_babies_135, _j_138, ((encore_arg_t) {.p = _sync_method_call_172}));
                                    /* temp = "" */;
                                    _enc__class_String_String_t* _new_175 = _enc__constructor_String_String(_ctx, NULL);
                                    char* _embed_176 = ({"";});
                                    pony_type_t* _tmp_177[] = {};
                                    _enc__type_init_String_String(_new_175);
                                    _enc__method_String_String_init(_ctx, _new_175, NULL, _embed_176);
                                    _temp_143 = _new_175;
                                    /* j = j + 1 */;
                                    int64_t _binop_179 = (({ _j_138;}) + ({int64_t _literal_178 = 1; _literal_178;}));
                                    _j_138 = _binop_179;
                                    /* level = 0 */;
                                    int64_t _literal_180 = 0;
                                    _level_146 = _literal_180;
                                    /* () */;
                                    UNIT; UNIT;}));
          }
          else
          {
            int64_t _X_181;
            if ((({int64_t _tupleCheck_206;
                   _tupleCheck_206 = 1;
                   int64_t _tupleAccess_207 = tuple_get(_tuple_161, 0).i;
                   int64_t _varBinding_208;
                   _X_181 = _tupleAccess_207;
                   _varBinding_208 = 1;
                   _tupleCheck_206 = (_tupleCheck_206 && _varBinding_208);
                   char _tupleAccess_209 = tuple_get(_tuple_161, 1).i;
                   int64_t _valueCheck_210;
                   _valueCheck_210 = (({char _literal_211 = 'e'; _literal_211;}) == _tupleAccess_209);
                   _tupleCheck_206 = (_tupleCheck_206 && _valueCheck_210); _tupleCheck_206;}) && ({int64_t _literal_212 = 1/*True*/; _literal_212;})))
            {
              _match_160 = ((void*) ({/* temp = temp.concatenate(cont(i)) */;
                                      check_receiver(_temp_143, ".", "temp", "concatenate", "\"./Ped_util/XML.enc\" (line 118, column 20)");
                                      _enc__class_String_String_t* _access_183 = array_get(_cont_16, _i_149).p;
                                      pony_type_t* _tmp_184[] = {};
                                      _enc__class_String_String_t* _sync_method_call_182 = _enc__method_String_String_concatenate(_ctx, _temp_143, NULL, _access_183);
                                      _temp_143 = _sync_method_call_182;
                                      /* level = X - 1 */;
                                      int64_t _binop_186 = (({ _X_181;}) - ({int64_t _literal_185 = 1; _literal_185;}));
                                      _level_146 = _binop_186;
                                      /* () */;
                                      UNIT; UNIT;}));
            }
            else
            {
              int64_t _X_187;
              if ((({int64_t _tupleCheck_199;
                     _tupleCheck_199 = 1;
                     int64_t _tupleAccess_200 = tuple_get(_tuple_161, 0).i;
                     int64_t _varBinding_201;
                     _X_187 = _tupleAccess_200;
                     _varBinding_201 = 1;
                     _tupleCheck_199 = (_tupleCheck_199 && _varBinding_201);
                     char _tupleAccess_202 = tuple_get(_tuple_161, 1).i;
                     int64_t _valueCheck_203;
                     _valueCheck_203 = (({char _literal_204 = 's'; _literal_204;}) == _tupleAccess_202);
                     _tupleCheck_199 = (_tupleCheck_199 && _valueCheck_203); _tupleCheck_199;}) && ({int64_t _literal_205 = 1/*True*/; _literal_205;})))
              {
                _match_160 = ((void*) ({/* temp = temp.concatenate(cont(i)) */;
                                        check_receiver(_temp_143, ".", "temp", "concatenate", "\"./Ped_util/XML.enc\" (line 123, column 20)");
                                        _enc__class_String_String_t* _access_189 = array_get(_cont_16, _i_149).p;
                                        pony_type_t* _tmp_190[] = {};
                                        _enc__class_String_String_t* _sync_method_call_188 = _enc__method_String_String_concatenate(_ctx, _temp_143, NULL, _access_189);
                                        _temp_143 = _sync_method_call_188;
                                        /* level = X + 1 */;
                                        int64_t _binop_192 = (({ _X_187;}) + ({int64_t _literal_191 = 1; _literal_191;}));
                                        _level_146 = _binop_192;
                                        /* () */;
                                        UNIT; UNIT;}));
              }
              else
              {
                tuple_t* ___193;
                if ((({int64_t _varBinding_197;
                       ___193 = _tuple_161;
                       _varBinding_197 = 1; _varBinding_197;}) && ({int64_t _literal_198 = 1/*True*/; _literal_198;})))
                {
                  _match_160 = ((void*) ({/* temp = temp.concatenate(cont(i)) */;
                                          check_receiver(_temp_143, ".", "temp", "concatenate", "\"./Ped_util/XML.enc\" (line 128, column 20)");
                                          _enc__class_String_String_t* _access_195 = array_get(_cont_16, _i_149).p;
                                          pony_type_t* _tmp_196[] = {};
                                          _enc__class_String_String_t* _sync_method_call_194 = _enc__method_String_String_concatenate(_ctx, _temp_143, NULL, _access_195);
                                          _temp_143 = _sync_method_call_194;
                                          /* () */;
                                          UNIT; UNIT;}));
                }
                else
                {
                  fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                  exit(1);
                };
              };
            };
          };
        };
      };
      _for_147 = _match_160;
      _index_148 = (_index_148 + _step_152);
    };
    /* node.children = new [XML_node](j) */;
    array_t* _array_237 = array_mk(_ctx, _j_138, (&(_enc__class__Ped_util_XML_XML_node_type)));
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_node_3, "children"); _node_3;}))._enc__field_children = _array_237;
    /* for i <- [0..j - 1] do
  match this.new_XML_node(babies(i)) with
    case Just(xml) =>
      (node.children)(i) = xml
      ()
    end
    case Nothing =>
      bad = true
      ()
    end
  
  end
end */;
    void* _for_238;
    /* Range not generated */;
    int64_t _literal_245 = 0;
    int64_t _binop_247 = (({ _j_138;}) - ({int64_t _literal_246 = 1; _literal_246;}));
    int64_t _literal_248 = 1;
    int64_t _literal_249 = 1;
    int64_t _step_243 = (_literal_249 * _literal_248);
    range_assert_step(_step_243);
    int64_t _index_239;
    if ((_step_243 > 0))
    {
      _index_239 = _literal_245;
    }
    else
    {
      _index_239 = _binop_247;
    };
    while (((_index_239 >= _literal_245) && (_index_239 <= _binop_247)))
    {
      int64_t _i_240 = _index_239;
      check_receiver(_this, ".", "this", "new_XML_node", "\"./Ped_util/XML.enc\" (line 136, column 15)");
      _enc__class_String_String_t* _access_252 = array_get(_babies_135, _i_240).p;
      pony_type_t* _tmp_253[] = {};
      option_t* _sync_method_call_251 = _enc__method__Ped_util_XML_XML_lib_new_XML_node(_ctx, _this, NULL, _access_252);
      void* _match_250;
      _enc__class__Ped_util_XML_XML_node_t* _xml_254;
      if ((({int64_t _optionCheck_261;
             _optionCheck_261 = ((JUST == (*_sync_method_call_251).tag) && ({int64_t _varBinding_262;
                                                                             _enc__class__Ped_util_XML_XML_node_t* _optionVal_260 = (*_sync_method_call_251).val.p;
                                                                             _xml_254 = _optionVal_260;
                                                                             _varBinding_262 = 1; _varBinding_262;})); _optionCheck_261;}) && ({int64_t _literal_263 = 1/*True*/; _literal_263;})))
      {
        _match_250 = ((void*) ({/* (node.children)(i) = xml */;
                                ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_node_3, "children");
                                array_t* _fieldacc_255 = (*_node_3)._enc__field_children;
                                array_set(_fieldacc_255, _i_240, ((encore_arg_t) {.p = _xml_254}));
                                /* () */;
                                UNIT; UNIT;}));
      }
      else
      {
        if ((({int64_t _valueCheck_257;
               _valueCheck_257 = (({option_t* _option_258 = (&(DEFAULT_NOTHING)); _option_258;}) == _sync_method_call_251); _valueCheck_257;}) && ({int64_t _literal_259 = 1/*True*/; _literal_259;})))
        {
          _match_250 = ((void*) ({/* bad = true */;
                                  int64_t _literal_256 = 1/*True*/;
                                  _bad_6 = _literal_256;
                                  /* () */;
                                  UNIT; UNIT;}));
        }
        else
        {
          fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
          exit(1);
        };
      };
      _for_238 = _match_250;
      _index_239 = (_index_239 + _step_243);
    };
    /* if bad == false then
  Just(node)
else
  Nothing
end */;
    option_t* _ite_264;
    if (({int64_t _binop_266 = (({ _bad_6;}) == ({int64_t _literal_265 = 0/*False*/; _literal_265;})); _binop_266;}))
    {
      option_t* _option_267 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _node_3}), (&(_enc__class__Ped_util_XML_XML_node_type)));
      _ite_264 = ((option_t*) _option_267);
    }
    else
    {
      option_t* _option_268 = (&(DEFAULT_NOTHING));
      _ite_264 = ((option_t*) _option_268);
    };
    _ite_119 = ((option_t*) _ite_264);
  }
  else
  {
    option_t* _option_269 = (&(DEFAULT_NOTHING));
    _ite_119 = ((option_t*) _option_269);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "new_XML_node");
  return ((option_t*) _ite_119);
}


void* _enc__method__Ped_util_XML_XML_lib_init(pony_ctx_t** _ctx, _enc__class__Ped_util_XML_XML_lib_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  UNIT;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class__Ped_util_XML_XML_lib_type = {.id=_ENC__ID__Ped_util_XML_XML_lib, .size=sizeof(_enc__class__Ped_util_XML_XML_lib_t), .trace=_enc__trace__Ped_util_XML_XML_lib, .vtable=trait_method_selector};
