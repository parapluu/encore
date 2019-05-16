#include "header.h"


static void* trait_method_selector(int id)
{
  switch (id)
  {
    case _ENC__MSG_Hashable_Hashable_hash_equals:
    {
      return _enc__method_String_String_hash_equals;
      break;
    }
    case _ENC__MSG_Hashable_Hashable_get_hash:
    {
      return _enc__method_String_String_get_hash;
      break;
    }
    case _ENC__MSG_Hashable_Hashable_hash_address:
    {
      return _enc__method_String_String_hash_address;
      break;
    }
    case _ENC__MSG_Std_Eq_eq:
    {
      return _enc__method_String_String_eq;
      break;
    }
    default:
    {
      printf("error, got invalid id: %d", id);
    }
  };
  return NULL;
}


void _enc__type_init_String_String(_enc__class_String_String_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace_String_String(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class_String_String_t* _this = p;
  uint64_t _enc__field_hash_code = _this->_enc__field_hash_code;
  /* Not tracing field '_enc__field_hash_code' */;
  int64_t _enc__field_length = _this->_enc__field_length;
  /* Not tracing field '_enc__field_length' */;
  char* _enc__field_cstring = _this->_enc__field_cstring;
  pony_trace((*_ctx), _enc__field_cstring);
}


_enc__class_String_String_t* _enc__constructor_String_String(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class_String_String_t* _this = ((_enc__class_String_String_t*) encore_alloc((*_ctx), sizeof(_enc__class_String_String_t)));
  _this->_enc__self_type = (&(_enc__class_String_String_type));
  return _this;
}


option_t* _enc__method_String_String_to_real(pony_ctx_t** _ctx, _enc__class_String_String_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "to_real");
  /* val s = this.cstring */;
  /* s = this.cstring */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "cstring");
  char* _fieldacc_0 = (*_this)._enc__field_cstring;
  char* _s_2 = _fieldacc_0;
  /* var n : real = 0.0 */;
  /* n = 0.0 */;
  double _literal_3 = 0.0;
  double _n_5 = ((double) _literal_3);
  /* var success = false */;
  /* success = false */;
  int64_t _literal_6 = 0/*False*/;
  int64_t _success_8 = _literal_6;
  /* EMBED (unit)
  char *s = #{s};
      char *endptr;
      #{n}= strtod(s,&endptr);
      if (s != endptr)
         #{success}= true;
END */;
  ({char *s = _s_2;
      char *endptr;
      _n_5= strtod(s,&endptr);
      if (s != endptr)
         _success_8= true;});
  /* if success then
  Just(n)
else
  Nothing
end */;
  option_t* _ite_9;
  if (({ _success_8;}))
  {
    option_t* _option_10 = option_mk(_ctx, JUST, ((encore_arg_t) {.d = _n_5}), ENCORE_PRIMITIVE);
    _ite_9 = ((option_t*) _option_10);
  }
  else
  {
    option_t* _option_11 = (&(DEFAULT_NOTHING));
    _ite_9 = ((option_t*) _option_11);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "to_real");
  return ((option_t*) _ite_9);
}


option_t* _enc__method_String_String_to_bool(pony_ctx_t** _ctx, _enc__class_String_String_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "to_bool");
  option_t* _ite_0;
  if (({check_receiver(_this, ".", "this", "eq", "\"/home/joy/encore/modules/standard/String.enc\" (line 479, column 8)");
        _enc__class_String_String_t* _new_2 = _enc__constructor_String_String(_ctx, NULL);
        char* _embed_3 = ({"true";});
        pony_type_t* _tmp_4[] = {};
        _enc__type_init_String_String(_new_2);
        _enc__method_String_String_init(_ctx, _new_2, NULL, _embed_3);
        pony_type_t* _tmp_5[] = {};
        int64_t _sync_method_call_1 = _enc__method_String_String_eq(_ctx, _this, NULL, _new_2); _sync_method_call_1;}))
  {
    int64_t _literal_6 = 1/*True*/;
    option_t* _option_7 = option_mk(_ctx, JUST, ((encore_arg_t) {.i = _literal_6}), ENCORE_PRIMITIVE);
    _ite_0 = ((option_t*) _option_7);
  }
  else
  {
    option_t* _ite_8;
    if (({check_receiver(_this, ".", "this", "eq", "\"/home/joy/encore/modules/standard/String.enc\" (line 481, column 13)");
          _enc__class_String_String_t* _new_10 = _enc__constructor_String_String(_ctx, NULL);
          char* _embed_11 = ({"false";});
          pony_type_t* _tmp_12[] = {};
          _enc__type_init_String_String(_new_10);
          _enc__method_String_String_init(_ctx, _new_10, NULL, _embed_11);
          pony_type_t* _tmp_13[] = {};
          int64_t _sync_method_call_9 = _enc__method_String_String_eq(_ctx, _this, NULL, _new_10); _sync_method_call_9;}))
    {
      int64_t _literal_14 = 0/*False*/;
      option_t* _option_15 = option_mk(_ctx, JUST, ((encore_arg_t) {.i = _literal_14}), ENCORE_PRIMITIVE);
      _ite_8 = ((option_t*) _option_15);
    }
    else
    {
      option_t* _option_16 = (&(DEFAULT_NOTHING));
      _ite_8 = ((option_t*) _option_16);
    };
    _ite_0 = ((option_t*) _ite_8);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "to_bool");
  return ((option_t*) _ite_0);
}


option_t* _enc__method_String_String_to_int(pony_ctx_t** _ctx, _enc__class_String_String_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "to_int");
  /* val s = this.cstring */;
  /* s = this.cstring */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "cstring");
  char* _fieldacc_0 = (*_this)._enc__field_cstring;
  char* _s_2 = _fieldacc_0;
  /* var n = 0 */;
  /* n = 0 */;
  int64_t _literal_3 = 0;
  int64_t _n_5 = _literal_3;
  /* var success = false */;
  /* success = false */;
  int64_t _literal_6 = 0/*False*/;
  int64_t _success_8 = _literal_6;
  /* EMBED (unit)
  char *s = #{s};
      char *endptr;
      #{n}= strtol(s,&endptr,0);
      if (s != endptr)
         #{success}= true;
END */;
  ({char *s = _s_2;
      char *endptr;
      _n_5= strtol(s,&endptr,0);
      if (s != endptr)
         _success_8= true;});
  /* if success then
  Just(n)
else
  Nothing
end */;
  option_t* _ite_9;
  if (({ _success_8;}))
  {
    option_t* _option_10 = option_mk(_ctx, JUST, ((encore_arg_t) {.i = _n_5}), ENCORE_PRIMITIVE);
   