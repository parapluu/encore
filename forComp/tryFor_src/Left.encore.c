#include "header.h"


static void* trait_method_selector(int id)
{
  switch (id)
  {
    case _ENC__MSG_Either_Either_Right:
    {
      return _enc__method_Either_Left_Right;
      break;
    }
    case _ENC__MSG_Either_Either_Left:
    {
      return _enc__method_Either_Left_Left;
      break;
    }
    case _ENC__MSG_Either_Either_flatMap:
    {
      return _enc__method_Either_Left_flatMap;
      break;
    }
    case _ENC__MSG_Either_Either_foreach:
    {
      return _enc__method_Either_Left_foreach;
      break;
    }
    case _ENC__MSG_Either_Either_map:
    {
      return _enc__method_Either_Left_map;
      break;
    }
    default:
    {
      printf("error, got invalid id: %d", id);
    }
  };
  return NULL;
}


void _enc__type_init_Either_Left(_enc__class_Either_Left_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  _this->_enc__type_a = va_arg(params, pony_type_t *);
  _this->_enc__type_b = va_arg(params, pony_type_t *);
  va_end(params);
}


void _enc__trace_Either_Left(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class_Either_Left_t* _this = p;
  pony_type_t* _enc__type_a = _this->_enc__type_a;
  pony_type_t* _enc__type_b = _this->_enc__type_b;
  encore_arg_t _enc__field_x = _this->_enc__field_x;
  encore_trace_polymorphic_variable((*_ctx), _enc__type_a, _enc__field_x);
}


_enc__class_Either_Left_t* _enc__constructor_Either_Left(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class_Either_Left_t* _this = ((_enc__class_Either_Left_t*) encore_alloc((*_ctx), sizeof(_enc__class_Either_Left_t)));
  _this->_enc__self_type = (&(_enc__class_Either_Left_type));
  return _this;
}


option_t* _enc__method_Either_Left_Right(pony_ctx_t** _ctx, _enc__class_Either_Left_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "Right");
  pony_type_t* _enc__type_a = (*((_enc__class_Either_Left_t*) _this))._enc__type_a;
  pony_type_t* _enc__type_b = (*((_enc__class_Either_Left_t*) _this))._enc__type_b;
  option_t* _option_0 = (&(DEFAULT_NOTHING));
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "Right");
  return ((option_t*) _option_0);
}


option_t* _enc__method_Either_Left_Left(pony_ctx_t** _ctx, _enc__class_Either_Left_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "Left");
  pony_type_t* _enc__type_a = (*((_enc__class_Either_Left_t*) _this))._enc__type_a;
  pony_type_t* _enc__type_b = (*((_enc__class_Either_Left_t*) _this))._enc__type_b;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "x");
  encore_arg_t _fieldacc_0 = (*_this)._enc__field_x;
  option_t* _option_1 = option_mk(_ctx, JUST, _fieldacc_0, _enc__type_a);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "Left");
  return ((option_t*) _option_1);
}


void* _enc__method_Either_Left_init(pony_ctx_t** _ctx, _enc__class_Either_Left_t* _this, pony_type_t** runtimeType, encore_arg_t _enc__arg_x)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  pony_type_t* _enc__type_a = (*((_enc__class_Either_Left_t*) _this))._enc__type_a;
  pony_type_t* _enc__type_b = (*((_enc__class_Either_Left_t*) _this))._enc__type_b;
  /* this.x = x */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "x"); _this;}))._enc__field_x = _enc__arg_x;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


_enc__trait_Either_Either_t* _enc__method_Either_Left_flatMap(pony_ctx_t** _ctx, _enc__class_Either_Left_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "flatMap");
  pony_type_t* _enc__type__c = (runtimeType[0]);
  pony_type_t* _enc__type_a = (*((_enc__class_Either_Left_t*) _this))._enc__type_a;
  pony_type_t* _enc__type_b = (*((_enc__class_Either_Left_t*) _this))._enc__type_b;
  _enc__trait_Either_Either_t* _match_0;
  encore_arg_t _x_1;
  if ((({int64_t _extractoCheck_17;
         _extractoCheck_17 = ((_this != NULL) && ({int64_t _optionCheck_15;
                                                   option_t* _extractedOption_13 = _enc__method_Either_Left_Left(_ctx, _this, NULL);
                                                   _optionCheck_15 = ((JUST == (*_extractedOption_13).tag) && ({int64_t _varBinding_16;
                                                                                                                encore_arg_t _optionVal_14 = (*_extractedOption_13).val;
                                                                                                                _x_1 = _optionVal_14;
                                                                                                                _varBinding_16 = 1; _varBinding_16;})); _optionCheck_15;})); _extractoCheck_17;}) && ({int64_t _literal_18 = 1/*True*/; _literal_18;})))
  {
    _match_0 = ((_enc__trait_Either_Either_t*) ({_enc__class_Either_Left_t* _new_2 = _enc__constructor_Either_Left(_ctx, NULL);
                                                 pony_type_t* _tmp_3[] = {};
                                                 _enc__type_init_Either_Left(_new_2, _enc__type_a, _enc__type__c);
                                                 _enc__method_Either_Left_init(_ctx, _new_2, NULL, _x_1); _new_2;}));
  }
  else
  {
    encore_arg_t _x_4;
    if ((({int64_t _extractoCheck_11;
           _extractoCheck_11 = ((_this != NULL) && ({int64_t _optionCheck_9;
                                                     option_t* _extractedOption_7 = _enc__method_Either_Left_Right(_ctx, _this, NULL);
                                                     _optionCheck_9 = ((JUST == (*_extractedOption_7).tag) && ({int64_t _varBinding_10;
                                                                                                                encore_arg_t _optionVal_8 = (*_extractedOption_7).val;
                                                                                                                _x_4 = _optionVal_8;
                                                                                                                _varBinding_10 = 1; _varBinding_10;})); _optionCheck_9;})); _extractoCheck_11;}) && ({int64_t _literal_12 = 1/*True*/; _literal_12;})))
    {
      _match_0 = ((_enc__trait_Either_Either_t*) ({value_t _tmp_5[] = {({ _x_4;})};
                                                   ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
                                                   _enc__trait_Either_Either_t* _clos_6 = closure_call(_ctx, _enc__arg_f, _tmp_5).p; _clos_6;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Data/Either.enc\" (line 43, column 5) ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "flatMap");
  return ((_enc__trait_Either_Either_t*) _match_0);
}


void* _enc__method_Either_Left_foreach(pony_ctx_t** _ctx, _enc__class_Either_Left_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "foreach");
  pony_type_t* _enc__type_a = (*((_enc__class_Either_Left_t*) _this))._enc__type_a;
  pony_type_t* _enc__type_b = (*((_enc__class_Either_Left_t*) _this))._enc__type_b;
  void* _match_0;
  encore_arg_t _x_1;
  if ((({int64_t _extractoCheck_14;
         _extractoCheck_14 = ((_this != NULL) && ({int64_t _optionCheck_12;
                                                   option_t* _extractedOption_10 = _enc__method_Either_Left_Left(_ctx, _this, NULL);
                                                   _optionCheck_12 = ((JUST == (*_extractedOption_10).tag) && ({int64_t _varBinding_13;
                                                                                                                encore_arg_t _optionVal_11 = (*_extractedOption_10).val;
                                                                                                                _x_1 = _optionVal_11;
                                                                                                                _varBinding_13 = 1; _varBinding_13;})); _optionCheck_12;})); _extractoCheck_14;}) && ({int64_t _literal_15 = 1/*True*/; _literal_15;})))
  {
    _match_0 = ((void*) ({UNIT; UNIT;}));
  }
  else
  {
    encore_arg_t _x_2;
    if ((({int64_t _extractoCheck_8;
           _extractoCheck_8 = ((_this != NULL) && ({int64_t _optionCheck_6;
                                                    option_t* _extractedOption_4 = _enc__method_Either_Left_Right(_ctx, _this, NULL);
                                                    _optionCheck_6 = ((JUST == (*_extractedOption_4).tag) && ({int64_t _varBinding_7;
                                                                                                               encore_arg_t _optionVal_5 = (*_extractedOption_4).val;
                                                                                                               _x_2 = _optionVal_5;
                                                                                                               _varBinding_7 = 1; _varBinding_7;})); _optionCheck_6;})); _extractoCheck_8;}) && ({int64_t _literal_9 = 1/*True*/; _literal_9;})))
    {
      _match_0 = ((void*) ({value_t _tmp_3[] = {({ _x_2;})};
                            ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
                            closure_call(_ctx, _enc__arg_f, _tmp_3).p; UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Data/Either.enc\" (line 31, column 5) ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "foreach");
  return UNIT;
}


_enc__trait_Either_Either_t* _enc__method_Either_Left_map(pony_ctx_t** _ctx, _enc__class_Either_Left_t* _this, pony_type_t** runtimeType, closure_t* _enc__arg_f)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "map");
  pony_type_t* _enc__type__c = (runtimeType[0]);
  pony_type_t* _enc__type_a = (*((_enc__class_Either_Left_t*) _this))._enc__type_a;
  pony_type_t* _enc__type_b = (*((_enc__class_Either_Left_t*) _this))._enc__type_b;
  _enc__trait_Either_Either_t* _match_0;
  encore_arg_t _x_1;
  if ((({int64_t _extractoCheck_19;
         _extractoCheck_19 = ((_this != NULL) && ({int64_t _optionCheck_17;
                                                   option_t* _extractedOption_15 = _enc__method_Either_Left_Left(_ctx, _this, NULL);
                                                   _optionCheck_17 = ((JUST == (*_extractedOption_15).tag) && ({int64_t _varBinding_18;
                                                                                                                encore_arg_t _optionVal_16 = (*_extractedOption_15).val;
                                                                                                                _x_1 = _optionVal_16;
                                                                                                                _varBinding_18 = 1; _varBinding_18;})); _optionCheck_17;})); _extractoCheck_19;}) && ({int64_t _literal_20 = 1/*True*/; _literal_20;})))
  {
    _match_0 = ((_enc__trait_Either_Either_t*) ({_enc__class_Either_Left_t* _new_2 = _enc__constructor_Either_Left(_ctx, NULL);
                                                 pony_type_t* _tmp_3[] = {};
                                                 _enc__type_init_Either_Left(_new_2, _enc__type_a, _enc__type__c);
                                                 _enc__method_Either_Left_init(_ctx, _new_2, NULL, _x_1); _new_2;}));
  }
  else
  {
    encore_arg_t _x_4;
    if ((({int64_t _extractoCheck_13;
           _extractoCheck_13 = ((_this != NULL) && ({int64_t _optionCheck_11;
                                                     option_t* _extractedOption_9 = _enc__method_Either_Left_Right(_ctx, _this, NULL);
                                                     _optionCheck_11 = ((JUST == (*_extractedOption_9).tag) && ({int64_t _varBinding_12;
                                                                                                                 encore_arg_t _optionVal_10 = (*_extractedOption_9).val;
                                                                                                                 _x_4 = _optionVal_10;
                                                                                                                 _varBinding_12 = 1; _varBinding_12;})); _optionCheck_11;})); _extractoCheck_13;}) && ({int64_t _literal_14 = 1/*True*/; _literal_14;})))
    {
      _match_0 = ((_enc__trait_Either_Either_t*) ({_enc__class_Either_Right_t* _new_5 = _enc__constructor_Either_Right(_ctx, NULL);
                                                   value_t _tmp_6[] = {({ _x_4;})};
                                                   ENC_DTRACE2(CLOSURE_CALL, (uintptr_t)*_ctx, "f");
                                                   encore_arg_t _clos_7 = closure_call(_ctx, _enc__arg_f, _tmp_6);
                                                   pony_type_t* _tmp_8[] = {};
                                                   _enc__type_init_Either_Right(_new_5, _enc__type_a, _enc__type__c);
                                                   _enc__method_Either_Right_init(_ctx, _new_5, NULL, _clos_7); _new_5;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found at \"/home/joy/encore/modules/standard/Data/Either.enc\" (line 19, column 5) ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "map");
  return ((_enc__trait_Either_Either_t*) _match_0);
}


pony_type_t _enc__class_Either_Left_type = {.id=_ENC__ID_Either_Left, .size=sizeof(_enc__class_Either_Left_t), .trace=_enc__trace_Either_Left, .vtable=trait_method_selector};
