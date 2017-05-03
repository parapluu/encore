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


void _enc__type_init__Ped_util_Regions_Item(_enc__class__Ped_util_Regions_Item_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_Regions_Item(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_Regions_Item_t* _this = p;
  option_t* _enc__field_prev = _this->_enc__field_prev;
  encore_trace_object((*_ctx), _enc__field_prev, option_trace);
  option_t* _enc__field_next = _this->_enc__field_next;
  encore_trace_object((*_ctx), _enc__field_next, option_trace);
  _enc__class__Ped_util_Agent_passive_Agent_t* _enc__field_a = _this->_enc__field_a;
  encore_trace_object((*_ctx), _enc__field_a, _enc__trace__Ped_util_Agent_passive_Agent);
}


_enc__class__Ped_util_Regions_Item_t* _enc__constructor__Ped_util_Regions_Item(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_Regions_Item_t* _this = ((_enc__class__Ped_util_Regions_Item_t*) encore_alloc((*_ctx), sizeof(_enc__class__Ped_util_Regions_Item_t)));
  _this->_enc__self_type = (&(_enc__class__Ped_util_Regions_Item_type));
  return _this;
}


void* _enc__method__Ped_util_Regions_Item_append(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Item_t* _this, pony_type_t** runtimeType, option_t* _enc__arg_next)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "append");
  /* this.next = next */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "next"); _this;}))._enc__field_next = _enc__arg_next;
  /* match next with
  case Nothing =>
    ()
  end
  case Just(thing) =>
    thing.prev = Just(this)
  end

end */;
  void* _match_0;
  if ((({int64_t _valueCheck_7;
         _valueCheck_7 = (({option_t* _option_8 = (&(DEFAULT_NOTHING)); _option_8;}) == _enc__arg_next); _valueCheck_7;}) && ({int64_t _literal_9 = 1/*True*/; _literal_9;})))
  {
    _match_0 = ((void*) ({UNIT; UNIT;}));
  }
  else
  {
    _enc__class__Ped_util_Regions_Item_t* _thing_1;
    if ((({int64_t _optionCheck_4;
           _optionCheck_4 = ((JUST == (*_enc__arg_next).tag) && ({int64_t _varBinding_5;
                                                                  _enc__class__Ped_util_Regions_Item_t* _optionVal_3 = (*_enc__arg_next).val.p;
                                                                  _thing_1 = _optionVal_3;
                                                                  _varBinding_5 = 1; _varBinding_5;})); _optionCheck_4;}) && ({int64_t _literal_6 = 1/*True*/; _literal_6;})))
    {
      _match_0 = ((void*) ({option_t* _option_2 = option_mk(_ctx, JUST, ((encore_arg_t) {.p = _this}), (&(_enc__class__Ped_util_Regions_Item_type)));
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_thing_1, "prev"); _thing_1;}))._enc__field_prev = _option_2; UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "append");
  return UNIT;
}


void* _enc__method__Ped_util_Regions_Item_delete(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Item_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "delete");
  tuple_t* _tuple_1 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_1, 0, (&(option_type)));
  tuple_set_type(_tuple_1, 1, (&(option_type)));
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
  option_t* _fieldacc_2 = (*_this)._enc__field_next;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "prev");
  option_t* _fieldacc_3 = (*_this)._enc__field_prev;
  tuple_set(_tuple_1, 0, ((encore_arg_t) {.p = _fieldacc_2}));
  tuple_set(_tuple_1, 1, ((encore_arg_t) {.p = _fieldacc_3}));
  void* _match_0;
  if ((({int64_t _tupleCheck_40;
         _tupleCheck_40 = 1;
         option_t* _tupleAccess_41 = tuple_get(_tuple_1, 0).p;
         int64_t _valueCheck_42;
         _valueCheck_42 = (({option_t* _option_43 = (&(DEFAULT_NOTHING)); _option_43;}) == _tupleAccess_41);
         _tupleCheck_40 = (_tupleCheck_40 && _valueCheck_42);
         option_t* _tupleAccess_44 = tuple_get(_tuple_1, 1).p;
         int64_t _valueCheck_45;
         _valueCheck_45 = (({option_t* _option_46 = (&(DEFAULT_NOTHING)); _option_46;}) == _tupleAccess_44);
         _tupleCheck_40 = (_tupleCheck_40 && _valueCheck_45); _tupleCheck_40;}) && ({int64_t _literal_47 = 1/*True*/; _literal_47;})))
  {
    _match_0 = ((void*) ({UNIT; UNIT;}));
  }
  else
  {
    _enc__class__Ped_util_Regions_Item_t* _next_4;
    _enc__class__Ped_util_Regions_Item_t* _prev_5;
    if ((({int64_t _tupleCheck_30;
           _tupleCheck_30 = 1;
           option_t* _tupleAccess_31 = tuple_get(_tuple_1, 0).p;
           int64_t _optionCheck_33;
           _optionCheck_33 = ((JUST == (*_tupleAccess_31).tag) && ({int64_t _varBinding_34;
                                                                    _enc__class__Ped_util_Regions_Item_t* _optionVal_32 = (*_tupleAccess_31).val.p;
                                                                    _next_4 = _optionVal_32;
                                                                    _varBinding_34 = 1; _varBinding_34;}));
           _tupleCheck_30 = (_tupleCheck_30 && _optionCheck_33);
           option_t* _tupleAccess_35 = tuple_get(_tuple_1, 1).p;
           int64_t _optionCheck_37;
           _optionCheck_37 = ((JUST == (*_tupleAccess_35).tag) && ({int64_t _varBinding_38;
                                                                    _enc__class__Ped_util_Regions_Item_t* _optionVal_36 = (*_tupleAccess_35).val.p;
                                                                    _prev_5 = _optionVal_36;
                                                                    _varBinding_38 = 1; _varBinding_38;}));
           _tupleCheck_30 = (_tupleCheck_30 && _optionCheck_37); _tupleCheck_30;}) && ({int64_t _literal_39 = 1/*True*/; _literal_39;})))
    {
      _match_0 = ((void*) ({/* prev.next = this.next */;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
                            option_t* _fieldacc_6 = (*_this)._enc__field_next;
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_prev_5, "next"); _prev_5;}))._enc__field_next = _fieldacc_6;
                            /* next.prev = this.prev */;
                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "prev");
                            option_t* _fieldacc_7 = (*_this)._enc__field_prev;
                            (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_next_4, "prev"); _next_4;}))._enc__field_prev = _fieldacc_7; UNIT;}));
    }
    else
    {
      _enc__class__Ped_util_Regions_Item_t* _prev_8;
      if ((({int64_t _tupleCheck_21;
             _tupleCheck_21 = 1;
             option_t* _tupleAccess_22 = tuple_get(_tuple_1, 0).p;
             int64_t _valueCheck_23;
             _valueCheck_23 = (({option_t* _option_24 = (&(DEFAULT_NOTHING)); _option_24;}) == _tupleAccess_22);
             _tupleCheck_21 = (_tupleCheck_21 && _valueCheck_23);
             option_t* _tupleAccess_25 = tuple_get(_tuple_1, 1).p;
             int64_t _optionCheck_27;
             _optionCheck_27 = ((JUST == (*_tupleAccess_25).tag) && ({int64_t _varBinding_28;
                                                                      _enc__class__Ped_util_Regions_Item_t* _optionVal_26 = (*_tupleAccess_25).val.p;
                                                                      _prev_8 = _optionVal_26;
                                                                      _varBinding_28 = 1; _varBinding_28;}));
             _tupleCheck_21 = (_tupleCheck_21 && _optionCheck_27); _tupleCheck_21;}) && ({int64_t _literal_29 = 1/*True*/; _literal_29;})))
      {
        _match_0 = ((void*) ({option_t* _option_9 = (&(DEFAULT_NOTHING));
                              (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_prev_8, "next"); _prev_8;}))._enc__field_next = _option_9; UNIT;}));
      }
      else
      {
        _enc__class__Ped_util_Regions_Item_t* _next_10;
        if ((({int64_t _tupleCheck_12;
               _tupleCheck_12 = 1;
               option_t* _tupleAccess_13 = tuple_get(_tuple_1, 0).p;
               int64_t _optionCheck_15;
               _optionCheck_15 = ((JUST == (*_tupleAccess_13).tag) && ({int64_t _varBinding_16;
                                                                        _enc__class__Ped_util_Regions_Item_t* _optionVal_14 = (*_tupleAccess_13).val.p;
                                                                        _next_10 = _optionVal_14;
                                                                        _varBinding_16 = 1; _varBinding_16;}));
               _tupleCheck_12 = (_tupleCheck_12 && _optionCheck_15);
               option_t* _tupleAccess_17 = tuple_get(_tuple_1, 1).p;
               int64_t _valueCheck_18;
               _valueCheck_18 = (({option_t* _option_19 = (&(DEFAULT_NOTHING)); _option_19;}) == _tupleAccess_17);
               _tupleCheck_12 = (_tupleCheck_12 && _valueCheck_18); _tupleCheck_12;}) && ({int64_t _literal_20 = 1/*True*/; _literal_20;})))
        {
          _match_0 = ((void*) ({option_t* _option_11 = (&(DEFAULT_NOTHING));
                                (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_next_10, "prev"); _next_10;}))._enc__field_prev = _option_11; UNIT;}));
        }
        else
        {
          fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
          exit(1);
        };
      };
    };
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "delete");
  return UNIT;
}


void* _enc__method__Ped_util_Regions_Item_init(pony_ctx_t** _ctx, _enc__class__Ped_util_Regions_Item_t* _this, pony_type_t** runtimeType, _enc__class__Ped_util_Agent_passive_Agent_t* _enc__arg_a)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  /* this.a = a */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "a"); _this;}))._enc__field_a = _enc__arg_a;
  /* this.next = Nothing */;
  option_t* _option_0 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "next"); _this;}))._enc__field_next = _option_0;
  /* this.prev = Nothing */;
  option_t* _option_1 = (&(DEFAULT_NOTHING));
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "prev"); _this;}))._enc__field_prev = _option_1;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class__Ped_util_Regions_Item_type = {.id=_ENC__ID__Ped_util_Regions_Item, .size=sizeof(_enc__class__Ped_util_Regions_Item_t), .trace=_enc__trace__Ped_util_Regions_Item, .vtable=trait_method_selector};
