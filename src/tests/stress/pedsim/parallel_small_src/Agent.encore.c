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


void _enc__type_init__Ped_util_Agent_passive_Agent(_enc__class__Ped_util_Agent_passive_Agent_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_Agent_passive_Agent(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_Agent_passive_Agent_t* _this = p;
  int64_t _enc__field_ttl = _this->_enc__field_ttl;
  /* Not tracing field '_enc__field_ttl' */;
  int64_t _enc__field_i = _this->_enc__field_i;
  /* Not tracing field '_enc__field_i' */;
  array_t* _enc__field_targets_size = _this->_enc__field_targets_size;
  encore_trace_object((*_ctx), _enc__field_targets_size, array_trace);
  array_t* _enc__field_targets_y = _this->_enc__field_targets_y;
  encore_trace_object((*_ctx), _enc__field_targets_y, array_trace);
  array_t* _enc__field_targets_x = _this->_enc__field_targets_x;
  encore_trace_object((*_ctx), _enc__field_targets_x, array_trace);
  int64_t _enc__field_y = _this->_enc__field_y;
  /* Not tracing field '_enc__field_y' */;
  int64_t _enc__field_x = _this->_enc__field_x;
  /* Not tracing field '_enc__field_x' */;
  uint64_t _enc__field_hash_code = _this->_enc__field_hash_code;
  /* Not tracing field '_enc__field_hash_code' */;
}


_enc__class__Ped_util_Agent_passive_Agent_t* _enc__constructor__Ped_util_Agent_passive_Agent(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_Agent_passive_Agent_t* _this = ((_enc__class__Ped_util_Agent_passive_Agent_t*) encore_alloc((*_ctx), sizeof(_enc__class__Ped_util_Agent_passive_Agent_t)));
  _this->_enc__self_type = (&(_enc__class__Ped_util_Agent_passive_Agent_type));
  return _this;
}


tuple_t* _enc__method__Ped_util_Agent_passive_Agent_pos(pony_ctx_t** _ctx, _enc__class__Ped_util_Agent_passive_Agent_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "pos");
  tuple_t* _tuple_0 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_0, 0, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_0, 1, ENCORE_PRIMITIVE);
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "x");
  int64_t _fieldacc_1 = (*_this)._enc__field_x;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "y");
  int64_t _fieldacc_2 = (*_this)._enc__field_y;
  tuple_set(_tuple_0, 0, ((encore_arg_t) {.i = _fieldacc_1}));
  tuple_set(_tuple_0, 1, ((encore_arg_t) {.i = _fieldacc_2}));
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "pos");
  return ((tuple_t*) _tuple_0);
}


void* _enc__method__Ped_util_Agent_passive_Agent_move_int(pony_ctx_t** _ctx, _enc__class__Ped_util_Agent_passive_Agent_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "move_int");
  /* this.x = x */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "x"); _this;}))._enc__field_x = _enc__arg_x;
  /* this.y = y */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "y"); _this;}))._enc__field_y = _enc__arg_y;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "move_int");
  return UNIT;
}


void* _enc__method__Ped_util_Agent_passive_Agent_next(pony_ctx_t** _ctx, _enc__class__Ped_util_Agent_passive_Agent_t* _this, pony_type_t** runtimeType, array_t* _enc__arg_ret)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
  /* x1 = this.x */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "x");
  int64_t _fieldacc_0 = (*_this)._enc__field_x;
  int64_t _x1_2 = _fieldacc_0;
  /* y1 = this.y */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "y");
  int64_t _fieldacc_3 = (*_this)._enc__field_y;
  int64_t _y1_5 = _fieldacc_3;
  /* var x2 = (this.targets_x)(this.i) */;
  /* x2 = (this.targets_x)(this.i) */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_x");
  array_t* _fieldacc_6 = (*_this)._enc__field_targets_x;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
  int64_t _fieldacc_7 = (*_this)._enc__field_i;
  int64_t _access_8 = array_get(_fieldacc_6, _fieldacc_7).i;
  int64_t _x2_10 = _access_8;
  /* var y2 = (this.targets_y)(this.i) */;
  /* y2 = (this.targets_y)(this.i) */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_y");
  array_t* _fieldacc_11 = (*_this)._enc__field_targets_y;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
  int64_t _fieldacc_12 = (*_this)._enc__field_i;
  int64_t _access_13 = array_get(_fieldacc_11, _fieldacc_12).i;
  int64_t _y2_15 = _access_13;
  /* var xdiff = x1 - x2 */;
  /* xdiff = x1 - x2 */;
  int64_t _binop_16 = (({ _x1_2;}) - ({ _x2_10;}));
  int64_t _xdiff_18 = _binop_16;
  /* var ydiff = y1 - y2 */;
  /* ydiff = y1 - y2 */;
  int64_t _binop_19 = (({ _y1_5;}) - ({ _y2_15;}));
  int64_t _ydiff_21 = _binop_19;
  /* var lenSquare = xdiff * xdiff + ydiff * ydiff */;
  /* lenSquare = xdiff * xdiff + ydiff * ydiff */;
  int64_t _binop_24 = (({int64_t _binop_22 = (({ _xdiff_18;}) * ({ _xdiff_18;})); _binop_22;}) + ({int64_t _binop_23 = (({ _ydiff_21;}) * ({ _ydiff_21;})); _binop_23;}));
  int64_t _lenSquare_26 = _binop_24;
  /* var invlen = inv_sqrt(lenSquare) */;
  /* invlen = inv_sqrt(lenSquare) */;
  ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.inv_sqrt");
  pony_type_t* _tmp_27[] = {};
  double _fun_call_28 = _enc__global_fun__Ped_util_Global_funsinv_sqrt(_ctx, NULL, _lenSquare_26);
  double _invlen_30 = _fun_call_28;
  /* if lenSquare < this.targets_size(this.i) * this.targets_size(this.i) then
  this.i = this.i + 1
end */;
  void* _ite_31;
  if (({int64_t _binop_39 = (({ _lenSquare_26;}) < ({int64_t _binop_38 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_size");
                                                                            array_t* _fieldacc_32 = (*_this)._enc__field_targets_size;
                                                                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
                                                                            int64_t _fieldacc_33 = (*_this)._enc__field_i;
                                                                            int64_t _access_34 = array_get(_fieldacc_32, _fieldacc_33).i; _access_34;}) * ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_size");
                                                                                                                                                            array_t* _fieldacc_35 = (*_this)._enc__field_targets_size;
                                                                                                                                                            ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
                                                                                                                                                            int64_t _fieldacc_36 = (*_this)._enc__field_i;
                                                                                                                                                            int64_t _access_37 = array_get(_fieldacc_35, _fieldacc_36).i; _access_37;})); _binop_38;})); _binop_39;}))
  {
    int64_t _binop_42 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
                           int64_t _fieldacc_40 = (*_this)._enc__field_i; _fieldacc_40;}) + ({int64_t _literal_41 = 1; _literal_41;}));
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "i"); _this;}))._enc__field_i = _binop_42;
    _ite_31 = ((void*) UNIT);
  }
  else
  {
    UNIT;
    _ite_31 = ((void*) UNIT);
  };
  /* if |this.targets_size| == this.i then
  this.i = 0
end */;
  void* _ite_43;
  if (({int64_t _binop_47 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_size");
                               array_t* _fieldacc_44 = (*_this)._enc__field_targets_size;
                               int64_t _size_45 = array_size(_fieldacc_44); _size_45;}) == ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
                                                                                             int64_t _fieldacc_46 = (*_this)._enc__field_i; _fieldacc_46;})); _binop_47;}))
  {
    int64_t _literal_48 = 0;
    (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "i"); _this;}))._enc__field_i = _literal_48;
    _ite_43 = ((void*) UNIT);
  }
  else
  {
    UNIT;
    _ite_43 = ((void*) UNIT);
  };
  /* x2 = (this.targets_x)(this.i) */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_x");
  array_t* _fieldacc_49 = (*_this)._enc__field_targets_x;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
  int64_t _fieldacc_50 = (*_this)._enc__field_i;
  int64_t _access_51 = array_get(_fieldacc_49, _fieldacc_50).i;
  _x2_10 = _access_51;
  /* y2 = (this.targets_y)(this.i) */;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_y");
  array_t* _fieldacc_52 = (*_this)._enc__field_targets_y;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "i");
  int64_t _fieldacc_53 = (*_this)._enc__field_i;
  int64_t _access_54 = array_get(_fieldacc_52, _fieldacc_53).i;
  _y2_15 = _access_54;
  /* xdiff = x1 - x2 */;
  int64_t _binop_55 = (({ _x1_2;}) - ({ _x2_10;}));
  _xdiff_18 = _binop_55;
  /* ydiff = y1 - y2 */;
  int64_t _binop_56 = (({ _y1_5;}) - ({ _y2_15;}));
  _ydiff_21 = _binop_56;
  /* lenSquare = xdiff * xdiff + ydiff * ydiff */;
  int64_t _binop_59 = (({int64_t _binop_57 = (({ _xdiff_18;}) * ({ _xdiff_18;})); _binop_57;}) + ({int64_t _binop_58 = (({ _ydiff_21;}) * ({ _ydiff_21;})); _binop_58;}));
  _lenSquare_26 = _binop_59;
  /* invlen = inv_sqrt(lenSquare) */;
  ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.inv_sqrt");
  pony_type_t* _tmp_60[] = {};
  double _fun_call_61 = _enc__global_fun__Ped_util_Global_funsinv_sqrt(_ctx, NULL, _lenSquare_26);
  _invlen_30 = _fun_call_61;
  /* let
  xoff = xdiff * invlen
  yoff = ydiff * invlen
  desired_x = x1 + round(xoff * -(1))
  desired_y = y1 + round(yoff * -(1))
in
  val dx = desired_x - x1
  val dy = desired_y - y1
  ret(0) = desired_x
  ret(1) = desired_y
  if dx == 0 || dy == 0 then
    ret(2) = desired_x + dy
    ret(3) = desired_y + dx
    ret(4) = desired_x - dy
    ret(5) = desired_y - dx
  else
    ret(2) = desired_x
    ret(3) = y1
    ret(4) = x1
    ret(5) = desired_y
  end
end */;
  /* xoff = xdiff * invlen */;
  double _binop_62 = (({ _xdiff_18;}) * ({ _invlen_30;}));
  double _xoff_64 = _binop_62;
  /* yoff = ydiff * invlen */;
  double _binop_65 = (({ _ydiff_21;}) * ({ _invlen_30;}));
  double _yoff_67 = _binop_65;
  /* desired_x = x1 + round(xoff * -(1)) */;
  int64_t _binop_73 = (({ _x1_2;}) + ({double _binop_70 = (({ _xoff_64;}) * ({int64_t _literal_68 = 1;
                                                                              int64_t _unary_69 = (- _literal_68); _unary_69;}));
                                       ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.round");
                                       pony_type_t* _tmp_71[] = {};
                                       int64_t _fun_call_72 = _enc__global_fun__Ped_util_Global_funsround(_ctx, NULL, _binop_70); _fun_call_72;}));
  int64_t _desired_x_75 = _binop_73;
  /* desired_y = y1 + round(yoff * -(1)) */;
  int64_t _binop_81 = (({ _y1_5;}) + ({double _binop_78 = (({ _yoff_67;}) * ({int64_t _literal_76 = 1;
                                                                              int64_t _unary_77 = (- _literal_76); _unary_77;}));
                                       ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.round");
                                       pony_type_t* _tmp_79[] = {};
                                       int64_t _fun_call_80 = _enc__global_fun__Ped_util_Global_funsround(_ctx, NULL, _binop_78); _fun_call_80;}));
  int64_t _desired_y_83 = _binop_81;
  /* val dx = desired_x - x1 */;
  /* dx = desired_x - x1 */;
  int64_t _binop_84 = (({ _desired_x_75;}) - ({ _x1_2;}));
  int64_t _dx_86 = _binop_84;
  /* val dy = desired_y - y1 */;
  /* dy = desired_y - y1 */;
  int64_t _binop_87 = (({ _desired_y_83;}) - ({ _y1_5;}));
  int64_t _dy_89 = _binop_87;
  /* ret(0) = desired_x */;
  int64_t _literal_90 = 0;
  array_set(_enc__arg_ret, _literal_90, ((encore_arg_t) {.i = _desired_x_75}));
  /* ret(1) = desired_y */;
  int64_t _literal_91 = 1;
  array_set(_enc__arg_ret, _literal_91, ((encore_arg_t) {.i = _desired_y_83}));
  /* if dx == 0 || dy == 0 then
  ret(2) = desired_x + dy
  ret(3) = desired_y + dx
  ret(4) = desired_x - dy
  ret(5) = desired_y - dx
else
  ret(2) = desired_x
  ret(3) = y1
  ret(4) = x1
  ret(5) = desired_y
end */;
  void* _ite_92;
  if (({int64_t _binop_97 = (({int64_t _binop_94 = (({ _dx_86;}) == ({int64_t _literal_93 = 0; _literal_93;})); _binop_94;}) || ({int64_t _binop_96 = (({ _dy_89;}) == ({int64_t _literal_95 = 0; _literal_95;})); _binop_96;})); _binop_97;}))
  {
    /* ret(2) = desired_x + dy */;
    int64_t _binop_98 = (({ _desired_x_75;}) + ({ _dy_89;}));
    int64_t _literal_99 = 2;
    array_set(_enc__arg_ret, _literal_99, ((encore_arg_t) {.i = _binop_98}));
    /* ret(3) = desired_y + dx */;
    int64_t _binop_100 = (({ _desired_y_83;}) + ({ _dx_86;}));
    int64_t _literal_101 = 3;
    array_set(_enc__arg_ret, _literal_101, ((encore_arg_t) {.i = _binop_100}));
    /* ret(4) = desired_x - dy */;
    int64_t _binop_102 = (({ _desired_x_75;}) - ({ _dy_89;}));
    int64_t _literal_103 = 4;
    array_set(_enc__arg_ret, _literal_103, ((encore_arg_t) {.i = _binop_102}));
    /* ret(5) = desired_y - dx */;
    int64_t _binop_104 = (({ _desired_y_83;}) - ({ _dx_86;}));
    int64_t _literal_105 = 5;
    array_set(_enc__arg_ret, _literal_105, ((encore_arg_t) {.i = _binop_104}));
    _ite_92 = ((void*) UNIT);
  }
  else
  {
    /* ret(2) = desired_x */;
    int64_t _literal_106 = 2;
    array_set(_enc__arg_ret, _literal_106, ((encore_arg_t) {.i = _desired_x_75}));
    /* ret(3) = y1 */;
    int64_t _literal_107 = 3;
    array_set(_enc__arg_ret, _literal_107, ((encore_arg_t) {.i = _y1_5}));
    /* ret(4) = x1 */;
    int64_t _literal_108 = 4;
    array_set(_enc__arg_ret, _literal_108, ((encore_arg_t) {.i = _x1_2}));
    /* ret(5) = desired_y */;
    int64_t _literal_109 = 5;
    array_set(_enc__arg_ret, _literal_109, ((encore_arg_t) {.i = _desired_y_83}));
    _ite_92 = ((void*) UNIT);
  };
  /* () */;
  UNIT;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "next");
  return UNIT;
}


void* _enc__method__Ped_util_Agent_passive_Agent_init(pony_ctx_t** _ctx, _enc__class__Ped_util_Agent_passive_Agent_t* _this, pony_type_t** runtimeType, tuple_t* _enc__arg_in_pos, array_t* _enc__arg_list, array_t* _enc__arg_targets_size, int64_t _enc__arg_in_id, int64_t _enc__arg_ttl)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  /* match in_pos with
  case (x, y) =>
    this.x = x
    this.y = y
  end

end */;
  void* _match_0;
  int64_t _x_1;
  int64_t _y_2;
  if ((({int64_t _tupleCheck_3;
         _tupleCheck_3 = 1;
         int64_t _tupleAccess_4 = tuple_get(_enc__arg_in_pos, 0).i;
         int64_t _varBinding_5;
         _x_1 = _tupleAccess_4;
         _varBinding_5 = 1;
         _tupleCheck_3 = (_tupleCheck_3 && _varBinding_5);
         int64_t _tupleAccess_6 = tuple_get(_enc__arg_in_pos, 1).i;
         int64_t _varBinding_7;
         _y_2 = _tupleAccess_6;
         _varBinding_7 = 1;
         _tupleCheck_3 = (_tupleCheck_3 && _varBinding_7); _tupleCheck_3;}) && ({int64_t _literal_8 = 1/*True*/; _literal_8;})))
  {
    _match_0 = ((void*) ({/* this.x = x */;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "x"); _this;}))._enc__field_x = _x_1;
                          /* this.y = y */;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "y"); _this;}))._enc__field_y = _y_2; UNIT;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  /* this.i = 0 */;
  int64_t _literal_9 = 0;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "i"); _this;}))._enc__field_i = _literal_9;
  /* this.targets_x = new [int](|targets_size|) */;
  int64_t _size_11 = array_size(_enc__arg_targets_size);
  array_t* _array_10 = array_mk(_ctx, _size_11, ENCORE_PRIMITIVE);
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_x"); _this;}))._enc__field_targets_x = _array_10;
  /* this.targets_y = new [int](|targets_size|) */;
  int64_t _size_13 = array_size(_enc__arg_targets_size);
  array_t* _array_12 = array_mk(_ctx, _size_13, ENCORE_PRIMITIVE);
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_y"); _this;}))._enc__field_targets_y = _array_12;
  /* this.targets_size = targets_size */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_size"); _this;}))._enc__field_targets_size = _enc__arg_targets_size;
  /* for index <- [0..|list| - 1] do
  match list(index) with
    case (x, y) =>
      (this.targets_x)(index) = x
      (this.targets_y)(index) = y
    end
  
  end
end */;
  void* _for_14;
  /* Range not generated */;
  int64_t _literal_21 = 0;
  int64_t _binop_24 = (({int64_t _size_22 = array_size(_enc__arg_list); _size_22;}) - ({int64_t _literal_23 = 1; _literal_23;}));
  int64_t _literal_25 = 1;
  int64_t _literal_26 = 1;
  int64_t _step_19 = (_literal_26 * _literal_25);
  range_assert_step(_step_19);
  int64_t _index_15;
  if ((_step_19 > 0))
  {
    _index_15 = _literal_21;
  }
  else
  {
    _index_15 = _binop_24;
  };
  while (((_index_15 >= _literal_21) && (_index_15 <= _binop_24)))
  {
    int64_t _index_16 = _index_15;
    tuple_t* _access_28 = array_get(_enc__arg_list, _index_16).p;
    void* _match_27;
    int64_t _x_29;
    int64_t _y_30;
    if ((({int64_t _tupleCheck_33;
           _tupleCheck_33 = 1;
           int64_t _tupleAccess_34 = tuple_get(_access_28, 0).i;
           int64_t _varBinding_35;
           _x_29 = _tupleAccess_34;
           _varBinding_35 = 1;
           _tupleCheck_33 = (_tupleCheck_33 && _varBinding_35);
           int64_t _tupleAccess_36 = tuple_get(_access_28, 1).i;
           int64_t _varBinding_37;
           _y_30 = _tupleAccess_36;
           _varBinding_37 = 1;
           _tupleCheck_33 = (_tupleCheck_33 && _varBinding_37); _tupleCheck_33;}) && ({int64_t _literal_38 = 1/*True*/; _literal_38;})))
    {
      _match_27 = ((void*) ({/* (this.targets_x)(index) = x */;
                             ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_x");
                             array_t* _fieldacc_31 = (*_this)._enc__field_targets_x;
                             array_set(_fieldacc_31, _index_16, ((encore_arg_t) {.i = _x_29}));
                             /* (this.targets_y)(index) = y */;
                             ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "targets_y");
                             array_t* _fieldacc_32 = (*_this)._enc__field_targets_y;
                             array_set(_fieldacc_32, _index_16, ((encore_arg_t) {.i = _y_30})); UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
    _for_14 = _match_27;
    _index_15 = (_index_15 + _step_19);
  };
  /* this.hash_code = in_id */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "hash_code"); _this;}))._enc__field_hash_code = ((uint64_t) _enc__arg_in_id);
  /* this.ttl = ttl */;
  (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ttl"); _this;}))._enc__field_ttl = _enc__arg_ttl;
  /* () */;
  UNIT;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class__Ped_util_Agent_passive_Agent_type = {.id=_ENC__ID__Ped_util_Agent_passive_Agent, .size=sizeof(_enc__class__Ped_util_Agent_passive_Agent_t), .trace=_enc__trace__Ped_util_Agent_passive_Agent, .vtable=trait_method_selector};
