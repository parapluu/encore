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


void _enc__type_init__Ped_util_Quad_tree_Quad_tree(_enc__class__Ped_util_Quad_tree_Quad_tree_t* _this, ... )
{
  va_list params;
  va_start(params, _this);
  va_end(params);
}


void _enc__trace__Ped_util_Quad_tree_Quad_tree(pony_ctx_t* _ctx_arg, void* p)
{
  pony_ctx_t** _ctx = (&(_ctx_arg));
  _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this = p;
  int64_t _enc__field_ymin = _this->_enc__field_ymin;
  /* Not tracing field '_enc__field_ymin' */;
  int64_t _enc__field_xmin = _this->_enc__field_xmin;
  /* Not tracing field '_enc__field_xmin' */;
  int64_t _enc__field_ymax = _this->_enc__field_ymax;
  /* Not tracing field '_enc__field_ymax' */;
  int64_t _enc__field_xmax = _this->_enc__field_xmax;
  /* Not tracing field '_enc__field_xmax' */;
  _enc__class__Ped_util_Quad_tree_Quad_t* _enc__field_tree = _this->_enc__field_tree;
  encore_trace_object((*_ctx), _enc__field_tree, _enc__trace__Ped_util_Quad_tree_Quad);
}


_enc__class__Ped_util_Quad_tree_Quad_tree_t* _enc__constructor__Ped_util_Quad_tree_Quad_tree(pony_ctx_t** _ctx, pony_type_t** runtimeType)
{
  _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this = ((_enc__class__Ped_util_Quad_tree_Quad_tree_t*) encore_alloc((*_ctx), sizeof(_enc__class__Ped_util_Quad_tree_Quad_tree_t)));
  _this->_enc__self_type = (&(_enc__class__Ped_util_Quad_tree_Quad_tree_type));
  return _this;
}


int64_t _enc__method__Ped_util_Quad_tree_Quad_tree_size(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this, pony_type_t** runtimeType)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "tree");
  _enc__class__Ped_util_Quad_tree_Quad_t* _fieldacc_0 = (*_this)._enc__field_tree;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_fieldacc_0, "size");
  int64_t _fieldacc_1 = (*_fieldacc_0)._enc__field_size;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "size");
  return ((int64_t) _fieldacc_1);
}


int64_t _enc__method__Ped_util_Quad_tree_Quad_tree_remove(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "tree");
  _enc__class__Ped_util_Quad_tree_Quad_t* _fieldacc_1 = (*_this)._enc__field_tree;
  check_receiver(_fieldacc_1, ".", "this.tree", "remove", "\"./Ped_util/Quad_tree.enc\" (line 149, column 5)");
  pony_type_t* _tmp_2[] = {};
  int64_t _sync_method_call_0 = _enc__method__Ped_util_Quad_tree_Quad_remove(_ctx, _fieldacc_1, NULL, _enc__arg_x, _enc__arg_y);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "remove");
  return ((int64_t) _sync_method_call_0);
}


void* _enc__method__Ped_util_Quad_tree_Quad_tree_set(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "set");
  /* if x > this.xmax || y > this.ymax || y < this.ymin || x < this.xmin then
  print("TRYING TO STORE VALUE OUTSIDE OF THIS TREE\n")
else
  this.tree.add(x, y)
end */;
  void* _ite_0;
  if (({int64_t _binop_11 = (({int64_t _binop_8 = (({int64_t _binop_5 = (({int64_t _binop_2 = (({ _enc__arg_x;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax");
                                                                                                                     int64_t _fieldacc_1 = (*_this)._enc__field_xmax; _fieldacc_1;})); _binop_2;}) || ({int64_t _binop_4 = (({ _enc__arg_y;}) > ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax");
                                                                                                                                                                                                                                                  int64_t _fieldacc_3 = (*_this)._enc__field_ymax; _fieldacc_3;})); _binop_4;})); _binop_5;}) || ({int64_t _binop_7 = (({ _enc__arg_y;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin");
                                                                                                                                                                                                                                                                                                                                                                                             int64_t _fieldacc_6 = (*_this)._enc__field_ymin; _fieldacc_6;})); _binop_7;})); _binop_8;}) || ({int64_t _binop_10 = (({ _enc__arg_x;}) < ({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin");
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         int64_t _fieldacc_9 = (*_this)._enc__field_xmin; _fieldacc_9;})); _binop_10;})); _binop_11;}))
  {
    char* _literal_12 = "TRYING TO STORE VALUE OUTSIDE OF THIS TREE\n";
    fprintf(stdout, "%s", _literal_12);
    _ite_0 = ((void*) UNIT);
  }
  else
  {
    ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "tree");
    _enc__class__Ped_util_Quad_tree_Quad_t* _fieldacc_14 = (*_this)._enc__field_tree;
    check_receiver(_fieldacc_14, ".", "this.tree", "add", "\"./Ped_util/Quad_tree.enc\" (line 144, column 7)");
    pony_type_t* _tmp_15[] = {};
    void* _sync_method_call_13 = _enc__method__Ped_util_Quad_tree_Quad_add(_ctx, _fieldacc_14, NULL, _enc__arg_x, _enc__arg_y);
    _ite_0 = ((void*) _sync_method_call_13);
  };
  /* () */;
  UNIT;
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "set");
  return UNIT;
}


int64_t _enc__method__Ped_util_Quad_tree_Quad_tree_get_val(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this, pony_type_t** runtimeType, int64_t _enc__arg_x, int64_t _enc__arg_y)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "get_val");
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_this, "tree");
  _enc__class__Ped_util_Quad_tree_Quad_t* _fieldacc_1 = (*_this)._enc__field_tree;
  check_receiver(_fieldacc_1, ".", "this.tree", "isin", "\"./Ped_util/Quad_tree.enc\" (line 138, column 5)");
  pony_type_t* _tmp_2[] = {};
  int64_t _sync_method_call_0 = _enc__method__Ped_util_Quad_tree_Quad_isin(_ctx, _fieldacc_1, NULL, _enc__arg_x, _enc__arg_y);
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "get_val");
  return ((int64_t) _sync_method_call_0);
}


void* _enc__method__Ped_util_Quad_tree_Quad_tree_init(pony_ctx_t** _ctx, _enc__class__Ped_util_Quad_tree_Quad_tree_t* _this, pony_type_t** runtimeType, tuple_t* _enc__arg_in_max, tuple_t* _enc__arg_in_min)
{
  ENC_DTRACE3(METHOD_ENTRY, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  tuple_t* _tuple_1 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_1, 0, (&(tuple_type)));
  tuple_set_type(_tuple_1, 1, (&(tuple_type)));
  tuple_set(_tuple_1, 0, ((encore_arg_t) {.p = _enc__arg_in_max}));
  tuple_set(_tuple_1, 1, ((encore_arg_t) {.p = _enc__arg_in_min}));
  void* _match_0;
  int64_t _xmax_2;
  int64_t _ymax_3;
  int64_t _xmin_4;
  int64_t _ymin_5;
  if ((({int64_t _tupleCheck_8;
         _tupleCheck_8 = 1;
         tuple_t* _tupleAccess_9 = tuple_get(_tuple_1, 0).p;
         int64_t _tupleCheck_10;
         _tupleCheck_10 = 1;
         int64_t _tupleAccess_11 = tuple_get(_tupleAccess_9, 0).i;
         int64_t _varBinding_12;
         _xmax_2 = _tupleAccess_11;
         _varBinding_12 = 1;
         _tupleCheck_10 = (_tupleCheck_10 && _varBinding_12);
         int64_t _tupleAccess_13 = tuple_get(_tupleAccess_9, 1).i;
         int64_t _varBinding_14;
         _ymax_3 = _tupleAccess_13;
         _varBinding_14 = 1;
         _tupleCheck_10 = (_tupleCheck_10 && _varBinding_14);
         _tupleCheck_8 = (_tupleCheck_8 && _tupleCheck_10);
         tuple_t* _tupleAccess_15 = tuple_get(_tuple_1, 1).p;
         int64_t _tupleCheck_16;
         _tupleCheck_16 = 1;
         int64_t _tupleAccess_17 = tuple_get(_tupleAccess_15, 0).i;
         int64_t _varBinding_18;
         _xmin_4 = _tupleAccess_17;
         _varBinding_18 = 1;
         _tupleCheck_16 = (_tupleCheck_16 && _varBinding_18);
         int64_t _tupleAccess_19 = tuple_get(_tupleAccess_15, 1).i;
         int64_t _varBinding_20;
         _ymin_5 = _tupleAccess_19;
         _varBinding_20 = 1;
         _tupleCheck_16 = (_tupleCheck_16 && _varBinding_20);
         _tupleCheck_8 = (_tupleCheck_8 && _tupleCheck_16); _tupleCheck_8;}) && ({int64_t _literal_21 = 1/*True*/; _literal_21;})))
  {
    _match_0 = ((void*) ({/* this.tree = new Quad(xmax, ymax, xmin, ymin) */;
                          _enc__class__Ped_util_Quad_tree_Quad_t* _new_6 = _enc__constructor__Ped_util_Quad_tree_Quad(_ctx, NULL);
                          pony_type_t* _tmp_7[] = {};
                          _enc__type_init__Ped_util_Quad_tree_Quad(_new_6);
                          _enc__method__Ped_util_Quad_tree_Quad_init(_ctx, _new_6, NULL, _xmax_2, _ymax_3, _xmin_4, _ymin_5);
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "tree"); _this;}))._enc__field_tree = _new_6;
                          /* this.xmax = xmax */;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "xmax"); _this;}))._enc__field_xmax = _xmax_2;
                          /* this.ymax = ymax */;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ymax"); _this;}))._enc__field_ymax = _ymax_3;
                          /* this.xmin = xmin */;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "xmin"); _this;}))._enc__field_xmin = _xmin_4;
                          /* this.ymin = ymin */;
                          (*({ENC_DTRACE3(FIELD_WRITE, (uintptr_t)*_ctx, (uintptr_t)_this, "ymin"); _this;}))._enc__field_ymin = _ymin_5; UNIT;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  ENC_DTRACE3(METHOD_EXIT, (uintptr_t)*_ctx, (uintptr_t)_this, "init");
  return UNIT;
}


pony_type_t _enc__class__Ped_util_Quad_tree_Quad_tree_type = {.id=_ENC__ID__Ped_util_Quad_tree_Quad_tree, .size=sizeof(_enc__class__Ped_util_Quad_tree_Quad_tree_t), .trace=_enc__trace__Ped_util_Quad_tree_Quad_tree, .vtable=trait_method_selector};
