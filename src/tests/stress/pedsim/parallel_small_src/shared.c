#include "header.h"


//////////////////////////////////////////////
// Embedded Code from "./parallel_small.enc"



























// This function is called in the very beginning of the program to
// build an array containing the arguments of the program.
array_t *_init_argv(pony_ctx_t** ctx, size_t argc, char **argv) {
 array_t *arr = array_mk(ctx, argc, &_enc__class_String_String_type);
 for(int i = 0; i < argc; i++) {
   _enc__class_String_String_t* s =
     encore_alloc(*ctx, sizeof(_enc__class_String_String_t));
   s->_enc__self_type = &_enc__class_String_String_type;
   _enc__method_String_String_init(ctx, s, NULL, argv[i]);
   array_set(arr, i, (encore_arg_t){.p = s});
 }
 return arr;
}


/////////////////////
// Global functions


array_t* _enc__global_fun__Ped_util_Global_funsparse_file(pony_ctx_t** _ctx, pony_type_t** runtimeType, _enc__class_String_String_t* _enc__arg_fname)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "parse_file");
  /* var sen = new XML_lib() */;
  /* sen = new XML_lib() */;
  _enc__class__Ped_util_XML_XML_lib_t* _new_0 = _enc__constructor__Ped_util_XML_XML_lib(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init__Ped_util_XML_XML_lib(_new_0);
  _enc__method__Ped_util_XML_XML_lib_init(_ctx, _new_0, NULL);
  _enc__class__Ped_util_XML_XML_lib_t* _sen_3 = _new_0;
  /* var file = new XML_node() */;
  /* file = new XML_node() */;
  _enc__class__Ped_util_XML_XML_node_t* _new_4 = _enc__constructor__Ped_util_XML_XML_node(_ctx, NULL);
  pony_type_t* _tmp_5[] = {};
  _enc__type_init__Ped_util_XML_XML_node(_new_4);
  _enc__method__Ped_util_XML_XML_node_init(_ctx, _new_4, NULL);
  _enc__class__Ped_util_XML_XML_node_t* _file_7 = _new_4;
  /* do
  match sen.file_to_xml(fname) with
    case Just(xml) =>
      file = xml
    end
    case Nothing =>
      print("\n\nBADXML!\n")
    end
  
  end
  var xml_way = file.children_named("waypoint")
  var waypoints = new [(int, int)](|xml_way|)
  var waypoints_distance = new [int](|xml_way|)
  var i = 0
  do
    for a <- xml_way do
      waypoints(i) = match (a.attribute_value("x").to_int(), a.attribute_value("y").to_int()) with
                       case (Just(x), Just(y)) =>
                         (x, y)
                       end
                       case _ =>
                         print("BADINT!\n")
                         abort("parsing error")
                         (0, 0)
                       end
                     
                     end
      waypoints_distance(i) = yolo_int(a.attribute_value("r").to_int())
      i = i + 1
    end
    var nr_agents = 0
    for a <- file.children_named("agent") do
      nr_agents = nr_agents + match a.attribute_value("n").to_int() with
                                case Just(nr) =>
                                  nr
                                end
                                case _ =>
                                  print("BADINT2!\n")
                                  abort("parsing error")
                                  0
                                end
                              
                              end
    end
    var agents = new [Agent](nr_agents)
    var i = 0
    do
      for a <- file.children_named("agent") do
        var max = yolo_int(a.attribute_value("n").to_int())
        var x = yolo_int(a.attribute_value("xs").to_int())
        var y = yolo_int(a.attribute_value("ys").to_int())
        var dx_max = yolo_int(a.attribute_value("dx").to_int()) / 2
        var dy_max = yolo_int(a.attribute_value("dy").to_int()) / 2
        var agent_waypoints = new [(int, int)](|a.children_named("addway")|)
        var dx = x - dx_max
        var dy = y - dy_max
        var j = 0
        do
          for b <- a.children_named("addway") do
            agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1)
            unless |agent_waypoints| > 1 then
              print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
              abort("parsing error")
            end
            j = j + 1
          end
          for unused <- [0..max - 1] do
            if dx > x + dx_max then
              print("out of space\n")
              abort("parsing error")
            else
              if dy > y + dy_max then
                dy = y - dy_max
                dx = dx + 1
              else
                dy = dy + 1
              end
            end
            agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000)
            i = i + 1
          end
        end
      end
      agents
    end
  end
end */;
  /* match sen.file_to_xml(fname) with
  case Just(xml) =>
    file = xml
  end
  case Nothing =>
    print("\n\nBADXML!\n")
  end

end */;
  check_receiver(_sen_3, ".", "sen", "file_to_xml", "\"./Ped_util/Global_funs.enc\" (line 144, column 11)");
  pony_type_t* _tmp_10[] = {};
  option_t* _sync_method_call_9 = _enc__method__Ped_util_XML_XML_lib_file_to_xml(_ctx, _sen_3, NULL, _enc__arg_fname);
  void* _match_8;
  _enc__class__Ped_util_XML_XML_node_t* _xml_11;
  if ((({int64_t _optionCheck_17;
         _optionCheck_17 = ((JUST == (*_sync_method_call_9).tag) && ({int64_t _varBinding_18;
                                                                      _enc__class__Ped_util_XML_XML_node_t* _optionVal_16 = (*_sync_method_call_9).val.p;
                                                                      _xml_11 = _optionVal_16;
                                                                      _varBinding_18 = 1; _varBinding_18;})); _optionCheck_17;}) && ({int64_t _literal_19 = 1/*True*/; _literal_19;})))
  {
    _match_8 = ((void*) ({_file_7 = _xml_11; UNIT;}));
  }
  else
  {
    if ((({int64_t _valueCheck_13;
           _valueCheck_13 = (({option_t* _option_14 = (&(DEFAULT_NOTHING)); _option_14;}) == _sync_method_call_9); _valueCheck_13;}) && ({int64_t _literal_15 = 1/*True*/; _literal_15;})))
    {
      _match_8 = ((void*) ({char* _literal_12 = "\n\nBADXML!\n";
                            fprintf(stdout, "%s", _literal_12); UNIT;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  /* var xml_way = file.children_named("waypoint") */;
  /* xml_way = file.children_named("waypoint") */;
  check_receiver(_file_7, ".", "file", "children_named", "\"./Ped_util/Global_funs.enc\" (line 153, column 19)");
  _enc__class_String_String_t* _new_21 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_22 = ({"waypoint";});
  pony_type_t* _tmp_23[] = {};
  _enc__type_init_String_String(_new_21);
  _enc__method_String_String_init(_ctx, _new_21, NULL, _embed_22);
  pony_type_t* _tmp_24[] = {};
  array_t* _sync_method_call_20 = _enc__method__Ped_util_XML_XML_node_children_named(_ctx, _file_7, NULL, _new_21);
  array_t* _xml_way_26 = _sync_method_call_20;
  /* var waypoints = new [(int, int)](|xml_way|) */;
  /* waypoints = new [(int, int)](|xml_way|) */;
  int64_t _size_28 = array_size(_xml_way_26);
  array_t* _array_27 = array_mk(_ctx, _size_28, (&(tuple_type)));
  array_t* _waypoints_30 = _array_27;
  /* var waypoints_distance = new [int](|xml_way|) */;
  /* waypoints_distance = new [int](|xml_way|) */;
  int64_t _size_32 = array_size(_xml_way_26);
  array_t* _array_31 = array_mk(_ctx, _size_32, ENCORE_PRIMITIVE);
  array_t* _waypoints_distance_34 = _array_31;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_35 = 0;
  int64_t _i_37 = _literal_35;
  /* do
  for a <- xml_way do
    waypoints(i) = match (a.attribute_value("x").to_int(), a.attribute_value("y").to_int()) with
                     case (Just(x), Just(y)) =>
                       (x, y)
                     end
                     case _ =>
                       print("BADINT!\n")
                       abort("parsing error")
                       (0, 0)
                     end
                   
                   end
    waypoints_distance(i) = yolo_int(a.attribute_value("r").to_int())
    i = i + 1
  end
  var nr_agents = 0
  for a <- file.children_named("agent") do
    nr_agents = nr_agents + match a.attribute_value("n").to_int() with
                              case Just(nr) =>
                                nr
                              end
                              case _ =>
                                print("BADINT2!\n")
                                abort("parsing error")
                                0
                              end
                            
                            end
  end
  var agents = new [Agent](nr_agents)
  var i = 0
  do
    for a <- file.children_named("agent") do
      var max = yolo_int(a.attribute_value("n").to_int())
      var x = yolo_int(a.attribute_value("xs").to_int())
      var y = yolo_int(a.attribute_value("ys").to_int())
      var dx_max = yolo_int(a.attribute_value("dx").to_int()) / 2
      var dy_max = yolo_int(a.attribute_value("dy").to_int()) / 2
      var agent_waypoints = new [(int, int)](|a.children_named("addway")|)
      var dx = x - dx_max
      var dy = y - dy_max
      var j = 0
      do
        for b <- a.children_named("addway") do
          agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1)
          unless |agent_waypoints| > 1 then
            print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
            abort("parsing error")
          end
          j = j + 1
        end
        for unused <- [0..max - 1] do
          if dx > x + dx_max then
            print("out of space\n")
            abort("parsing error")
          else
            if dy > y + dy_max then
              dy = y - dy_max
              dx = dx + 1
            else
              dy = dy + 1
            end
          end
          agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000)
          i = i + 1
        end
      end
    end
    agents
  end
end */;
  /* for a <- xml_way do
  waypoints(i) = match (a.attribute_value("x").to_int(), a.attribute_value("y").to_int()) with
                   case (Just(x), Just(y)) =>
                     (x, y)
                   end
                   case _ =>
                     print("BADINT!\n")
                     abort("parsing error")
                     (0, 0)
                   end
                 
                 end
  waypoints_distance(i) = yolo_int(a.attribute_value("r").to_int())
  i = i + 1
end */;
  void* _for_38;
  int64_t _start_41 = 0;
  int64_t _stop_42 = (array_size(_xml_way_26) - 1);
  int64_t _src_step_44 = 1;
  int64_t _literal_45 = 1;
  int64_t _step_43 = (_literal_45 * _src_step_44);
  range_assert_step(_step_43);
  int64_t _index_39;
  if ((_step_43 > 0))
  {
    _index_39 = _start_41;
  }
  else
  {
    _index_39 = _stop_42;
  };
  while (((_index_39 >= _start_41) && (_index_39 <= _stop_42)))
  {
    _enc__class__Ped_util_XML_XML_node_t* _a_40 = array_get(_xml_way_26, _index_39).p;
    /* waypoints(i) = match (a.attribute_value("x").to_int(), a.attribute_value("y").to_int()) with
                 case (Just(x), Just(y)) =>
                   (x, y)
                 end
                 case _ =>
                   print("BADINT!\n")
                   abort("parsing error")
                   (0, 0)
                 end
               
               end */;
    tuple_t* _tuple_47 = tuple_mk(_ctx, 2);
    tuple_set_type(_tuple_47, 0, (&(option_type)));
    tuple_set_type(_tuple_47, 1, (&(option_type)));
    check_receiver(_a_40, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 159, column 31)");
    _enc__class_String_String_t* _new_50 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_51 = ({"x";});
    pony_type_t* _tmp_52[] = {};
    _enc__type_init_String_String(_new_50);
    _enc__method_String_String_init(_ctx, _new_50, NULL, _embed_51);
    pony_type_t* _tmp_53[] = {};
    _enc__class_String_String_t* _sync_method_call_49 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_40, NULL, _new_50);
    check_receiver(_sync_method_call_49, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"x\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 159, column 31)");
    pony_type_t* _tmp_54[] = {};
    option_t* _sync_method_call_48 = _enc__method_String_String_to_int(_ctx, _sync_method_call_49, NULL);
    check_receiver(_a_40, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 159, column 64)");
    _enc__class_String_String_t* _new_57 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_58 = ({"y";});
    pony_type_t* _tmp_59[] = {};
    _enc__type_init_String_String(_new_57);
    _enc__method_String_String_init(_ctx, _new_57, NULL, _embed_58);
    pony_type_t* _tmp_60[] = {};
    _enc__class_String_String_t* _sync_method_call_56 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_40, NULL, _new_57);
    check_receiver(_sync_method_call_56, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"y\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 159, column 64)");
    pony_type_t* _tmp_61[] = {};
    option_t* _sync_method_call_55 = _enc__method_String_String_to_int(_ctx, _sync_method_call_56, NULL);
    tuple_set(_tuple_47, 0, ((encore_arg_t) {.p = _sync_method_call_48}));
    tuple_set(_tuple_47, 1, ((encore_arg_t) {.p = _sync_method_call_55}));
    tuple_t* _match_46;
    int64_t _x_62;
    int64_t _y_63;
    if ((({int64_t _tupleCheck_73;
           _tupleCheck_73 = 1;
           option_t* _tupleAccess_74 = tuple_get(_tuple_47, 0).p;
           int64_t _optionCheck_76;
           _optionCheck_76 = ((JUST == (*_tupleAccess_74).tag) && ({int64_t _varBinding_77;
                                                                    int64_t _optionVal_75 = (*_tupleAccess_74).val.i;
                                                                    _x_62 = _optionVal_75;
                                                                    _varBinding_77 = 1; _varBinding_77;}));
           _tupleCheck_73 = (_tupleCheck_73 && _optionCheck_76);
           option_t* _tupleAccess_78 = tuple_get(_tuple_47, 1).p;
           int64_t _optionCheck_80;
           _optionCheck_80 = ((JUST == (*_tupleAccess_78).tag) && ({int64_t _varBinding_81;
                                                                    int64_t _optionVal_79 = (*_tupleAccess_78).val.i;
                                                                    _y_63 = _optionVal_79;
                                                                    _varBinding_81 = 1; _varBinding_81;}));
           _tupleCheck_73 = (_tupleCheck_73 && _optionCheck_80); _tupleCheck_73;}) && ({int64_t _literal_82 = 1/*True*/; _literal_82;})))
    {
      _match_46 = ((tuple_t*) ({tuple_t* _tuple_64 = tuple_mk(_ctx, 2);
                                tuple_set_type(_tuple_64, 0, ENCORE_PRIMITIVE);
                                tuple_set_type(_tuple_64, 1, ENCORE_PRIMITIVE);
                                tuple_set(_tuple_64, 0, ((encore_arg_t) {.i = _x_62}));
                                tuple_set(_tuple_64, 1, ((encore_arg_t) {.i = _y_63})); _tuple_64;}));
    }
    else
    {
      tuple_t* ___65;
      if ((({int64_t _varBinding_71;
             ___65 = _tuple_47;
             _varBinding_71 = 1; _varBinding_71;}) && ({int64_t _literal_72 = 1/*True*/; _literal_72;})))
      {
        _match_46 = ((tuple_t*) ({/* print("BADINT!\n") */;
                                  char* _literal_66 = "BADINT!\n";
                                  fprintf(stdout, "%s", _literal_66);
                                  /* abort("parsing error") */;
                                  /* abort("parsing error") */;
                                  char* _literal_67 = "parsing error";
                                  fprintf(stderr, "%s\n", _literal_67);
                                  /* abort("parsing error") */;
                                  fprintf(stderr, "\"./Ped_util/Global_funs.enc\" (line 165, column 28)\n");
                                  /* abort("parsing error") */;
                                  abort();
                                  /* (0, 0) */;
                                  tuple_t* _tuple_68 = tuple_mk(_ctx, 2);
                                  tuple_set_type(_tuple_68, 0, ENCORE_PRIMITIVE);
                                  tuple_set_type(_tuple_68, 1, ENCORE_PRIMITIVE);
                                  int64_t _literal_69 = 0;
                                  int64_t _literal_70 = 0;
                                  tuple_set(_tuple_68, 0, ((encore_arg_t) {.i = _literal_69}));
                                  tuple_set(_tuple_68, 1, ((encore_arg_t) {.i = _literal_70})); _tuple_68;}));
      }
      else
      {
        fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
        exit(1);
      };
    };
    array_set(_waypoints_30, _i_37, ((encore_arg_t) {.p = _match_46}));
    /* waypoints_distance(i) = yolo_int(a.attribute_value("r").to_int()) */;
    check_receiver(_a_40, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 170, column 42)");
    _enc__class_String_String_t* _new_85 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_86 = ({"r";});
    pony_type_t* _tmp_87[] = {};
    _enc__type_init_String_String(_new_85);
    _enc__method_String_String_init(_ctx, _new_85, NULL, _embed_86);
    pony_type_t* _tmp_88[] = {};
    _enc__class_String_String_t* _sync_method_call_84 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_40, NULL, _new_85);
    check_receiver(_sync_method_call_84, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"r\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 170, column 42)");
    pony_type_t* _tmp_89[] = {};
    option_t* _sync_method_call_83 = _enc__method_String_String_to_int(_ctx, _sync_method_call_84, NULL);
    ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
    pony_type_t* _tmp_90[] = {};
    int64_t _fun_call_91 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_83);
    array_set(_waypoints_distance_34, _i_37, ((encore_arg_t) {.i = _fun_call_91}));
    /* i = i + 1 */;
    int64_t _binop_93 = (({ _i_37;}) + ({int64_t _literal_92 = 1; _literal_92;}));
    _i_37 = _binop_93;
    _for_38 = UNIT;
    _index_39 = (_index_39 + _step_43);
  };
  /* var nr_agents = 0 */;
  /* nr_agents = 0 */;
  int64_t _literal_94 = 0;
  int64_t _nr_agents_96 = _literal_94;
  /* for a <- file.children_named("agent") do
  nr_agents = nr_agents + match a.attribute_value("n").to_int() with
                            case Just(nr) =>
                              nr
                            end
                            case _ =>
                              print("BADINT2!\n")
                              abort("parsing error")
                              0
                            end
                          
                          end
end */;
  void* _for_97;
  check_receiver(_file_7, ".", "file", "children_named", "\"./Ped_util/Global_funs.enc\" (line 174, column 16)");
  _enc__class_String_String_t* _new_105 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_106 = ({"agent";});
  pony_type_t* _tmp_107[] = {};
  _enc__type_init_String_String(_new_105);
  _enc__method_String_String_init(_ctx, _new_105, NULL, _embed_106);
  pony_type_t* _tmp_108[] = {};
  array_t* _sync_method_call_104 = _enc__method__Ped_util_XML_XML_node_children_named(_ctx, _file_7, NULL, _new_105);
  int64_t _start_100 = 0;
  int64_t _stop_101 = (array_size(_sync_method_call_104) - 1);
  int64_t _src_step_103 = 1;
  int64_t _literal_109 = 1;
  int64_t _step_102 = (_literal_109 * _src_step_103);
  range_assert_step(_step_102);
  int64_t _index_98;
  if ((_step_102 > 0))
  {
    _index_98 = _start_100;
  }
  else
  {
    _index_98 = _stop_101;
  };
  while (((_index_98 >= _start_100) && (_index_98 <= _stop_101)))
  {
    _enc__class__Ped_util_XML_XML_node_t* _a_99 = array_get(_sync_method_call_104, _index_98).p;
    int64_t _binop_129 = (({ _nr_agents_96;}) + ({check_receiver(_a_99, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 175, column 39)");
                                                  _enc__class_String_String_t* _new_113 = _enc__constructor_String_String(_ctx, NULL);
                                                  char* _embed_114 = ({"n";});
                                                  pony_type_t* _tmp_115[] = {};
                                                  _enc__type_init_String_String(_new_113);
                                                  _enc__method_String_String_init(_ctx, _new_113, NULL, _embed_114);
                                                  pony_type_t* _tmp_116[] = {};
                                                  _enc__class_String_String_t* _sync_method_call_112 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_99, NULL, _new_113);
                                                  check_receiver(_sync_method_call_112, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"n\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 175, column 39)");
                                                  pony_type_t* _tmp_117[] = {};
                                                  option_t* _sync_method_call_111 = _enc__method_String_String_to_int(_ctx, _sync_method_call_112, NULL);
                                                  int64_t _match_110;
                                                  int64_t _nr_118;
                                                  if ((({int64_t _optionCheck_126;
                                                         _optionCheck_126 = ((JUST == (*_sync_method_call_111).tag) && ({int64_t _varBinding_127;
                                                                                                                         int64_t _optionVal_125 = (*_sync_method_call_111).val.i;
                                                                                                                         _nr_118 = _optionVal_125;
                                                                                                                         _varBinding_127 = 1; _varBinding_127;})); _optionCheck_126;}) && ({int64_t _literal_128 = 1/*True*/; _literal_128;})))
                                                  {
                                                    _match_110 = ((int64_t) ({ _nr_118;}));
                                                  }
                                                  else
                                                  {
                                                    option_t* ___119;
                                                    if ((({int64_t _varBinding_123;
                                                           ___119 = _sync_method_call_111;
                                                           _varBinding_123 = 1; _varBinding_123;}) && ({int64_t _literal_124 = 1/*True*/; _literal_124;})))
                                                    {
                                                      _match_110 = ((int64_t) ({/* print("BADINT2!\n") */;
                                                                                char* _literal_120 = "BADINT2!\n";
                                                                                fprintf(stdout, "%s", _literal_120);
                                                                                /* abort("parsing error") */;
                                                                                /* abort("parsing error") */;
                                                                                char* _literal_121 = "parsing error";
                                                                                fprintf(stderr, "%s\n", _literal_121);
                                                                                /* abort("parsing error") */;
                                                                                fprintf(stderr, "\"./Ped_util/Global_funs.enc\" (line 181, column 37)\n");
                                                                                /* abort("parsing error") */;
                                                                                abort();
                                                                                /* 0 */;
                                                                                int64_t _literal_122 = 0; _literal_122;}));
                                                    }
                                                    else
                                                    {
                                                      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
                                                      exit(1);
                                                    };
                                                  }; _match_110;}));
    _nr_agents_96 = _binop_129;
    _for_97 = UNIT;
    _index_98 = (_index_98 + _step_102);
  };
  /* var agents = new [Agent](nr_agents) */;
  /* agents = new [Agent](nr_agents) */;
  array_t* _array_130 = array_mk(_ctx, _nr_agents_96, (&(_enc__class__Ped_util_Agent_passive_Agent_type)));
  array_t* _agents_132 = _array_130;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_133 = 0;
  int64_t _i_135 = _literal_133;
  /* do
  for a <- file.children_named("agent") do
    var max = yolo_int(a.attribute_value("n").to_int())
    var x = yolo_int(a.attribute_value("xs").to_int())
    var y = yolo_int(a.attribute_value("ys").to_int())
    var dx_max = yolo_int(a.attribute_value("dx").to_int()) / 2
    var dy_max = yolo_int(a.attribute_value("dy").to_int()) / 2
    var agent_waypoints = new [(int, int)](|a.children_named("addway")|)
    var dx = x - dx_max
    var dy = y - dy_max
    var j = 0
    do
      for b <- a.children_named("addway") do
        agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1)
        unless |agent_waypoints| > 1 then
          print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
          abort("parsing error")
        end
        j = j + 1
      end
      for unused <- [0..max - 1] do
        if dx > x + dx_max then
          print("out of space\n")
          abort("parsing error")
        else
          if dy > y + dy_max then
            dy = y - dy_max
            dx = dx + 1
          else
            dy = dy + 1
          end
        end
        agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000)
        i = i + 1
      end
    end
  end
  agents
end */;
  /* for a <- file.children_named("agent") do
  var max = yolo_int(a.attribute_value("n").to_int())
  var x = yolo_int(a.attribute_value("xs").to_int())
  var y = yolo_int(a.attribute_value("ys").to_int())
  var dx_max = yolo_int(a.attribute_value("dx").to_int()) / 2
  var dy_max = yolo_int(a.attribute_value("dy").to_int()) / 2
  var agent_waypoints = new [(int, int)](|a.children_named("addway")|)
  var dx = x - dx_max
  var dy = y - dy_max
  var j = 0
  do
    for b <- a.children_named("addway") do
      agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1)
      unless |agent_waypoints| > 1 then
        print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
        abort("parsing error")
      end
      j = j + 1
    end
    for unused <- [0..max - 1] do
      if dx > x + dx_max then
        print("out of space\n")
        abort("parsing error")
      else
        if dy > y + dy_max then
          dy = y - dy_max
          dx = dx + 1
        else
          dy = dy + 1
        end
      end
      agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000)
      i = i + 1
    end
  end
end */;
  void* _for_136;
  check_receiver(_file_7, ".", "file", "children_named", "\"./Ped_util/Global_funs.enc\" (line 190, column 18)");
  _enc__class_String_String_t* _new_144 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_145 = ({"agent";});
  pony_type_t* _tmp_146[] = {};
  _enc__type_init_String_String(_new_144);
  _enc__method_String_String_init(_ctx, _new_144, NULL, _embed_145);
  pony_type_t* _tmp_147[] = {};
  array_t* _sync_method_call_143 = _enc__method__Ped_util_XML_XML_node_children_named(_ctx, _file_7, NULL, _new_144);
  int64_t _start_139 = 0;
  int64_t _stop_140 = (array_size(_sync_method_call_143) - 1);
  int64_t _src_step_142 = 1;
  int64_t _literal_148 = 1;
  int64_t _step_141 = (_literal_148 * _src_step_142);
  range_assert_step(_step_141);
  int64_t _index_137;
  if ((_step_141 > 0))
  {
    _index_137 = _start_139;
  }
  else
  {
    _index_137 = _stop_140;
  };
  while (((_index_137 >= _start_139) && (_index_137 <= _stop_140)))
  {
    _enc__class__Ped_util_XML_XML_node_t* _a_138 = array_get(_sync_method_call_143, _index_137).p;
    /* var max = yolo_int(a.attribute_value("n").to_int()) */;
    /* max = yolo_int(a.attribute_value("n").to_int()) */;
    check_receiver(_a_138, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 191, column 30)");
    _enc__class_String_String_t* _new_151 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_152 = ({"n";});
    pony_type_t* _tmp_153[] = {};
    _enc__type_init_String_String(_new_151);
    _enc__method_String_String_init(_ctx, _new_151, NULL, _embed_152);
    pony_type_t* _tmp_154[] = {};
    _enc__class_String_String_t* _sync_method_call_150 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_138, NULL, _new_151);
    check_receiver(_sync_method_call_150, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"n\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 191, column 30)");
    pony_type_t* _tmp_155[] = {};
    option_t* _sync_method_call_149 = _enc__method_String_String_to_int(_ctx, _sync_method_call_150, NULL);
    ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
    pony_type_t* _tmp_156[] = {};
    int64_t _fun_call_157 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_149);
    int64_t _max_159 = _fun_call_157;
    /* var x = yolo_int(a.attribute_value("xs").to_int()) */;
    /* x = yolo_int(a.attribute_value("xs").to_int()) */;
    check_receiver(_a_138, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 192, column 28)");
    _enc__class_String_String_t* _new_162 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_163 = ({"xs";});
    pony_type_t* _tmp_164[] = {};
    _enc__type_init_String_String(_new_162);
    _enc__method_String_String_init(_ctx, _new_162, NULL, _embed_163);
    pony_type_t* _tmp_165[] = {};
    _enc__class_String_String_t* _sync_method_call_161 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_138, NULL, _new_162);
    check_receiver(_sync_method_call_161, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"xs\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 192, column 28)");
    pony_type_t* _tmp_166[] = {};
    option_t* _sync_method_call_160 = _enc__method_String_String_to_int(_ctx, _sync_method_call_161, NULL);
    ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
    pony_type_t* _tmp_167[] = {};
    int64_t _fun_call_168 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_160);
    int64_t _x_170 = _fun_call_168;
    /* var y = yolo_int(a.attribute_value("ys").to_int()) */;
    /* y = yolo_int(a.attribute_value("ys").to_int()) */;
    check_receiver(_a_138, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 193, column 28)");
    _enc__class_String_String_t* _new_173 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_174 = ({"ys";});
    pony_type_t* _tmp_175[] = {};
    _enc__type_init_String_String(_new_173);
    _enc__method_String_String_init(_ctx, _new_173, NULL, _embed_174);
    pony_type_t* _tmp_176[] = {};
    _enc__class_String_String_t* _sync_method_call_172 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_138, NULL, _new_173);
    check_receiver(_sync_method_call_172, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"ys\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 193, column 28)");
    pony_type_t* _tmp_177[] = {};
    option_t* _sync_method_call_171 = _enc__method_String_String_to_int(_ctx, _sync_method_call_172, NULL);
    ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
    pony_type_t* _tmp_178[] = {};
    int64_t _fun_call_179 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_171);
    int64_t _y_181 = _fun_call_179;
    /* var dx_max = yolo_int(a.attribute_value("dx").to_int()) / 2 */;
    /* dx_max = yolo_int(a.attribute_value("dx").to_int()) / 2 */;
    int64_t _binop_192 = (({check_receiver(_a_138, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 194, column 33)");
                            _enc__class_String_String_t* _new_184 = _enc__constructor_String_String(_ctx, NULL);
                            char* _embed_185 = ({"dx";});
                            pony_type_t* _tmp_186[] = {};
                            _enc__type_init_String_String(_new_184);
                            _enc__method_String_String_init(_ctx, _new_184, NULL, _embed_185);
                            pony_type_t* _tmp_187[] = {};
                            _enc__class_String_String_t* _sync_method_call_183 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_138, NULL, _new_184);
                            check_receiver(_sync_method_call_183, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"dx\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 194, column 33)");
                            pony_type_t* _tmp_188[] = {};
                            option_t* _sync_method_call_182 = _enc__method_String_String_to_int(_ctx, _sync_method_call_183, NULL);
                            ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
                            pony_type_t* _tmp_189[] = {};
                            int64_t _fun_call_190 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_182); _fun_call_190;}) / ({int64_t _literal_191 = 2; _literal_191;}));
    int64_t _dx_max_194 = _binop_192;
    /* var dy_max = yolo_int(a.attribute_value("dy").to_int()) / 2 */;
    /* dy_max = yolo_int(a.attribute_value("dy").to_int()) / 2 */;
    int64_t _binop_205 = (({check_receiver(_a_138, ".", "a", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 195, column 33)");
                            _enc__class_String_String_t* _new_197 = _enc__constructor_String_String(_ctx, NULL);
                            char* _embed_198 = ({"dy";});
                            pony_type_t* _tmp_199[] = {};
                            _enc__type_init_String_String(_new_197);
                            _enc__method_String_String_init(_ctx, _new_197, NULL, _embed_198);
                            pony_type_t* _tmp_200[] = {};
                            _enc__class_String_String_t* _sync_method_call_196 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _a_138, NULL, _new_197);
                            check_receiver(_sync_method_call_196, ".", "a.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"dy\";\n                                    END))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 195, column 33)");
                            pony_type_t* _tmp_201[] = {};
                            option_t* _sync_method_call_195 = _enc__method_String_String_to_int(_ctx, _sync_method_call_196, NULL);
                            ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
                            pony_type_t* _tmp_202[] = {};
                            int64_t _fun_call_203 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_195); _fun_call_203;}) / ({int64_t _literal_204 = 2; _literal_204;}));
    int64_t _dy_max_207 = _binop_205;
    /* var agent_waypoints = new [(int, int)](|a.children_named("addway")|) */;
    /* agent_waypoints = new [(int, int)](|a.children_named("addway")|) */;
    check_receiver(_a_138, ".", "a", "children_named", "\"./Ped_util/Global_funs.enc\" (line 196, column 51)");
    _enc__class_String_String_t* _new_210 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_211 = ({"addway";});
    pony_type_t* _tmp_212[] = {};
    _enc__type_init_String_String(_new_210);
    _enc__method_String_String_init(_ctx, _new_210, NULL, _embed_211);
    pony_type_t* _tmp_213[] = {};
    array_t* _sync_method_call_209 = _enc__method__Ped_util_XML_XML_node_children_named(_ctx, _a_138, NULL, _new_210);
    int64_t _size_214 = array_size(_sync_method_call_209);
    array_t* _array_208 = array_mk(_ctx, _size_214, (&(tuple_type)));
    array_t* _agent_waypoints_216 = _array_208;
    /* var dx = x - dx_max */;
    /* dx = x - dx_max */;
    int64_t _binop_217 = (({ _x_170;}) - ({ _dx_max_194;}));
    int64_t _dx_219 = _binop_217;
    /* var dy = y - dy_max */;
    /* dy = y - dy_max */;
    int64_t _binop_220 = (({ _y_181;}) - ({ _dy_max_207;}));
    int64_t _dy_222 = _binop_220;
    /* var j = 0 */;
    /* j = 0 */;
    int64_t _literal_223 = 0;
    int64_t _j_225 = _literal_223;
    /* do
  for b <- a.children_named("addway") do
    agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1)
    unless |agent_waypoints| > 1 then
      print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
      abort("parsing error")
    end
    j = j + 1
  end
  for unused <- [0..max - 1] do
    if dx > x + dx_max then
      print("out of space\n")
      abort("parsing error")
    else
      if dy > y + dy_max then
        dy = y - dy_max
        dx = dx + 1
      else
        dy = dy + 1
      end
    end
    agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000)
    i = i + 1
  end
end */;
    /* for b <- a.children_named("addway") do
  agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1)
  unless |agent_waypoints| > 1 then
    print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
    abort("parsing error")
  end
  j = j + 1
end */;
    void* _for_226;
    check_receiver(_a_138, ".", "a", "children_named", "\"./Ped_util/Global_funs.enc\" (line 201, column 22)");
    _enc__class_String_String_t* _new_234 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_235 = ({"addway";});
    pony_type_t* _tmp_236[] = {};
    _enc__type_init_String_String(_new_234);
    _enc__method_String_String_init(_ctx, _new_234, NULL, _embed_235);
    pony_type_t* _tmp_237[] = {};
    array_t* _sync_method_call_233 = _enc__method__Ped_util_XML_XML_node_children_named(_ctx, _a_138, NULL, _new_234);
    int64_t _start_229 = 0;
    int64_t _stop_230 = (array_size(_sync_method_call_233) - 1);
    int64_t _src_step_232 = 1;
    int64_t _literal_238 = 1;
    int64_t _step_231 = (_literal_238 * _src_step_232);
    range_assert_step(_step_231);
    int64_t _index_227;
    if ((_step_231 > 0))
    {
      _index_227 = _start_229;
    }
    else
    {
      _index_227 = _stop_230;
    };
    while (((_index_227 >= _start_229) && (_index_227 <= _stop_230)))
    {
      _enc__class__Ped_util_XML_XML_node_t* _b_228 = array_get(_sync_method_call_233, _index_227).p;
      /* agent_waypoints(j) = waypoints(yolo_int(yolo_string(b.attribute_value("id").substring(1, 2)).to_int()) - 1) */;
      int64_t _binop_255 = (({check_receiver(_b_228, ".", "b", "attribute_value", "\"./Ped_util/Global_funs.enc\" (line 202, column 67)");
                              _enc__class_String_String_t* _new_242 = _enc__constructor_String_String(_ctx, NULL);
                              char* _embed_243 = ({"id";});
                              pony_type_t* _tmp_244[] = {};
                              _enc__type_init_String_String(_new_242);
                              _enc__method_String_String_init(_ctx, _new_242, NULL, _embed_243);
                              pony_type_t* _tmp_245[] = {};
                              _enc__class_String_String_t* _sync_method_call_241 = _enc__method__Ped_util_XML_XML_node_attribute_value(_ctx, _b_228, NULL, _new_242);
                              check_receiver(_sync_method_call_241, ".", "b.attribute_value(new String.String(EMBED (EMBED char* END)\n                                      \"id\";\n                                    END))", "substring", "\"./Ped_util/Global_funs.enc\" (line 202, column 67)");
                              int64_t _literal_246 = 1;
                              int64_t _literal_247 = 2;
                              pony_type_t* _tmp_248[] = {};
                              option_t* _sync_method_call_240 = _enc__method_String_String_substring(_ctx, _sync_method_call_241, NULL, _literal_246, _literal_247);
                              ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_string");
                              pony_type_t* _tmp_249[] = {};
                              _enc__class_String_String_t* _fun_call_250 = _enc__global_fun__Ped_util_Global_funsyolo_string(_ctx, NULL, _sync_method_call_240);
                              check_receiver(_fun_call_250, ".", "Global_funs.yolo_string(b.attribute_value(new String.String(EMBED (EMBED char* END)\n                                                              \"id\";\n                                                            END)).substring(1, 2))", "to_int", "\"./Ped_util/Global_funs.enc\" (line 202, column 55)");
                              pony_type_t* _tmp_251[] = {};
                              option_t* _sync_method_call_239 = _enc__method_String_String_to_int(_ctx, _fun_call_250, NULL);
                              ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.yolo_int");
                              pony_type_t* _tmp_252[] = {};
                              int64_t _fun_call_253 = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, NULL, _sync_method_call_239); _fun_call_253;}) - ({int64_t _literal_254 = 1; _literal_254;}));
      tuple_t* _access_256 = array_get(_waypoints_30, _binop_255).p;
      array_set(_agent_waypoints_216, _j_225, ((encore_arg_t) {.p = _access_256}));
      /* unless |agent_waypoints| > 1 then
  print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|)
  abort("parsing error")
end */;
      void* _ite_257;
      if (({int64_t _binop_260 = (({int64_t _size_258 = array_size(_agent_waypoints_216); _size_258;}) > ({int64_t _literal_259 = 1; _literal_259;}));
            int64_t _unary_261 = (! _binop_260); _unary_261;}))
      {
        /* print("THIS SHOULD NEVER HAPPEN |new_agent.waypoint|={}\n", |agent_waypoints|) */;
        int64_t _size_262 = array_size(_agent_waypoints_216);
        fprintf(stdout, "THIS SHOULD NEVER HAPPEN |new_agent.waypoint|=%lli\n", _size_262);
        /* abort("parsing error") */;
        /* abort("parsing error") */;
        char* _literal_263 = "parsing error";
        fprintf(stderr, "%s\n", _literal_263);
        /* abort("parsing error") */;
        fprintf(stderr, "\"./Ped_util/Global_funs.enc\" (line 205, column 17)\n");
        /* abort("parsing error") */;
        abort();
        _ite_257 = ((void*) UNIT);
      }
      else
      {
        UNIT;
        _ite_257 = ((void*) UNIT);
      };
      /* j = j + 1 */;
      int64_t _binop_265 = (({ _j_225;}) + ({int64_t _literal_264 = 1; _literal_264;}));
      _j_225 = _binop_265;
      _for_226 = UNIT;
      _index_227 = (_index_227 + _step_231);
    };
    /* for unused <- [0..max - 1] do
  if dx > x + dx_max then
    print("out of space\n")
    abort("parsing error")
  else
    if dy > y + dy_max then
      dy = y - dy_max
      dx = dx + 1
    else
      dy = dy + 1
    end
  end
  agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000)
  i = i + 1
end */;
    void* _for_266;
    /* Range not generated */;
    int64_t _literal_273 = 0;
    int64_t _binop_275 = (({ _max_159;}) - ({int64_t _literal_274 = 1; _literal_274;}));
    int64_t _literal_276 = 1;
    int64_t _literal_277 = 1;
    int64_t _step_271 = (_literal_277 * _literal_276);
    range_assert_step(_step_271);
    int64_t _index_267;
    if ((_step_271 > 0))
    {
      _index_267 = _literal_273;
    }
    else
    {
      _index_267 = _binop_275;
    };
    while (((_index_267 >= _literal_273) && (_index_267 <= _binop_275)))
    {
      int64_t _unused_268 = _index_267;
      /* if dx > x + dx_max then
  print("out of space\n")
  abort("parsing error")
else
  if dy > y + dy_max then
    dy = y - dy_max
    dx = dx + 1
  else
    dy = dy + 1
  end
end */;
      void* _ite_278;
      if (({int64_t _binop_280 = (({ _dx_219;}) > ({int64_t _binop_279 = (({ _x_170;}) + ({ _dx_max_194;})); _binop_279;})); _binop_280;}))
      {
        /* print("out of space\n") */;
        char* _literal_281 = "out of space\n";
        fprintf(stdout, "%s", _literal_281);
        /* abort("parsing error") */;
        /* abort("parsing error") */;
        char* _literal_282 = "parsing error";
        fprintf(stderr, "%s\n", _literal_282);
        /* abort("parsing error") */;
        fprintf(stderr, "\"./Ped_util/Global_funs.enc\" (line 212, column 17)\n");
        /* abort("parsing error") */;
        abort();
        _ite_278 = ((void*) UNIT);
      }
      else
      {
        void* _ite_283;
        if (({int64_t _binop_285 = (({ _dy_222;}) > ({int64_t _binop_284 = (({ _y_181;}) + ({ _dy_max_207;})); _binop_284;})); _binop_285;}))
        {
          /* dy = y - dy_max */;
          int64_t _binop_286 = (({ _y_181;}) - ({ _dy_max_207;}));
          _dy_222 = _binop_286;
          /* dx = dx + 1 */;
          int64_t _binop_288 = (({ _dx_219;}) + ({int64_t _literal_287 = 1; _literal_287;}));
          _dx_219 = _binop_288;
          _ite_283 = ((void*) UNIT);
        }
        else
        {
          int64_t _binop_290 = (({ _dy_222;}) + ({int64_t _literal_289 = 1; _literal_289;}));
          _dy_222 = _binop_290;
          _ite_283 = ((void*) UNIT);
        };
        _ite_278 = ((void*) _ite_283);
      };
      /* agents(i) = new Agent((dx, dy), copy_tr(agent_waypoints), copy_ar(waypoints_distance), i, 10000) */;
      _enc__class__Ped_util_Agent_passive_Agent_t* _new_291 = _enc__constructor__Ped_util_Agent_passive_Agent(_ctx, NULL);
      tuple_t* _tuple_292 = tuple_mk(_ctx, 2);
      tuple_set_type(_tuple_292, 0, ENCORE_PRIMITIVE);
      tuple_set_type(_tuple_292, 1, ENCORE_PRIMITIVE);
      tuple_set(_tuple_292, 0, ((encore_arg_t) {.i = _dx_219}));
      tuple_set(_tuple_292, 1, ((encore_arg_t) {.i = _dy_222}));
      ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.copy_tr");
      pony_type_t* _tmp_293[] = {};
      array_t* _fun_call_294 = _enc__global_fun__Ped_util_Global_funscopy_tr(_ctx, NULL, _agent_waypoints_216);
      ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.copy_ar");
      pony_type_t* _tmp_295[] = {};
      array_t* _fun_call_296 = _enc__global_fun__Ped_util_Global_funscopy_ar(_ctx, NULL, _waypoints_distance_34);
      int64_t _literal_297 = 10000;
      pony_type_t* _tmp_298[] = {};
      _enc__type_init__Ped_util_Agent_passive_Agent(_new_291);
      _enc__method__Ped_util_Agent_passive_Agent_init(_ctx, _new_291, NULL, _tuple_292, _fun_call_294, _fun_call_296, _i_135, _literal_297);
      array_set(_agents_132, _i_135, ((encore_arg_t) {.p = _new_291}));
      /* i = i + 1 */;
      int64_t _binop_300 = (({ _i_135;}) + ({int64_t _literal_299 = 1; _literal_299;}));
      _i_135 = _binop_300;
      _for_266 = UNIT;
      _index_267 = (_index_267 + _step_271);
    };
    _for_136 = _for_266;
    _index_137 = (_index_137 + _step_141);
  };
  /* agents */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "parse_file");
  return ((array_t*) _agents_132);
}


double _enc__global_fun__Ped_util_Global_funsdistance_int(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_x1, int64_t _enc__arg_y1, int64_t _enc__arg_x2, int64_t _enc__arg_y2)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "distance_int");
  int64_t _binop_6 = (({int64_t _binop_5 = (({int64_t _binop_3 = (({int64_t _binop_2 = (({int64_t _binop_1 = (({ _enc__arg_x1;}) - ({int64_t _binop_0 = (({ _enc__arg_x2;}) * ({ _enc__arg_x1;})); _binop_0;})); _binop_1;}) - ({ _enc__arg_x2;})); _binop_2;}) + ({ _enc__arg_y1;})); _binop_3;}) - ({int64_t _binop_4 = (({ _enc__arg_y2;}) * ({ _enc__arg_y1;})); _binop_4;})); _binop_5;}) - ({ _enc__arg_y2;}));
  ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.sqrt");
  pony_type_t* _tmp_7[] = {};
  double _fun_call_8 = _enc__global_fun__Ped_util_Global_funssqrt(_ctx, NULL, ((double) _binop_6));
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "distance_int");
  return ((double) _fun_call_8);
}


int64_t _enc__global_fun__Ped_util_Global_funsround(pony_ctx_t** _ctx, pony_type_t** runtimeType, double _enc__arg_x)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "round");
  /* val i = 0 */;
  /* i = 0 */;
  int64_t _literal_0 = 0;
  int64_t _i_2 = _literal_0;
  /* EMBED (unit)
  { #{i}=round(#{x}); }
END */;
  ({{ _i_2=round(_enc__arg_x); }});
  /* i */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "round");
  return ((int64_t) _i_2);
}


int64_t _enc__global_fun__Ped_util_Global_funsabs(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_a)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "abs");
  int64_t _ite_0;
  if (({int64_t _binop_2 = (({ _enc__arg_a;}) > ({int64_t _literal_1 = 0; _literal_1;})); _binop_2;}))
  {
    _ite_0 = ((int64_t) _enc__arg_a);
  }
  else
  {
    int64_t _binop_5 = (({ _enc__arg_a;}) * ({int64_t _literal_3 = 1;
                                              int64_t _unary_4 = (- _literal_3); _unary_4;}));
    _ite_0 = ((int64_t) _binop_5);
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "abs");
  return ((int64_t) _ite_0);
}


double _enc__global_fun__Ped_util_Global_funssqrt(pony_ctx_t** _ctx, pony_type_t** runtimeType, double _enc__arg_nr)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "sqrt");
  /* EMBED (unit)
  { #{nr}=sqrt(#{nr}); }
END */;
  ({{ _enc__arg_nr=sqrt(_enc__arg_nr); }});
  /* nr */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "sqrt");
  return ((double) _enc__arg_nr);
}


_enc__class_String_String_t* _enc__global_fun__Ped_util_Global_funsyolo_string(pony_ctx_t** _ctx, pony_type_t** runtimeType, option_t* _enc__arg_i)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "yolo_string");
  _enc__class_String_String_t* _match_0;
  _enc__class_String_String_t* _nr_1;
  if ((({int64_t _optionCheck_11;
         _optionCheck_11 = ((JUST == (*_enc__arg_i).tag) && ({int64_t _varBinding_12;
                                                              _enc__class_String_String_t* _optionVal_10 = (*_enc__arg_i).val.p;
                                                              _nr_1 = _optionVal_10;
                                                              _varBinding_12 = 1; _varBinding_12;})); _optionCheck_11;}) && ({int64_t _literal_13 = 1/*True*/; _literal_13;})))
  {
    _match_0 = ((_enc__class_String_String_t*) ({ _nr_1;}));
  }
  else
  {
    option_t* ___2;
    if ((({int64_t _varBinding_8;
           ___2 = _enc__arg_i;
           _varBinding_8 = 1; _varBinding_8;}) && ({int64_t _literal_9 = 1/*True*/; _literal_9;})))
    {
      _match_0 = ((_enc__class_String_String_t*) ({/* print("orEmpty\n") */;
                                                   char* _literal_3 = "orEmpty\n";
                                                   fprintf(stdout, "%s", _literal_3);
                                                   /* abort("parsing error") */;
                                                   /* abort("parsing error") */;
                                                   char* _literal_4 = "parsing error";
                                                   fprintf(stderr, "%s\n", _literal_4);
                                                   /* abort("parsing error") */;
                                                   fprintf(stderr, "\"./Ped_util/Global_funs.enc\" (line 109, column 7)\n");
                                                   /* abort("parsing error") */;
                                                   abort();
                                                   /* "" */;
                                                   _enc__class_String_String_t* _new_5 = _enc__constructor_String_String(_ctx, NULL);
                                                   char* _embed_6 = ({"";});
                                                   pony_type_t* _tmp_7[] = {};
                                                   _enc__type_init_String_String(_new_5);
                                                   _enc__method_String_String_init(_ctx, _new_5, NULL, _embed_6); _new_5;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "yolo_string");
  return ((_enc__class_String_String_t*) _match_0);
}


int64_t _enc__global_fun__Ped_util_Global_funsyolo_int(pony_ctx_t** _ctx, pony_type_t** runtimeType, option_t* _enc__arg_i)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "yolo_int");
  int64_t _match_0;
  int64_t _nr_1;
  if ((({int64_t _optionCheck_9;
         _optionCheck_9 = ((JUST == (*_enc__arg_i).tag) && ({int64_t _varBinding_10;
                                                             int64_t _optionVal_8 = (*_enc__arg_i).val.i;
                                                             _nr_1 = _optionVal_8;
                                                             _varBinding_10 = 1; _varBinding_10;})); _optionCheck_9;}) && ({int64_t _literal_11 = 1/*True*/; _literal_11;})))
  {
    _match_0 = ((int64_t) ({ _nr_1;}));
  }
  else
  {
    option_t* ___2;
    if ((({int64_t _varBinding_6;
           ___2 = _enc__arg_i;
           _varBinding_6 = 1; _varBinding_6;}) && ({int64_t _literal_7 = 1/*True*/; _literal_7;})))
    {
      _match_0 = ((int64_t) ({/* print("or0\n") */;
                              char* _literal_3 = "or0\n";
                              fprintf(stdout, "%s", _literal_3);
                              /* abort("parsing error") */;
                              /* abort("parsing error") */;
                              char* _literal_4 = "parsing error";
                              fprintf(stderr, "%s\n", _literal_4);
                              /* abort("parsing error") */;
                              fprintf(stderr, "\"./Ped_util/Global_funs.enc\" (line 96, column 7)\n");
                              /* abort("parsing error") */;
                              abort();
                              /* 0 */;
                              int64_t _literal_5 = 0; _literal_5;}));
    }
    else
    {
      fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
      exit(1);
    };
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "yolo_int");
  return ((int64_t) _match_0);
}


int64_t _enc__global_fun__Ped_util_Global_funsmin(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_a, int64_t _enc__arg_b)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "min");
  int64_t _ite_0;
  if (({int64_t _binop_1 = (({ _enc__arg_a;}) < ({ _enc__arg_b;})); _binop_1;}))
  {
    _ite_0 = ((int64_t) _enc__arg_a);
  }
  else
  {
    _ite_0 = ((int64_t) _enc__arg_b);
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "min");
  return ((int64_t) _ite_0);
}


int64_t _enc__global_fun__Ped_util_Global_funsmax(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_a, int64_t _enc__arg_b)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "max");
  int64_t _ite_0;
  if (({int64_t _binop_1 = (({ _enc__arg_a;}) > ({ _enc__arg_b;})); _binop_1;}))
  {
    _ite_0 = ((int64_t) _enc__arg_a);
  }
  else
  {
    _ite_0 = ((int64_t) _enc__arg_b);
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "max");
  return ((int64_t) _ite_0);
}


array_t* _enc__global_fun__Ped_util_Global_funsflatten(pony_ctx_t** _ctx, pony_type_t** runtimeType, array_t* _enc__arg_listlist)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "flatten");
  /* var len = 0 */;
  /* len = 0 */;
  int64_t _literal_0 = 0;
  int64_t _len_2 = _literal_0;
  /* for a <- listlist do
  len = len + |a|
end */;
  void* _for_3;
  int64_t _start_6 = 0;
  int64_t _stop_7 = (array_size(_enc__arg_listlist) - 1);
  int64_t _src_step_9 = 1;
  int64_t _literal_10 = 1;
  int64_t _step_8 = (_literal_10 * _src_step_9);
  range_assert_step(_step_8);
  int64_t _index_4;
  if ((_step_8 > 0))
  {
    _index_4 = _start_6;
  }
  else
  {
    _index_4 = _stop_7;
  };
  while (((_index_4 >= _start_6) && (_index_4 <= _stop_7)))
  {
    array_t* _a_5 = array_get(_enc__arg_listlist, _index_4).p;
    int64_t _binop_12 = (({ _len_2;}) + ({int64_t _size_11 = array_size(_a_5); _size_11;}));
    _len_2 = _binop_12;
    _for_3 = UNIT;
    _index_4 = (_index_4 + _step_8);
  };
  /* var list = new [(int, int)](len) */;
  /* list = new [(int, int)](len) */;
  array_t* _array_13 = array_mk(_ctx, _len_2, (&(tuple_type)));
  array_t* _list_15 = _array_13;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_16 = 0;
  int64_t _i_18 = _literal_16;
  /* for a <- listlist do
  for b <- a do
    list(i) = b
    i = i + 1
  end
end */;
  void* _for_19;
  int64_t _start_22 = 0;
  int64_t _stop_23 = (array_size(_enc__arg_listlist) - 1);
  int64_t _src_step_25 = 1;
  int64_t _literal_26 = 1;
  int64_t _step_24 = (_literal_26 * _src_step_25);
  range_assert_step(_step_24);
  int64_t _index_20;
  if ((_step_24 > 0))
  {
    _index_20 = _start_22;
  }
  else
  {
    _index_20 = _stop_23;
  };
  while (((_index_20 >= _start_22) && (_index_20 <= _stop_23)))
  {
    array_t* _a_21 = array_get(_enc__arg_listlist, _index_20).p;
    void* _for_27;
    int64_t _start_30 = 0;
    int64_t _stop_31 = (array_size(_a_21) - 1);
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
      tuple_t* _b_29 = array_get(_a_21, _index_28).p;
      /* list(i) = b */;
      array_set(_list_15, _i_18, ((encore_arg_t) {.p = _b_29}));
      /* i = i + 1 */;
      int64_t _binop_36 = (({ _i_18;}) + ({int64_t _literal_35 = 1; _literal_35;}));
      _i_18 = _binop_36;
      _for_27 = UNIT;
      _index_28 = (_index_28 + _step_32);
    };
    _for_19 = _for_27;
    _index_20 = (_index_20 + _step_24);
  };
  /* list */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "flatten");
  return ((array_t*) _list_15);
}


array_t* _enc__global_fun__Ped_util_Global_funscopy_tr(pony_ctx_t** _ctx, pony_type_t** runtimeType, array_t* _enc__arg_arr)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "copy_tr");
  /* var a2 = new [(int, int)](|arr|) */;
  /* a2 = new [(int, int)](|arr|) */;
  int64_t _size_1 = array_size(_enc__arg_arr);
  array_t* _array_0 = array_mk(_ctx, _size_1, (&(tuple_type)));
  array_t* _a2_3 = _array_0;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_4 = 0;
  int64_t _i_6 = _literal_4;
  /* for a <- arr do
  a2(i) = a
  i = i + 1
end */;
  void* _for_7;
  int64_t _start_10 = 0;
  int64_t _stop_11 = (array_size(_enc__arg_arr) - 1);
  int64_t _src_step_13 = 1;
  int64_t _literal_14 = 1;
  int64_t _step_12 = (_literal_14 * _src_step_13);
  range_assert_step(_step_12);
  int64_t _index_8;
  if ((_step_12 > 0))
  {
    _index_8 = _start_10;
  }
  else
  {
    _index_8 = _stop_11;
  };
  while (((_index_8 >= _start_10) && (_index_8 <= _stop_11)))
  {
    tuple_t* _a_9 = array_get(_enc__arg_arr, _index_8).p;
    /* a2(i) = a */;
    array_set(_a2_3, _i_6, ((encore_arg_t) {.p = _a_9}));
    /* i = i + 1 */;
    int64_t _binop_16 = (({ _i_6;}) + ({int64_t _literal_15 = 1; _literal_15;}));
    _i_6 = _binop_16;
    _for_7 = UNIT;
    _index_8 = (_index_8 + _step_12);
  };
  /* a2 */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "copy_tr");
  return ((array_t*) _a2_3);
}


array_t* _enc__global_fun__Ped_util_Global_funscopy_ar(pony_ctx_t** _ctx, pony_type_t** runtimeType, array_t* _enc__arg_arr)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "copy_ar");
  /* var a2 = new [int](|arr|) */;
  /* a2 = new [int](|arr|) */;
  int64_t _size_1 = array_size(_enc__arg_arr);
  array_t* _array_0 = array_mk(_ctx, _size_1, ENCORE_PRIMITIVE);
  array_t* _a2_3 = _array_0;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_4 = 0;
  int64_t _i_6 = _literal_4;
  /* for a <- arr do
  a2(i) = a
  i = i + 1
end */;
  void* _for_7;
  int64_t _start_10 = 0;
  int64_t _stop_11 = (array_size(_enc__arg_arr) - 1);
  int64_t _src_step_13 = 1;
  int64_t _literal_14 = 1;
  int64_t _step_12 = (_literal_14 * _src_step_13);
  range_assert_step(_step_12);
  int64_t _index_8;
  if ((_step_12 > 0))
  {
    _index_8 = _start_10;
  }
  else
  {
    _index_8 = _stop_11;
  };
  while (((_index_8 >= _start_10) && (_index_8 <= _stop_11)))
  {
    int64_t _a_9 = array_get(_enc__arg_arr, _index_8).i;
    /* a2(i) = a */;
    array_set(_a2_3, _i_6, ((encore_arg_t) {.i = _a_9}));
    /* i = i + 1 */;
    int64_t _binop_16 = (({ _i_6;}) + ({int64_t _literal_15 = 1; _literal_15;}));
    _i_6 = _binop_16;
    _for_7 = UNIT;
    _index_8 = (_index_8 + _step_12);
  };
  /* a2 */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "copy_ar");
  return ((array_t*) _a2_3);
}


tuple_t* _enc__global_fun__Ped_util_Global_funsextreme_check(pony_ctx_t** _ctx, pony_type_t** runtimeType, tuple_t* _enc__arg_a, tuple_t* _enc__arg_b)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "extreme_check");
  tuple_t* _tuple_1 = tuple_mk(_ctx, 2);
  tuple_set_type(_tuple_1, 0, (&(tuple_type)));
  tuple_set_type(_tuple_1, 1, (&(tuple_type)));
  tuple_set(_tuple_1, 0, ((encore_arg_t) {.p = _enc__arg_a}));
  tuple_set(_tuple_1, 1, ((encore_arg_t) {.p = _enc__arg_b}));
  tuple_t* _match_0;
  int64_t _x_2;
  int64_t _y_3;
  int64_t _xmax_4;
  int64_t _ymax_5;
  int64_t _xmin_6;
  int64_t _ymin_7;
  if ((({int64_t _tupleCheck_17;
         _tupleCheck_17 = 1;
         tuple_t* _tupleAccess_18 = tuple_get(_tuple_1, 0).p;
         int64_t _tupleCheck_19;
         _tupleCheck_19 = 1;
         int64_t _tupleAccess_20 = tuple_get(_tupleAccess_18, 0).i;
         int64_t _varBinding_21;
         _x_2 = _tupleAccess_20;
         _varBinding_21 = 1;
         _tupleCheck_19 = (_tupleCheck_19 && _varBinding_21);
         int64_t _tupleAccess_22 = tuple_get(_tupleAccess_18, 1).i;
         int64_t _varBinding_23;
         _y_3 = _tupleAccess_22;
         _varBinding_23 = 1;
         _tupleCheck_19 = (_tupleCheck_19 && _varBinding_23);
         _tupleCheck_17 = (_tupleCheck_17 && _tupleCheck_19);
         tuple_t* _tupleAccess_24 = tuple_get(_tuple_1, 1).p;
         int64_t _tupleCheck_25;
         _tupleCheck_25 = 1;
         int64_t _tupleAccess_26 = tuple_get(_tupleAccess_24, 0).i;
         int64_t _varBinding_27;
         _xmax_4 = _tupleAccess_26;
         _varBinding_27 = 1;
         _tupleCheck_25 = (_tupleCheck_25 && _varBinding_27);
         int64_t _tupleAccess_28 = tuple_get(_tupleAccess_24, 1).i;
         int64_t _varBinding_29;
         _ymax_5 = _tupleAccess_28;
         _varBinding_29 = 1;
         _tupleCheck_25 = (_tupleCheck_25 && _varBinding_29);
         int64_t _tupleAccess_30 = tuple_get(_tupleAccess_24, 2).i;
         int64_t _varBinding_31;
         _xmin_6 = _tupleAccess_30;
         _varBinding_31 = 1;
         _tupleCheck_25 = (_tupleCheck_25 && _varBinding_31);
         int64_t _tupleAccess_32 = tuple_get(_tupleAccess_24, 3).i;
         int64_t _varBinding_33;
         _ymin_7 = _tupleAccess_32;
         _varBinding_33 = 1;
         _tupleCheck_25 = (_tupleCheck_25 && _varBinding_33);
         _tupleCheck_17 = (_tupleCheck_17 && _tupleCheck_25); _tupleCheck_17;}) && ({int64_t _literal_34 = 1/*True*/; _literal_34;})))
  {
    _match_0 = ((tuple_t*) ({tuple_t* _tuple_8 = tuple_mk(_ctx, 4);
                             tuple_set_type(_tuple_8, 0, ENCORE_PRIMITIVE);
                             tuple_set_type(_tuple_8, 1, ENCORE_PRIMITIVE);
                             tuple_set_type(_tuple_8, 2, ENCORE_PRIMITIVE);
                             tuple_set_type(_tuple_8, 3, ENCORE_PRIMITIVE);
                             ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.max");
                             pony_type_t* _tmp_9[] = {};
                             int64_t _fun_call_10 = _enc__global_fun__Ped_util_Global_funsmax(_ctx, NULL, _x_2, _xmax_4);
                             ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.max");
                             pony_type_t* _tmp_11[] = {};
                             int64_t _fun_call_12 = _enc__global_fun__Ped_util_Global_funsmax(_ctx, NULL, _y_3, _ymax_5);
                             ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.min");
                             pony_type_t* _tmp_13[] = {};
                             int64_t _fun_call_14 = _enc__global_fun__Ped_util_Global_funsmin(_ctx, NULL, _x_2, _xmin_6);
                             ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.min");
                             pony_type_t* _tmp_15[] = {};
                             int64_t _fun_call_16 = _enc__global_fun__Ped_util_Global_funsmin(_ctx, NULL, _y_3, _ymin_7);
                             tuple_set(_tuple_8, 0, ((encore_arg_t) {.i = _fun_call_10}));
                             tuple_set(_tuple_8, 1, ((encore_arg_t) {.i = _fun_call_12}));
                             tuple_set(_tuple_8, 2, ((encore_arg_t) {.i = _fun_call_14}));
                             tuple_set(_tuple_8, 3, ((encore_arg_t) {.i = _fun_call_16})); _tuple_8;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "extreme_check");
  return ((tuple_t*) _match_0);
}


tuple_t* _enc__global_fun__Ped_util_Global_funsfind_extreme(pony_ctx_t** _ctx, pony_type_t** runtimeType, array_t* _enc__arg_agents)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "find_extreme");
  /* var extreme = (agents(0).x, agents(0).y, agents(0).x, agents(0).y) */;
  /* extreme = (agents(0).x, agents(0).y, agents(0).x, agents(0).y) */;
  tuple_t* _tuple_0 = tuple_mk(_ctx, 4);
  tuple_set_type(_tuple_0, 0, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_0, 1, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_0, 2, ENCORE_PRIMITIVE);
  tuple_set_type(_tuple_0, 3, ENCORE_PRIMITIVE);
  int64_t _literal_1 = 0;
  _enc__class__Ped_util_Agent_passive_Agent_t* _access_2 = array_get(_enc__arg_agents, _literal_1).p;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_access_2, "x");
  int64_t _fieldacc_3 = (*_access_2)._enc__field_x;
  int64_t _literal_4 = 0;
  _enc__class__Ped_util_Agent_passive_Agent_t* _access_5 = array_get(_enc__arg_agents, _literal_4).p;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_access_5, "y");
  int64_t _fieldacc_6 = (*_access_5)._enc__field_y;
  int64_t _literal_7 = 0;
  _enc__class__Ped_util_Agent_passive_Agent_t* _access_8 = array_get(_enc__arg_agents, _literal_7).p;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_access_8, "x");
  int64_t _fieldacc_9 = (*_access_8)._enc__field_x;
  int64_t _literal_10 = 0;
  _enc__class__Ped_util_Agent_passive_Agent_t* _access_11 = array_get(_enc__arg_agents, _literal_10).p;
  ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_access_11, "y");
  int64_t _fieldacc_12 = (*_access_11)._enc__field_y;
  tuple_set(_tuple_0, 0, ((encore_arg_t) {.i = _fieldacc_3}));
  tuple_set(_tuple_0, 1, ((encore_arg_t) {.i = _fieldacc_6}));
  tuple_set(_tuple_0, 2, ((encore_arg_t) {.i = _fieldacc_9}));
  tuple_set(_tuple_0, 3, ((encore_arg_t) {.i = _fieldacc_12}));
  tuple_t* _extreme_14 = _tuple_0;
  /* var i = 0 */;
  /* i = 0 */;
  int64_t _literal_15 = 0;
  int64_t _i_17 = _literal_15;
  /* for a <- agents do
  i = i + 1
  extreme = extreme_check(a.pos(), extreme)
  for index <- [0..|a.targets_x| - 1] do
    extreme = extreme_check(((a.targets_x)(index), (a.targets_y)(index)), extreme)
  end
end */;
  void* _for_18;
  int64_t _start_21 = 0;
  int64_t _stop_22 = (array_size(_enc__arg_agents) - 1);
  int64_t _src_step_24 = 1;
  int64_t _literal_25 = 1;
  int64_t _step_23 = (_literal_25 * _src_step_24);
  range_assert_step(_step_23);
  int64_t _index_19;
  if ((_step_23 > 0))
  {
    _index_19 = _start_21;
  }
  else
  {
    _index_19 = _stop_22;
  };
  while (((_index_19 >= _start_21) && (_index_19 <= _stop_22)))
  {
    _enc__class__Ped_util_Agent_passive_Agent_t* _a_20 = array_get(_enc__arg_agents, _index_19).p;
    /* i = i + 1 */;
    int64_t _binop_27 = (({ _i_17;}) + ({int64_t _literal_26 = 1; _literal_26;}));
    _i_17 = _binop_27;
    /* extreme = extreme_check(a.pos(), extreme) */;
    check_receiver(_a_20, ".", "a", "pos", "\"./Ped_util/Global_funs.enc\" (line 23, column 29)");
    pony_type_t* _tmp_29[] = {};
    tuple_t* _sync_method_call_28 = _enc__method__Ped_util_Agent_passive_Agent_pos(_ctx, _a_20, NULL);
    ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.extreme_check");
    pony_type_t* _tmp_30[] = {};
    tuple_t* _fun_call_31 = _enc__global_fun__Ped_util_Global_funsextreme_check(_ctx, NULL, _sync_method_call_28, _extreme_14);
    _extreme_14 = _fun_call_31;
    /* for index <- [0..|a.targets_x| - 1] do
  extreme = extreme_check(((a.targets_x)(index), (a.targets_y)(index)), extreme)
end */;
    void* _for_32;
    /* Range not generated */;
    int64_t _literal_39 = 0;
    int64_t _binop_43 = (({ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_20, "targets_x");
                           array_t* _fieldacc_40 = (*_a_20)._enc__field_targets_x;
                           int64_t _size_41 = array_size(_fieldacc_40); _size_41;}) - ({int64_t _literal_42 = 1; _literal_42;}));
    int64_t _literal_44 = 1;
    int64_t _literal_45 = 1;
    int64_t _step_37 = (_literal_45 * _literal_44);
    range_assert_step(_step_37);
    int64_t _index_33;
    if ((_step_37 > 0))
    {
      _index_33 = _literal_39;
    }
    else
    {
      _index_33 = _binop_43;
    };
    while (((_index_33 >= _literal_39) && (_index_33 <= _binop_43)))
    {
      int64_t _index_34 = _index_33;
      tuple_t* _tuple_46 = tuple_mk(_ctx, 2);
      tuple_set_type(_tuple_46, 0, ENCORE_PRIMITIVE);
      tuple_set_type(_tuple_46, 1, ENCORE_PRIMITIVE);
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_20, "targets_x");
      array_t* _fieldacc_47 = (*_a_20)._enc__field_targets_x;
      int64_t _access_48 = array_get(_fieldacc_47, _index_34).i;
      ENC_DTRACE3(FIELD_ACCESS, (uintptr_t)*_ctx, (uintptr_t)_a_20, "targets_y");
      array_t* _fieldacc_49 = (*_a_20)._enc__field_targets_y;
      int64_t _access_50 = array_get(_fieldacc_49, _index_34).i;
      tuple_set(_tuple_46, 0, ((encore_arg_t) {.i = _access_48}));
      tuple_set(_tuple_46, 1, ((encore_arg_t) {.i = _access_50}));
      ENC_DTRACE2(FUNCTION_CALL, (uintptr_t)*_ctx, "Global_funs.extreme_check");
      pony_type_t* _tmp_51[] = {};
      tuple_t* _fun_call_52 = _enc__global_fun__Ped_util_Global_funsextreme_check(_ctx, NULL, _tuple_46, _extreme_14);
      _extreme_14 = _fun_call_52;
      _for_32 = UNIT;
      _index_33 = (_index_33 + _step_37);
    };
    _for_18 = _for_32;
    _index_19 = (_index_19 + _step_23);
  };
  /* match extreme with
  case (xmax, ymax, xmin, ymin) =>
    (xmax, ymax, xmin, ymin)
  end

end */;
  tuple_t* _match_53;
  int64_t _xmax_54;
  int64_t _ymax_55;
  int64_t _xmin_56;
  int64_t _ymin_57;
  if ((({int64_t _tupleCheck_59;
         _tupleCheck_59 = 1;
         int64_t _tupleAccess_60 = tuple_get(_extreme_14, 0).i;
         int64_t _varBinding_61;
         _xmax_54 = _tupleAccess_60;
         _varBinding_61 = 1;
         _tupleCheck_59 = (_tupleCheck_59 && _varBinding_61);
         int64_t _tupleAccess_62 = tuple_get(_extreme_14, 1).i;
         int64_t _varBinding_63;
         _ymax_55 = _tupleAccess_62;
         _varBinding_63 = 1;
         _tupleCheck_59 = (_tupleCheck_59 && _varBinding_63);
         int64_t _tupleAccess_64 = tuple_get(_extreme_14, 2).i;
         int64_t _varBinding_65;
         _xmin_56 = _tupleAccess_64;
         _varBinding_65 = 1;
         _tupleCheck_59 = (_tupleCheck_59 && _varBinding_65);
         int64_t _tupleAccess_66 = tuple_get(_extreme_14, 3).i;
         int64_t _varBinding_67;
         _ymin_57 = _tupleAccess_66;
         _varBinding_67 = 1;
         _tupleCheck_59 = (_tupleCheck_59 && _varBinding_67); _tupleCheck_59;}) && ({int64_t _literal_68 = 1/*True*/; _literal_68;})))
  {
    _match_53 = ((tuple_t*) ({tuple_t* _tuple_58 = tuple_mk(_ctx, 4);
                              tuple_set_type(_tuple_58, 0, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_58, 1, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_58, 2, ENCORE_PRIMITIVE);
                              tuple_set_type(_tuple_58, 3, ENCORE_PRIMITIVE);
                              tuple_set(_tuple_58, 0, ((encore_arg_t) {.i = _xmax_54}));
                              tuple_set(_tuple_58, 1, ((encore_arg_t) {.i = _ymax_55}));
                              tuple_set(_tuple_58, 2, ((encore_arg_t) {.i = _xmin_56}));
                              tuple_set(_tuple_58, 3, ((encore_arg_t) {.i = _ymin_57})); _tuple_58;}));
  }
  else
  {
    fprintf(stderr, "*** Runtime error: No matching clause was found ***\n");
    exit(1);
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "find_extreme");
  return ((tuple_t*) _match_53);
}


double _enc__global_fun__Ped_util_Global_funsinv_sqrt(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_input)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "inv_sqrt");
  /* var ret = 0.0 */;
  /* ret = 0.0 */;
  double _literal_0 = 0.0;
  double _ret_2 = _literal_0;
  /* EMBED (unit)
  {
  float x = #{input};
  float xhalf = 0.5f * x;
  int i = *(int*)&x;
  i = 0x5f3759df - (i >> 1);
  x = *(float*)&i;
  #{ret}= x*(1.5f - xhalf*x*x);
  }
END */;
  ({{
  float x = _enc__arg_input;
  float xhalf = 0.5f * x;
  int i = *(int*)&x;
  i = 0x5f3759df - (i >> 1);
  x = *(float*)&i;
  _ret_2= x*(1.5f - xhalf*x*x);
  }});
  /* ret */;
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "inv_sqrt");
  return ((double) _ret_2);
}


int64_t _enc__global_fun__Ped_util_Regionslink(pony_ctx_t** _ctx, pony_type_t** runtimeType, _enc__class__Ped_util_Regions_Box_t* _enc__arg_a, _enc__class__Ped_util_Regions_Box_t* _enc__arg_b)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "link");
  /* val aa = a!link(b) */;
  /* aa = a!link(b) */;
  check_receiver(_enc__arg_a, " ! ", "a", "link", "\"./Ped_util/Regions.enc\" (line 11, column 13)");
  pony_type_t* _tmp_0[] = {};
  future_t* _fut_1 = _enc__method__Ped_util_Regions_Box_link_future(_ctx, _enc__arg_a, NULL, _enc__arg_b);
  future_t* _aa_3 = _fut_1;
  /* val bb = b!link(a) */;
  /* bb = b!link(a) */;
  check_receiver(_enc__arg_b, " ! ", "b", "link", "\"./Ped_util/Regions.enc\" (line 12, column 13)");
  pony_type_t* _tmp_4[] = {};
  future_t* _fut_5 = _enc__method__Ped_util_Regions_Box_link_future(_ctx, _enc__arg_b, NULL, _enc__arg_a);
  future_t* _bb_7 = _fut_5;
  /* val aaa = get(aa) */;
  /* aaa = get(aa) */;
  int64_t _tmp_8 = future_get_actor(_ctx, _aa_3).i;
  int64_t _aaa_10 = _tmp_8;
  /* val bbb = get(bb) */;
  /* bbb = get(bb) */;
  int64_t _tmp_11 = future_get_actor(_ctx, _bb_7).i;
  int64_t _bbb_13 = _tmp_11;
  /* aaa && bbb */;
  int64_t _binop_14 = (({ _aaa_10;}) && ({ _bbb_13;}));
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "link");
  return ((int64_t) _binop_14);
}


void* _enc__global_fun__Ped_util_Regionsregions(pony_ctx_t** _ctx, pony_type_t** runtimeType, array_t* _enc__arg_agents, int64_t _enc__arg_boxes)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "regions");
  /* var box = new Tiling_box(agents, boxes) */;
  /* box = new Tiling_box(agents, boxes) */;
  _enc__class__Ped_util_Regions_Tiling_box_t* _new_0 = _enc__constructor__Ped_util_Regions_Tiling_box(_ctx, NULL);
  pony_type_t* _tmp_1[] = {};
  _enc__type_init__Ped_util_Regions_Tiling_box(_new_0);
  _enc__method__Ped_util_Regions_Tiling_box_init_one_way(_ctx, _new_0, NULL, _enc__arg_agents, _enc__arg_boxes);
  _enc__class__Ped_util_Regions_Tiling_box_t* _box_3 = _new_0;
  /* box!move(10000) */;
  check_receiver(_box_3, " ! ", "box", "move", "\"./Ped_util/Regions.enc\" (line 8, column 6)");
  int64_t _literal_4 = 10000;
  pony_type_t* _tmp_5[] = {};
  _enc__method__Ped_util_Regions_Tiling_box_move_one_way(_ctx, _box_3, NULL, _literal_4);
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "regions");
  return UNIT;
}


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_bool(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_x)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "string_from_bool");
  _enc__class_String_String_t* _ite_0;
  if (({ _enc__arg_x;}))
  {
    _enc__class_String_String_t* _new_1 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_2 = ({"true";});
    pony_type_t* _tmp_3[] = {};
    _enc__type_init_String_String(_new_1);
    _enc__method_String_String_init(_ctx, _new_1, NULL, _embed_2);
    _ite_0 = ((_enc__class_String_String_t*) _new_1);
  }
  else
  {
    _enc__class_String_String_t* _new_4 = _enc__constructor_String_String(_ctx, NULL);
    char* _embed_5 = ({"false";});
    pony_type_t* _tmp_6[] = {};
    _enc__type_init_String_String(_new_4);
    _enc__method_String_String_init(_ctx, _new_4, NULL, _embed_5);
    _ite_0 = ((_enc__class_String_String_t*) _new_4);
  };
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "string_from_bool");
  return ((_enc__class_String_String_t*) _ite_0);
}


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_int(pony_ctx_t** _ctx, pony_type_t** runtimeType, int64_t _enc__arg_n)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "string_from_int");
  _enc__class_String_String_t* _new_0 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_1 = ({int n = _enc__arg_n;
               int len = n == 0? 2:
                         n < 0? (int) ceil(log10(-n)) + 2:
                                (int) ceil(log10(n)) + 1;
               char *s = encore_alloc(*_ctx, len);
               sprintf(s, "%d", n);
               s;});
  pony_type_t* _tmp_2[] = {};
  _enc__type_init_String_String(_new_0);
  _enc__method_String_String_init(_ctx, _new_0, NULL, _embed_1);
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "string_from_int");
  return ((_enc__class_String_String_t*) _new_0);
}


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_real(pony_ctx_t** _ctx, pony_type_t** runtimeType, double _enc__arg_n)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "string_from_real");
  _enc__class_String_String_t* _new_0 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_1 = ({double n = _enc__arg_n;
               char buf[13] = { 0 }; // long enough to print the minimum double value.
               char *s = encore_alloc(*_ctx, sizeof(buf));
               snprintf(s, sizeof(buf), "%.5g", n);
               s;});
  pony_type_t* _tmp_2[] = {};
  _enc__type_init_String_String(_new_0);
  _enc__method_String_String_init(_ctx, _new_0, NULL, _embed_1);
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "string_from_real");
  return ((_enc__class_String_String_t*) _new_0);
}


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_array(pony_ctx_t** _ctx, pony_type_t** runtimeType, array_t* _enc__arg_arr)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "string_from_array");
  /* len = |arr| */;
  int64_t _size_0 = array_size(_enc__arg_arr);
  int64_t _len_2 = _size_0;
  /* s = EMBED (EMBED char* END)
  encore_alloc(*_ctx, #{len}+ 1);
END */;
  char* _embed_3 = ({encore_alloc(*_ctx, _len_2+ 1);});
  char* _s_5 = _embed_3;
  /* p = s */;
  char* _p_7 = _s_5;
  /* for c <- arr do
  EMBED (unit)
    *#{p}++ = #{c};
  END
end */;
  void* _for_8;
  int64_t _start_11 = 0;
  int64_t _stop_12 = (array_size(_enc__arg_arr) - 1);
  int64_t _src_step_14 = 1;
  int64_t _literal_15 = 1;
  int64_t _step_13 = (_literal_15 * _src_step_14);
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
    char _c_10 = array_get(_enc__arg_arr, _index_9).i;
    ({*_p_7++ = _c_10;});
    _for_8 = UNIT;
    _index_9 = (_index_9 + _step_13);
  };
  /* new String(s) */;
  _enc__class_String_String_t* _new_16 = _enc__constructor_String_String(_ctx, NULL);
  pony_type_t* _tmp_17[] = {};
  _enc__type_init_String_String(_new_16);
  _enc__method_String_String_init(_ctx, _new_16, NULL, _s_5);
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "string_from_array");
  return ((_enc__class_String_String_t*) _new_16);
}


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_char(pony_ctx_t** _ctx, pony_type_t** runtimeType, char _enc__arg_c)
{
  ENC_DTRACE2(FUNCTION_ENTRY, (uintptr_t)*_ctx, "string_from_char");
  /* s = EMBED (EMBED char* END)
  encore_alloc(*_ctx, 2);
END */;
  char* _embed_0 = ({encore_alloc(*_ctx, 2);});
  char* _s_2 = _embed_0;
  _enc__class_String_String_t* _new_3 = _enc__constructor_String_String(_ctx, NULL);
  char* _embed_4 = ({*_s_2= _enc__arg_c; _s_2;});
  pony_type_t* _tmp_5[] = {};
  _enc__type_init_String_String(_new_3);
  _enc__method_String_String_init(_ctx, _new_3, NULL, _embed_4);
  ENC_DTRACE2(FUNCTION_EXIT, (uintptr_t)*_ctx, "string_from_char");
  return ((_enc__class_String_String_t*) _new_3);
}


value_t _enc__fun_wrapper__Ped_util_Global_funsparse_file(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funsparse_file(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsdistance_int(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.d = _enc__global_fun__Ped_util_Global_funsdistance_int(_ctx, runtimeType, (_args[0]).i, (_args[1]).i, (_args[2]).i, (_args[3]).i)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsround(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.i = _enc__global_fun__Ped_util_Global_funsround(_ctx, runtimeType, (_args[0]).d)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsabs(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.i = _enc__global_fun__Ped_util_Global_funsabs(_ctx, runtimeType, (_args[0]).i)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funssqrt(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.d = _enc__global_fun__Ped_util_Global_funssqrt(_ctx, runtimeType, (_args[0]).d)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsyolo_string(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funsyolo_string(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsyolo_int(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.i = _enc__global_fun__Ped_util_Global_funsyolo_int(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsmin(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.i = _enc__global_fun__Ped_util_Global_funsmin(_ctx, runtimeType, (_args[0]).i, (_args[1]).i)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsmax(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.i = _enc__global_fun__Ped_util_Global_funsmax(_ctx, runtimeType, (_args[0]).i, (_args[1]).i)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsflatten(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funsflatten(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funscopy_tr(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funscopy_tr(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funscopy_ar(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funscopy_ar(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsextreme_check(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funsextreme_check(_ctx, runtimeType, (_args[0]).p, (_args[1]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsfind_extreme(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Global_funsfind_extreme(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Global_funsinv_sqrt(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.d = _enc__global_fun__Ped_util_Global_funsinv_sqrt(_ctx, runtimeType, (_args[0]).i)});
}


value_t _enc__fun_wrapper__Ped_util_Regionslink(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.i = _enc__global_fun__Ped_util_Regionslink(_ctx, runtimeType, (_args[0]).p, (_args[1]).p)});
}


value_t _enc__fun_wrapper__Ped_util_Regionsregions(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun__Ped_util_Regionsregions(_ctx, runtimeType, (_args[0]).p, (_args[1]).i)});
}


value_t _enc__fun_wrapper_Stringstring_from_bool(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun_Stringstring_from_bool(_ctx, runtimeType, (_args[0]).i)});
}


value_t _enc__fun_wrapper_Stringstring_from_int(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun_Stringstring_from_int(_ctx, runtimeType, (_args[0]).i)});
}


value_t _enc__fun_wrapper_Stringstring_from_real(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun_Stringstring_from_real(_ctx, runtimeType, (_args[0]).d)});
}


value_t _enc__fun_wrapper_Stringstring_from_array(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun_Stringstring_from_array(_ctx, runtimeType, (_args[0]).p)});
}


value_t _enc__fun_wrapper_Stringstring_from_char(pony_ctx_t** _ctx, pony_type_t** runtimeType, value_t _args[], void* _env_not_used)
{
  return ((encore_arg_t) {.p = _enc__global_fun_Stringstring_from_char(_ctx, runtimeType, (_args[0]).i)});
}


struct closure* _enc__closure__Ped_util_Global_funsparse_file = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsparse_file)))};


struct closure* _enc__closure__Ped_util_Global_funsdistance_int = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsdistance_int)))};


struct closure* _enc__closure__Ped_util_Global_funsround = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsround)))};


struct closure* _enc__closure__Ped_util_Global_funsabs = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsabs)))};


struct closure* _enc__closure__Ped_util_Global_funssqrt = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funssqrt)))};


struct closure* _enc__closure__Ped_util_Global_funsyolo_string = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsyolo_string)))};


struct closure* _enc__closure__Ped_util_Global_funsyolo_int = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsyolo_int)))};


struct closure* _enc__closure__Ped_util_Global_funsmin = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsmin)))};


struct closure* _enc__closure__Ped_util_Global_funsmax = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsmax)))};


struct closure* _enc__closure__Ped_util_Global_funsflatten = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsflatten)))};


struct closure* _enc__closure__Ped_util_Global_funscopy_tr = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funscopy_tr)))};


struct closure* _enc__closure__Ped_util_Global_funscopy_ar = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funscopy_ar)))};


struct closure* _enc__closure__Ped_util_Global_funsextreme_check = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsextreme_check)))};


struct closure* _enc__closure__Ped_util_Global_funsfind_extreme = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsfind_extreme)))};


struct closure* _enc__closure__Ped_util_Global_funsinv_sqrt = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Global_funsinv_sqrt)))};


struct closure* _enc__closure__Ped_util_Regionslink = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Regionslink)))};


struct closure* _enc__closure__Ped_util_Regionsregions = &(struct closure){.call=((void*) (&(_enc__fun_wrapper__Ped_util_Regionsregions)))};


struct closure* _enc__closure_Stringstring_from_bool = &(struct closure){.call=((void*) (&(_enc__fun_wrapper_Stringstring_from_bool)))};


struct closure* _enc__closure_Stringstring_from_int = &(struct closure){.call=((void*) (&(_enc__fun_wrapper_Stringstring_from_int)))};


struct closure* _enc__closure_Stringstring_from_real = &(struct closure){.call=((void*) (&(_enc__fun_wrapper_Stringstring_from_real)))};


struct closure* _enc__closure_Stringstring_from_array = &(struct closure){.call=((void*) (&(_enc__fun_wrapper_Stringstring_from_array)))};


struct closure* _enc__closure_Stringstring_from_char = &(struct closure){.call=((void*) (&(_enc__fun_wrapper_Stringstring_from_char)))};


int main(int argc, char** argv)
{
  return encore_start(argc, argv, (&(_enc__class__parallel_small_Main_type)));
}
