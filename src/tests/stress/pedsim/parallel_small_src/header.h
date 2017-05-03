#ifndef HEADER_H
#define HEADER_H


#define _XOPEN_SOURCE 800


#include <pthread.h>
#include <pony.h>
#include <pool.h>
#include <stdlib.h>
#include <closure.h>
#include <stream.h>
#include <array.h>
#include <tuple.h>
#include <range.h>
#include <future.h>
#include <task.h>
#include <option.h>
#include <party.h>
#include <string.h>
#include <stdio.h>
#include <stdarg.h>
#include <dtrace_enabled.h>
#include <dtrace_encore.h>


#define UNIT ((void*) -1)


////////////////////
// Shared messages


pony_msg_t m_MSG_alloc;


pony_msg_t m_resume_get;


pony_msg_t m_resume_suspend;


pony_msg_t m_resume_await;


pony_msg_t m_run_closure;


//////////////////
// Embedded code



























#include <string.h>
#include <strings.h>
#include <alloca.h>
#include <ctype.h>
#include <math.h>
array_t *_init_argv(pony_ctx_t** ctx, size_t argc, char **argv);


/////////////////////
// Class type decls


typedef struct _enc__class__Ped_util_Agent_passive_Agent_t _enc__class__Ped_util_Agent_passive_Agent_t;


typedef struct _enc__class__Ped_util_IO_File_t _enc__class__Ped_util_IO_File_t;


typedef struct _enc__class__Ped_util_Quad_tree_Quad_tree_t _enc__class__Ped_util_Quad_tree_Quad_tree_t;


typedef struct _enc__class__Ped_util_Quad_tree_Quad_t _enc__class__Ped_util_Quad_tree_Quad_t;


typedef struct _enc__class__Ped_util_Regions_Item_t _enc__class__Ped_util_Regions_Item_t;


typedef struct _enc__class__Ped_util_Regions_Box_t _enc__class__Ped_util_Regions_Box_t;


typedef struct _enc__class__Ped_util_Regions_Tiling_box_t _enc__class__Ped_util_Regions_Tiling_box_t;


typedef struct _enc__class__Ped_util_XML_XML_lib_t _enc__class__Ped_util_XML_XML_lib_t;


typedef struct _enc__class__Ped_util_XML_XML_node_t _enc__class__Ped_util_XML_XML_node_t;


typedef struct _enc__class__parallel_small_Main_t _enc__class__parallel_small_Main_t;


typedef struct _enc__class_String_String_t _enc__class_String_String_t;


/////////////////////
// Trait type decls


typedef struct _enc__trait_Std_Eq_t _enc__trait_Std_Eq_t;


typedef struct _enc__trait_Std_Id_t _enc__trait_Std_Id_t;


////////////////////////
// Passive class types


struct _enc__class__Ped_util_Agent_passive_Agent_t
{
  pony_type_t* _enc__self_type;
  int64_t _enc__field_ttl;
  int64_t _enc__field_i;
  array_t* _enc__field_targets_size;
  array_t* _enc__field_targets_y;
  array_t* _enc__field_targets_x;
  int64_t _enc__field_y;
  int64_t _enc__field_x;
  uint64_t _enc__field_hash_code;
};


struct _enc__class__Ped_util_IO_File_t
{
  pony_type_t* _enc__self_type;
  _enc__class_String_String_t* _enc__field_mode;
  _enc__class_String_String_t* _enc__field_fname;
  FILE* _enc__field_file;
};


struct _enc__class__Ped_util_Quad_tree_Quad_tree_t
{
  pony_type_t* _enc__self_type;
  int64_t _enc__field_ymin;
  int64_t _enc__field_xmin;
  int64_t _enc__field_ymax;
  int64_t _enc__field_xmax;
  _enc__class__Ped_util_Quad_tree_Quad_t* _enc__field_tree;
};


struct _enc__class__Ped_util_Quad_tree_Quad_t
{
  pony_type_t* _enc__self_type;
  int64_t _enc__field_size;
  int64_t _enc__field_min_y;
  int64_t _enc__field_min_x;
  int64_t _enc__field_max_y;
  int64_t _enc__field_max_x;
  option_t* _enc__field_bottom_left;
  option_t* _enc__field_bottom_right;
  option_t* _enc__field_top_left;
  option_t* _enc__field_top_right;
  array_t* _enc__field_agent;
};


struct _enc__class__Ped_util_Regions_Item_t
{
  pony_type_t* _enc__self_type;
  option_t* _enc__field_prev;
  option_t* _enc__field_next;
  _enc__class__Ped_util_Agent_passive_Agent_t* _enc__field_a;
};


struct _enc__class__Ped_util_XML_XML_lib_t
{
  pony_type_t* _enc__self_type;
};


struct _enc__class__Ped_util_XML_XML_node_t
{
  pony_type_t* _enc__self_type;
  array_t* _enc__field_atribs;
  _enc__class_String_String_t* _enc__field_name;
  array_t* _enc__field_comments;
  array_t* _enc__field_children;
  _enc__class_String_String_t* _enc__field_s_rep;
};


struct _enc__class_String_String_t
{
  pony_type_t* _enc__self_type;
  int64_t _enc__field_length;
  char* _enc__field_data;
};


//////////////////
// Runtime types


extern pony_type_t _enc__class__Ped_util_Agent_passive_Agent_type;


extern pony_type_t _enc__class__Ped_util_IO_File_type;


extern pony_type_t _enc__class__Ped_util_Quad_tree_Quad_tree_type;


extern pony_type_t _enc__class__Ped_util_Quad_tree_Quad_type;


extern pony_type_t _enc__class__Ped_util_Regions_Item_type;


extern pony_type_t _enc__class__Ped_util_Regions_Box_type;


extern pony_type_t _enc__class__Ped_util_Regions_Tiling_box_type;


extern pony_type_t _enc__class__Ped_util_XML_XML_lib_type;


extern pony_type_t _enc__class__Ped_util_XML_XML_node_type;


extern pony_type_t _enc__class__parallel_small_Main_type;


extern pony_type_t _enc__class_String_String_type;


extern pony_type_t _enc__trait_Std_Eq_type;


extern pony_type_t _enc__trait_Std_Id_type;


////////////////
// Message IDs


enum
{
  _MSG_DUMMY__ = 1024,
  _ENC__FUT_MSG__Ped_util_Agent_passive_Agent_pos,
  _ENC__FUT_MSG__Ped_util_Agent_passive_Agent_move_int,
  _ENC__FUT_MSG__Ped_util_Agent_passive_Agent_next,
  _ENC__FUT_MSG__Ped_util_Agent_passive_Agent_init,
  _ENC__FUT_MSG__Ped_util_IO_File_eof,
  _ENC__FUT_MSG__Ped_util_IO_File_readlineChar,
  _ENC__FUT_MSG__Ped_util_IO_File_readline,
  _ENC__FUT_MSG__Ped_util_IO_File_writeChar,
  _ENC__FUT_MSG__Ped_util_IO_File_write,
  _ENC__FUT_MSG__Ped_util_IO_File_close,
  _ENC__FUT_MSG__Ped_util_IO_File_open,
  _ENC__FUT_MSG__Ped_util_IO_File_init,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_tree_size,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_tree_remove,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_tree_set,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_tree_get_val,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_tree_init,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_recur,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_same,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_isin,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_remove,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_add,
  _ENC__FUT_MSG__Ped_util_Quad_tree_Quad_init,
  _ENC__FUT_MSG__Ped_util_Regions_Item_append,
  _ENC__FUT_MSG__Ped_util_Regions_Item_delete,
  _ENC__FUT_MSG__Ped_util_Regions_Item_init,
  _ENC__FUT_MSG__Ped_util_Regions_Box_await,
  _ENC__FUT_MSG__Ped_util_Regions_Box_suspend,
  _ENC__FUT_MSG__Ped_util_Regions_Box_move,
  _ENC__FUT_MSG__Ped_util_Regions_Box_is_something,
  _ENC__FUT_MSG__Ped_util_Regions_Box_merge,
  _ENC__FUT_MSG__Ped_util_Regions_Box_link,
  _ENC__FUT_MSG__Ped_util_Regions_Box_add_internal,
  _ENC__FUT_MSG__Ped_util_Regions_Box_external_move,
  _ENC__FUT_MSG__Ped_util_Regions_Box_add,
  _ENC__FUT_MSG__Ped_util_Regions_Box_agents,
  _ENC__FUT_MSG__Ped_util_Regions_Box_max,
  _ENC__FUT_MSG__Ped_util_Regions_Box_min,
  _ENC__FUT_MSG__Ped_util_Regions_Box_init,
  _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_await,
  _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_suspend,
  _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_agents,
  _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_move,
  _ENC__FUT_MSG__Ped_util_Regions_Tiling_box_init,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_file_to_xml,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_pair,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_or_find_from,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_ext_atribs,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_tag_type,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_new_XML_node,
  _ENC__FUT_MSG__Ped_util_XML_XML_lib_init,
  _ENC__FUT_MSG__Ped_util_XML_XML_node_attribute_value,
  _ENC__FUT_MSG__Ped_util_XML_XML_node_children_named,
  _ENC__FUT_MSG__Ped_util_XML_XML_node_init,
  _ENC__FUT_MSG__parallel_small_Main_init,
  _ENC__FUT_MSG__parallel_small_Main_await,
  _ENC__FUT_MSG__parallel_small_Main_suspend,
  _ENC__FUT_MSG__parallel_small_Main_main,
  _ENC__FUT_MSG_String_String_to_int,
  _ENC__FUT_MSG_String_String_split,
  _ENC__FUT_MSG_String_String_to_array,
  _ENC__FUT_MSG_String_String_char_at,
  _ENC__FUT_MSG_String_String_format,
  _ENC__FUT_MSG_String_String_delete,
  _ENC__FUT_MSG_String_String_find_from,
  _ENC__FUT_MSG_String_String_find,
  _ENC__FUT_MSG_String_String_replace,
  _ENC__FUT_MSG_String_String_trim,
  _ENC__FUT_MSG_String_String_getData,
  _ENC__FUT_MSG_String_String_join,
  _ENC__FUT_MSG_String_String_occurrences,
  _ENC__FUT_MSG_String_String_eq,
  _ENC__FUT_MSG_String_String_equals,
  _ENC__FUT_MSG_String_String_substring,
  _ENC__FUT_MSG_String_String_size,
  _ENC__FUT_MSG_String_String_length,
  _ENC__FUT_MSG_String_String_to_lower,
  _ENC__FUT_MSG_String_String_to_upper,
  _ENC__FUT_MSG_String_String_compare_ignore_case,
  _ENC__FUT_MSG_String_String_compare,
  _ENC__FUT_MSG_String_String_contains_ignore_case,
  _ENC__FUT_MSG_String_String_contains,
  _ENC__FUT_MSG_String_String_copy,
  _ENC__FUT_MSG_String_String_concatenate,
  _ENC__FUT_MSG_String_String_is_empty,
  _ENC__FUT_MSG_String_String_init,
  _ENC__ONEWAY_MSG__Ped_util_Agent_passive_Agent_pos,
  _ENC__ONEWAY_MSG__Ped_util_Agent_passive_Agent_move_int,
  _ENC__ONEWAY_MSG__Ped_util_Agent_passive_Agent_next,
  _ENC__ONEWAY_MSG__Ped_util_Agent_passive_Agent_init,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_eof,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_readlineChar,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_readline,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_writeChar,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_write,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_close,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_open,
  _ENC__ONEWAY_MSG__Ped_util_IO_File_init,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_tree_size,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_tree_remove,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_tree_set,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_tree_get_val,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_tree_init,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_recur,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_same,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_isin,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_remove,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_add,
  _ENC__ONEWAY_MSG__Ped_util_Quad_tree_Quad_init,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Item_append,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Item_delete,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Item_init,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_await,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_suspend,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_move,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_is_something,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_merge,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_link,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_add_internal,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_external_move,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_add,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_agents,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_max,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_min,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Box_init,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_await,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_suspend,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_agents,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_move,
  _ENC__ONEWAY_MSG__Ped_util_Regions_Tiling_box_init,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_file_to_xml,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_pair,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_or_find_from,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_ext_atribs,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_tag_type,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_new_XML_node,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_lib_init,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_node_attribute_value,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_node_children_named,
  _ENC__ONEWAY_MSG__Ped_util_XML_XML_node_init,
  _ENC__ONEWAY_MSG__parallel_small_Main_init,
  _ENC__ONEWAY_MSG__parallel_small_Main_await,
  _ENC__ONEWAY_MSG__parallel_small_Main_suspend,
  _ENC__ONEWAY_MSG__parallel_small_Main_main,
  _ENC__ONEWAY_MSG_String_String_to_int,
  _ENC__ONEWAY_MSG_String_String_split,
  _ENC__ONEWAY_MSG_String_String_to_array,
  _ENC__ONEWAY_MSG_String_String_char_at,
  _ENC__ONEWAY_MSG_String_String_format,
  _ENC__ONEWAY_MSG_String_String_delete,
  _ENC__ONEWAY_MSG_String_String_find_from,
  _ENC__ONEWAY_MSG_String_String_find,
  _ENC__ONEWAY_MSG_String_String_replace,
  _ENC__ONEWAY_MSG_String_String_trim,
  _ENC__ONEWAY_MSG_String_String_getData,
  _ENC__ONEWAY_MSG_String_String_join,
  _ENC__ONEWAY_MSG_String_String_occurrences,
  _ENC__ONEWAY_MSG_String_String_eq,
  _ENC__ONEWAY_MSG_String_String_equals,
  _ENC__ONEWAY_MSG_String_String_substring,
  _ENC__ONEWAY_MSG_String_String_size,
  _ENC__ONEWAY_MSG_String_String_length,
  _ENC__ONEWAY_MSG_String_String_to_lower,
  _ENC__ONEWAY_MSG_String_String_to_upper,
  _ENC__ONEWAY_MSG_String_String_compare_ignore_case,
  _ENC__ONEWAY_MSG_String_String_compare,
  _ENC__ONEWAY_MSG_String_String_contains_ignore_case,
  _ENC__ONEWAY_MSG_String_String_contains,
  _ENC__ONEWAY_MSG_String_String_copy,
  _ENC__ONEWAY_MSG_String_String_concatenate,
  _ENC__ONEWAY_MSG_String_String_is_empty,
  _ENC__ONEWAY_MSG_String_String_init,
};


//////////////////
// Message types


typedef struct _enc__fut_msg__Ped_util_Agent_passive_Agent_pos_t _enc__fut_msg__Ped_util_Agent_passive_Agent_pos_t;


typedef struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_pos_t _enc__oneway_msg__Ped_util_Agent_passive_Agent_pos_t;


typedef struct _enc__fut_msg__Ped_util_Agent_passive_Agent_move_int_t _enc__fut_msg__Ped_util_Agent_passive_Agent_move_int_t;


typedef struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_move_int_t _enc__oneway_msg__Ped_util_Agent_passive_Agent_move_int_t;


typedef struct _enc__fut_msg__Ped_util_Agent_passive_Agent_next_t _enc__fut_msg__Ped_util_Agent_passive_Agent_next_t;


typedef struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_next_t _enc__oneway_msg__Ped_util_Agent_passive_Agent_next_t;


typedef struct _enc__fut_msg__Ped_util_Agent_passive_Agent_init_t _enc__fut_msg__Ped_util_Agent_passive_Agent_init_t;


typedef struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_init_t _enc__oneway_msg__Ped_util_Agent_passive_Agent_init_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_eof_t _enc__fut_msg__Ped_util_IO_File_eof_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_eof_t _enc__oneway_msg__Ped_util_IO_File_eof_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_readlineChar_t _enc__fut_msg__Ped_util_IO_File_readlineChar_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_readlineChar_t _enc__oneway_msg__Ped_util_IO_File_readlineChar_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_readline_t _enc__fut_msg__Ped_util_IO_File_readline_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_readline_t _enc__oneway_msg__Ped_util_IO_File_readline_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_writeChar_t _enc__fut_msg__Ped_util_IO_File_writeChar_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_writeChar_t _enc__oneway_msg__Ped_util_IO_File_writeChar_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_write_t _enc__fut_msg__Ped_util_IO_File_write_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_write_t _enc__oneway_msg__Ped_util_IO_File_write_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_close_t _enc__fut_msg__Ped_util_IO_File_close_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_close_t _enc__oneway_msg__Ped_util_IO_File_close_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_open_t _enc__fut_msg__Ped_util_IO_File_open_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_open_t _enc__oneway_msg__Ped_util_IO_File_open_t;


typedef struct _enc__fut_msg__Ped_util_IO_File_init_t _enc__fut_msg__Ped_util_IO_File_init_t;


typedef struct _enc__oneway_msg__Ped_util_IO_File_init_t _enc__oneway_msg__Ped_util_IO_File_init_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_size_t _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_size_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_size_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_size_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_remove_t _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_remove_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_remove_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_remove_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_set_t _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_set_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_set_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_set_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_get_val_t _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_get_val_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_get_val_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_get_val_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_init_t _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_init_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_init_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_init_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_recur_t _enc__fut_msg__Ped_util_Quad_tree_Quad_recur_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_recur_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_recur_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_same_t _enc__fut_msg__Ped_util_Quad_tree_Quad_same_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_same_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_same_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_isin_t _enc__fut_msg__Ped_util_Quad_tree_Quad_isin_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_isin_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_isin_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_remove_t _enc__fut_msg__Ped_util_Quad_tree_Quad_remove_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_remove_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_remove_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_add_t _enc__fut_msg__Ped_util_Quad_tree_Quad_add_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_add_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_add_t;


typedef struct _enc__fut_msg__Ped_util_Quad_tree_Quad_init_t _enc__fut_msg__Ped_util_Quad_tree_Quad_init_t;


typedef struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_init_t _enc__oneway_msg__Ped_util_Quad_tree_Quad_init_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Item_append_t _enc__fut_msg__Ped_util_Regions_Item_append_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Item_append_t _enc__oneway_msg__Ped_util_Regions_Item_append_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Item_delete_t _enc__fut_msg__Ped_util_Regions_Item_delete_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Item_delete_t _enc__oneway_msg__Ped_util_Regions_Item_delete_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Item_init_t _enc__fut_msg__Ped_util_Regions_Item_init_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Item_init_t _enc__oneway_msg__Ped_util_Regions_Item_init_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_await_t _enc__fut_msg__Ped_util_Regions_Box_await_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_await_t _enc__oneway_msg__Ped_util_Regions_Box_await_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_suspend_t _enc__fut_msg__Ped_util_Regions_Box_suspend_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_suspend_t _enc__oneway_msg__Ped_util_Regions_Box_suspend_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_move_t _enc__fut_msg__Ped_util_Regions_Box_move_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_move_t _enc__oneway_msg__Ped_util_Regions_Box_move_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_is_something_t _enc__fut_msg__Ped_util_Regions_Box_is_something_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_is_something_t _enc__oneway_msg__Ped_util_Regions_Box_is_something_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_merge_t _enc__fut_msg__Ped_util_Regions_Box_merge_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_merge_t _enc__oneway_msg__Ped_util_Regions_Box_merge_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_link_t _enc__fut_msg__Ped_util_Regions_Box_link_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_link_t _enc__oneway_msg__Ped_util_Regions_Box_link_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_add_internal_t _enc__fut_msg__Ped_util_Regions_Box_add_internal_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_add_internal_t _enc__oneway_msg__Ped_util_Regions_Box_add_internal_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_external_move_t _enc__fut_msg__Ped_util_Regions_Box_external_move_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_external_move_t _enc__oneway_msg__Ped_util_Regions_Box_external_move_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_add_t _enc__fut_msg__Ped_util_Regions_Box_add_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_add_t _enc__oneway_msg__Ped_util_Regions_Box_add_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_agents_t _enc__fut_msg__Ped_util_Regions_Box_agents_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_agents_t _enc__oneway_msg__Ped_util_Regions_Box_agents_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_max_t _enc__fut_msg__Ped_util_Regions_Box_max_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_max_t _enc__oneway_msg__Ped_util_Regions_Box_max_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_min_t _enc__fut_msg__Ped_util_Regions_Box_min_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_min_t _enc__oneway_msg__Ped_util_Regions_Box_min_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Box_init_t _enc__fut_msg__Ped_util_Regions_Box_init_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Box_init_t _enc__oneway_msg__Ped_util_Regions_Box_init_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Tiling_box_await_t _enc__fut_msg__Ped_util_Regions_Tiling_box_await_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t _enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t _enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_suspend_t _enc__oneway_msg__Ped_util_Regions_Tiling_box_suspend_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t _enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_agents_t _enc__oneway_msg__Ped_util_Regions_Tiling_box_agents_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Tiling_box_move_t _enc__fut_msg__Ped_util_Regions_Tiling_box_move_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t _enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t;


typedef struct _enc__fut_msg__Ped_util_Regions_Tiling_box_init_t _enc__fut_msg__Ped_util_Regions_Tiling_box_init_t;


typedef struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t _enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_file_to_xml_t _enc__fut_msg__Ped_util_XML_XML_lib_file_to_xml_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_file_to_xml_t _enc__oneway_msg__Ped_util_XML_XML_lib_file_to_xml_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_pair_t _enc__fut_msg__Ped_util_XML_XML_lib_pair_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_pair_t _enc__oneway_msg__Ped_util_XML_XML_lib_pair_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_or_find_from_t _enc__fut_msg__Ped_util_XML_XML_lib_or_find_from_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_or_find_from_t _enc__oneway_msg__Ped_util_XML_XML_lib_or_find_from_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_ext_atribs_t _enc__fut_msg__Ped_util_XML_XML_lib_ext_atribs_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_ext_atribs_t _enc__oneway_msg__Ped_util_XML_XML_lib_ext_atribs_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_tag_type_t _enc__fut_msg__Ped_util_XML_XML_lib_tag_type_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_tag_type_t _enc__oneway_msg__Ped_util_XML_XML_lib_tag_type_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_new_XML_node_t _enc__fut_msg__Ped_util_XML_XML_lib_new_XML_node_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_new_XML_node_t _enc__oneway_msg__Ped_util_XML_XML_lib_new_XML_node_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_lib_init_t _enc__fut_msg__Ped_util_XML_XML_lib_init_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_lib_init_t _enc__oneway_msg__Ped_util_XML_XML_lib_init_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_node_attribute_value_t _enc__fut_msg__Ped_util_XML_XML_node_attribute_value_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_node_attribute_value_t _enc__oneway_msg__Ped_util_XML_XML_node_attribute_value_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_node_children_named_t _enc__fut_msg__Ped_util_XML_XML_node_children_named_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_node_children_named_t _enc__oneway_msg__Ped_util_XML_XML_node_children_named_t;


typedef struct _enc__fut_msg__Ped_util_XML_XML_node_init_t _enc__fut_msg__Ped_util_XML_XML_node_init_t;


typedef struct _enc__oneway_msg__Ped_util_XML_XML_node_init_t _enc__oneway_msg__Ped_util_XML_XML_node_init_t;


typedef struct _enc__fut_msg__parallel_small_Main_init_t _enc__fut_msg__parallel_small_Main_init_t;


typedef struct _enc__oneway_msg__parallel_small_Main_init_t _enc__oneway_msg__parallel_small_Main_init_t;


typedef struct _enc__fut_msg__parallel_small_Main_await_t _enc__fut_msg__parallel_small_Main_await_t;


typedef struct _enc__oneway_msg__parallel_small_Main_await_t _enc__oneway_msg__parallel_small_Main_await_t;


typedef struct _enc__fut_msg__parallel_small_Main_suspend_t _enc__fut_msg__parallel_small_Main_suspend_t;


typedef struct _enc__oneway_msg__parallel_small_Main_suspend_t _enc__oneway_msg__parallel_small_Main_suspend_t;


typedef struct _enc__fut_msg__parallel_small_Main_main_t _enc__fut_msg__parallel_small_Main_main_t;


typedef struct _enc__oneway_msg__parallel_small_Main_main_t _enc__oneway_msg__parallel_small_Main_main_t;


typedef struct _enc__fut_msg_String_String_to_int_t _enc__fut_msg_String_String_to_int_t;


typedef struct _enc__oneway_msg_String_String_to_int_t _enc__oneway_msg_String_String_to_int_t;


typedef struct _enc__fut_msg_String_String_split_t _enc__fut_msg_String_String_split_t;


typedef struct _enc__oneway_msg_String_String_split_t _enc__oneway_msg_String_String_split_t;


typedef struct _enc__fut_msg_String_String_to_array_t _enc__fut_msg_String_String_to_array_t;


typedef struct _enc__oneway_msg_String_String_to_array_t _enc__oneway_msg_String_String_to_array_t;


typedef struct _enc__fut_msg_String_String_char_at_t _enc__fut_msg_String_String_char_at_t;


typedef struct _enc__oneway_msg_String_String_char_at_t _enc__oneway_msg_String_String_char_at_t;


typedef struct _enc__fut_msg_String_String_format_t _enc__fut_msg_String_String_format_t;


typedef struct _enc__oneway_msg_String_String_format_t _enc__oneway_msg_String_String_format_t;


typedef struct _enc__fut_msg_String_String_delete_t _enc__fut_msg_String_String_delete_t;


typedef struct _enc__oneway_msg_String_String_delete_t _enc__oneway_msg_String_String_delete_t;


typedef struct _enc__fut_msg_String_String_find_from_t _enc__fut_msg_String_String_find_from_t;


typedef struct _enc__oneway_msg_String_String_find_from_t _enc__oneway_msg_String_String_find_from_t;


typedef struct _enc__fut_msg_String_String_find_t _enc__fut_msg_String_String_find_t;


typedef struct _enc__oneway_msg_String_String_find_t _enc__oneway_msg_String_String_find_t;


typedef struct _enc__fut_msg_String_String_replace_t _enc__fut_msg_String_String_replace_t;


typedef struct _enc__oneway_msg_String_String_replace_t _enc__oneway_msg_String_String_replace_t;


typedef struct _enc__fut_msg_String_String_trim_t _enc__fut_msg_String_String_trim_t;


typedef struct _enc__oneway_msg_String_String_trim_t _enc__oneway_msg_String_String_trim_t;


typedef struct _enc__fut_msg_String_String_getData_t _enc__fut_msg_String_String_getData_t;


typedef struct _enc__oneway_msg_String_String_getData_t _enc__oneway_msg_String_String_getData_t;


typedef struct _enc__fut_msg_String_String_join_t _enc__fut_msg_String_String_join_t;


typedef struct _enc__oneway_msg_String_String_join_t _enc__oneway_msg_String_String_join_t;


typedef struct _enc__fut_msg_String_String_occurrences_t _enc__fut_msg_String_String_occurrences_t;


typedef struct _enc__oneway_msg_String_String_occurrences_t _enc__oneway_msg_String_String_occurrences_t;


typedef struct _enc__fut_msg_String_String_eq_t _enc__fut_msg_String_String_eq_t;


typedef struct _enc__oneway_msg_String_String_eq_t _enc__oneway_msg_String_String_eq_t;


typedef struct _enc__fut_msg_String_String_equals_t _enc__fut_msg_String_String_equals_t;


typedef struct _enc__oneway_msg_String_String_equals_t _enc__oneway_msg_String_String_equals_t;


typedef struct _enc__fut_msg_String_String_substring_t _enc__fut_msg_String_String_substring_t;


typedef struct _enc__oneway_msg_String_String_substring_t _enc__oneway_msg_String_String_substring_t;


typedef struct _enc__fut_msg_String_String_size_t _enc__fut_msg_String_String_size_t;


typedef struct _enc__oneway_msg_String_String_size_t _enc__oneway_msg_String_String_size_t;


typedef struct _enc__fut_msg_String_String_length_t _enc__fut_msg_String_String_length_t;


typedef struct _enc__oneway_msg_String_String_length_t _enc__oneway_msg_String_String_length_t;


typedef struct _enc__fut_msg_String_String_to_lower_t _enc__fut_msg_String_String_to_lower_t;


typedef struct _enc__oneway_msg_String_String_to_lower_t _enc__oneway_msg_String_String_to_lower_t;


typedef struct _enc__fut_msg_String_String_to_upper_t _enc__fut_msg_String_String_to_upper_t;


typedef struct _enc__oneway_msg_String_String_to_upper_t _enc__oneway_msg_String_String_to_upper_t;


typedef struct _enc__fut_msg_String_String_compare_ignore_case_t _enc__fut_msg_String_String_compare_ignore_case_t;


typedef struct _enc__oneway_msg_String_String_compare_ignore_case_t _enc__oneway_msg_String_String_compare_ignore_case_t;


typedef struct _enc__fut_msg_String_String_compare_t _enc__fut_msg_String_String_compare_t;


typedef struct _enc__oneway_msg_String_String_compare_t _enc__oneway_msg_String_String_compare_t;


typedef struct _enc__fut_msg_String_String_contains_ignore_case_t _enc__fut_msg_String_String_contains_ignore_case_t;


typedef struct _enc__oneway_msg_String_String_contains_ignore_case_t _enc__oneway_msg_String_String_contains_ignore_case_t;


typedef struct _enc__fut_msg_String_String_contains_t _enc__fut_msg_String_String_contains_t;


typedef struct _enc__oneway_msg_String_String_contains_t _enc__oneway_msg_String_String_contains_t;


typedef struct _enc__fut_msg_String_String_copy_t _enc__fut_msg_String_String_copy_t;


typedef struct _enc__oneway_msg_String_String_copy_t _enc__oneway_msg_String_String_copy_t;


typedef struct _enc__fut_msg_String_String_concatenate_t _enc__fut_msg_String_String_concatenate_t;


typedef struct _enc__oneway_msg_String_String_concatenate_t _enc__oneway_msg_String_String_concatenate_t;


typedef struct _enc__fut_msg_String_String_is_empty_t _enc__fut_msg_String_String_is_empty_t;


typedef struct _enc__oneway_msg_String_String_is_empty_t _enc__oneway_msg_String_String_is_empty_t;


typedef struct _enc__fut_msg_String_String_init_t _enc__fut_msg_String_String_init_t;


typedef struct _enc__oneway_msg_String_String_init_t _enc__oneway_msg_String_String_init_t;


struct _enc__fut_msg__Ped_util_Agent_passive_Agent_pos_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_pos_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Agent_passive_Agent_move_int_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_move_int_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Agent_passive_Agent_next_t
{
  encore_fut_msg_t ;
  array_t* f1 /* ret */;
};


struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_next_t
{
  encore_oneway_msg_t msg;
  array_t* f1 /* ret */;
};


struct _enc__fut_msg__Ped_util_Agent_passive_Agent_init_t
{
  encore_fut_msg_t ;
  tuple_t* f1 /* in_pos */;
  array_t* f2 /* list */;
  array_t* f3 /* targets_size */;
  int64_t f4 /* in_id */;
  int64_t f5 /* ttl */;
};


struct _enc__oneway_msg__Ped_util_Agent_passive_Agent_init_t
{
  encore_oneway_msg_t msg;
  tuple_t* f1 /* in_pos */;
  array_t* f2 /* list */;
  array_t* f3 /* targets_size */;
  int64_t f4 /* in_id */;
  int64_t f5 /* ttl */;
};


struct _enc__fut_msg__Ped_util_IO_File_eof_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_IO_File_eof_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_IO_File_readlineChar_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_IO_File_readlineChar_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_IO_File_readline_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_IO_File_readline_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_IO_File_writeChar_t
{
  encore_fut_msg_t ;
  char* f1 /* content */;
};


struct _enc__oneway_msg__Ped_util_IO_File_writeChar_t
{
  encore_oneway_msg_t msg;
  char* f1 /* content */;
};


struct _enc__fut_msg__Ped_util_IO_File_write_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* content */;
};


struct _enc__oneway_msg__Ped_util_IO_File_write_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* content */;
};


struct _enc__fut_msg__Ped_util_IO_File_close_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_IO_File_close_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_IO_File_open_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* fin */;
  _enc__class_String_String_t* f2 /* mode */;
};


struct _enc__oneway_msg__Ped_util_IO_File_open_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* fin */;
  _enc__class_String_String_t* f2 /* mode */;
};


struct _enc__fut_msg__Ped_util_IO_File_init_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* fname */;
  _enc__class_String_String_t* f2 /* mode */;
};


struct _enc__oneway_msg__Ped_util_IO_File_init_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* fname */;
  _enc__class_String_String_t* f2 /* mode */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_size_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_size_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_remove_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_remove_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_set_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_set_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_get_val_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_get_val_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_tree_init_t
{
  encore_fut_msg_t ;
  tuple_t* f1 /* in_max */;
  tuple_t* f2 /* in_min */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_tree_init_t
{
  encore_oneway_msg_t msg;
  tuple_t* f1 /* in_max */;
  tuple_t* f2 /* in_min */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_recur_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_recur_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_same_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_same_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_isin_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_isin_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_remove_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_remove_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_add_t
{
  encore_fut_msg_t ;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_add_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* x */;
  int64_t f2 /* y */;
};


struct _enc__fut_msg__Ped_util_Quad_tree_Quad_init_t
{
  encore_fut_msg_t ;
  int64_t f1 /* max_x */;
  int64_t f2 /* max_y */;
  int64_t f3 /* min_x */;
  int64_t f4 /* min_y */;
};


struct _enc__oneway_msg__Ped_util_Quad_tree_Quad_init_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* max_x */;
  int64_t f2 /* max_y */;
  int64_t f3 /* min_x */;
  int64_t f4 /* min_y */;
};


struct _enc__fut_msg__Ped_util_Regions_Item_append_t
{
  encore_fut_msg_t ;
  option_t* f1 /* next */;
};


struct _enc__oneway_msg__Ped_util_Regions_Item_append_t
{
  encore_oneway_msg_t msg;
  option_t* f1 /* next */;
};


struct _enc__fut_msg__Ped_util_Regions_Item_delete_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Item_delete_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Item_init_t
{
  encore_fut_msg_t ;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
};


struct _enc__oneway_msg__Ped_util_Regions_Item_init_t
{
  encore_oneway_msg_t msg;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_await_t
{
  encore_fut_msg_t ;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_await_t
{
  encore_oneway_msg_t msg;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_suspend_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_suspend_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Box_move_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_move_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Box_is_something_t
{
  encore_fut_msg_t ;
  option_t* f1 /* a */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_is_something_t
{
  encore_oneway_msg_t msg;
  option_t* f1 /* a */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_merge_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_merge_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Box_link_t
{
  encore_fut_msg_t ;
  _enc__class__Ped_util_Regions_Box_t* f1 /* a */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_link_t
{
  encore_oneway_msg_t msg;
  _enc__class__Ped_util_Regions_Box_t* f1 /* a */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_add_internal_t
{
  encore_fut_msg_t ;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
  int64_t f2 /* x */;
  int64_t f3 /* y */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_add_internal_t
{
  encore_oneway_msg_t msg;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
  int64_t f2 /* x */;
  int64_t f3 /* y */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_external_move_t
{
  encore_fut_msg_t ;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
  int64_t f2 /* x */;
  int64_t f3 /* y */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_external_move_t
{
  encore_oneway_msg_t msg;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
  int64_t f2 /* x */;
  int64_t f3 /* y */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_add_t
{
  encore_fut_msg_t ;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_add_t
{
  encore_oneway_msg_t msg;
  _enc__class__Ped_util_Agent_passive_Agent_t* f1 /* a */;
};


struct _enc__fut_msg__Ped_util_Regions_Box_agents_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_agents_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Box_max_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_max_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Box_min_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_min_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Box_init_t
{
  encore_fut_msg_t ;
  tuple_t* f1 /* in_max */;
  tuple_t* f2 /* in_min */;
};


struct _enc__oneway_msg__Ped_util_Regions_Box_init_t
{
  encore_oneway_msg_t msg;
  tuple_t* f1 /* in_max */;
  tuple_t* f2 /* in_min */;
};


struct _enc__fut_msg__Ped_util_Regions_Tiling_box_await_t
{
  encore_fut_msg_t ;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_await_t
{
  encore_oneway_msg_t msg;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__fut_msg__Ped_util_Regions_Tiling_box_suspend_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_suspend_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Tiling_box_agents_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_agents_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_Regions_Tiling_box_move_t
{
  encore_fut_msg_t ;
  int64_t f1 /* i */;
};


struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_move_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* i */;
};


struct _enc__fut_msg__Ped_util_Regions_Tiling_box_init_t
{
  encore_fut_msg_t ;
  array_t* f1 /* agents */;
  int64_t f2 /* n */;
};


struct _enc__oneway_msg__Ped_util_Regions_Tiling_box_init_t
{
  encore_oneway_msg_t msg;
  array_t* f1 /* agents */;
  int64_t f2 /* n */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_file_to_xml_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* fname */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_file_to_xml_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* fname */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_pair_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* head */;
  _enc__class_String_String_t* f2 /* tail */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_pair_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* head */;
  _enc__class_String_String_t* f2 /* tail */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_or_find_from_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* a */;
  _enc__class_String_String_t* f2 /* b */;
  int64_t f3 /* i */;
  _enc__class_String_String_t* f4 /* s */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_or_find_from_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* a */;
  _enc__class_String_String_t* f2 /* b */;
  int64_t f3 /* i */;
  _enc__class_String_String_t* f4 /* s */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_ext_atribs_t
{
  encore_fut_msg_t ;
  _enc__class__Ped_util_XML_XML_node_t* f1 /* node */;
  _enc__class_String_String_t* f2 /* s */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_ext_atribs_t
{
  encore_oneway_msg_t msg;
  _enc__class__Ped_util_XML_XML_node_t* f1 /* node */;
  _enc__class_String_String_t* f2 /* s */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_tag_type_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_tag_type_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_new_XML_node_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_new_XML_node_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__fut_msg__Ped_util_XML_XML_lib_init_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_XML_XML_lib_init_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__Ped_util_XML_XML_node_attribute_value_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* name */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_node_attribute_value_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* name */;
};


struct _enc__fut_msg__Ped_util_XML_XML_node_children_named_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* name */;
};


struct _enc__oneway_msg__Ped_util_XML_XML_node_children_named_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* name */;
};


struct _enc__fut_msg__Ped_util_XML_XML_node_init_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__Ped_util_XML_XML_node_init_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__parallel_small_Main_init_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__parallel_small_Main_init_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__parallel_small_Main_await_t
{
  encore_fut_msg_t ;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__oneway_msg__parallel_small_Main_await_t
{
  encore_oneway_msg_t msg;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__fut_msg__parallel_small_Main_suspend_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__parallel_small_Main_suspend_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__parallel_small_Main_main_t
{
  encore_fut_msg_t ;
  array_t* f1 /* args */;
};


struct _enc__oneway_msg__parallel_small_Main_main_t
{
  encore_oneway_msg_t msg;
  array_t* f1 /* args */;
};


struct _enc__fut_msg_String_String_to_int_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_to_int_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_split_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* p */;
};


struct _enc__oneway_msg_String_String_split_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* p */;
};


struct _enc__fut_msg_String_String_to_array_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_to_array_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_char_at_t
{
  encore_fut_msg_t ;
  int64_t f1 /* i */;
};


struct _enc__oneway_msg_String_String_char_at_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* i */;
};


struct _enc__fut_msg_String_String_format_t
{
  encore_fut_msg_t ;
  array_t* f1 /* b */;
};


struct _enc__oneway_msg_String_String_format_t
{
  encore_oneway_msg_t msg;
  array_t* f1 /* b */;
};


struct _enc__fut_msg_String_String_delete_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__oneway_msg_String_String_delete_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__fut_msg_String_String_find_from_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* a */;
  int64_t f2 /* b */;
};


struct _enc__oneway_msg_String_String_find_from_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* a */;
  int64_t f2 /* b */;
};


struct _enc__fut_msg_String_String_find_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* a */;
};


struct _enc__oneway_msg_String_String_find_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* a */;
};


struct _enc__fut_msg_String_String_replace_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* a */;
  _enc__class_String_String_t* f2 /* b */;
};


struct _enc__oneway_msg_String_String_replace_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* a */;
  _enc__class_String_String_t* f2 /* b */;
};


struct _enc__fut_msg_String_String_trim_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_trim_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_getData_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_getData_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_join_t
{
  encore_fut_msg_t ;
  array_t* f1 /* strings */;
};


struct _enc__oneway_msg_String_String_join_t
{
  encore_oneway_msg_t msg;
  array_t* f1 /* strings */;
};


struct _enc__fut_msg_String_String_occurrences_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__oneway_msg_String_String_occurrences_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__fut_msg_String_String_eq_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__oneway_msg_String_String_eq_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__fut_msg_String_String_equals_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__oneway_msg_String_String_equals_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* s */;
};


struct _enc__fut_msg_String_String_substring_t
{
  encore_fut_msg_t ;
  int64_t f1 /* from */;
  int64_t f2 /* to */;
};


struct _enc__oneway_msg_String_String_substring_t
{
  encore_oneway_msg_t msg;
  int64_t f1 /* from */;
  int64_t f2 /* to */;
};


struct _enc__fut_msg_String_String_size_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_size_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_length_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_length_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_to_lower_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_to_lower_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_to_upper_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_to_upper_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_compare_ignore_case_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__oneway_msg_String_String_compare_ignore_case_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__fut_msg_String_String_compare_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__oneway_msg_String_String_compare_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__fut_msg_String_String_contains_ignore_case_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__oneway_msg_String_String_contains_ignore_case_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__fut_msg_String_String_contains_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__oneway_msg_String_String_contains_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__fut_msg_String_String_copy_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_copy_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_concatenate_t
{
  encore_fut_msg_t ;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__oneway_msg_String_String_concatenate_t
{
  encore_oneway_msg_t msg;
  _enc__class_String_String_t* f1 /* b */;
};


struct _enc__fut_msg_String_String_is_empty_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg_String_String_is_empty_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg_String_String_init_t
{
  encore_fut_msg_t ;
  char* f1 /* s */;
};


struct _enc__oneway_msg_String_String_init_t
{
  encore_oneway_msg_t msg;
  char* f1 /* s */;
};


/////////////////////
// Global functions


array_t* _enc__global_fun__Ped_util_Global_funsparse_file(pony_ctx_t**, pony_type_t**, _enc__class_String_String_t*);


double _enc__global_fun__Ped_util_Global_funsdistance_int(pony_ctx_t**, pony_type_t**, int64_t, int64_t, int64_t, int64_t);


int64_t _enc__global_fun__Ped_util_Global_funsround(pony_ctx_t**, pony_type_t**, double);


int64_t _enc__global_fun__Ped_util_Global_funsabs(pony_ctx_t**, pony_type_t**, int64_t);


double _enc__global_fun__Ped_util_Global_funssqrt(pony_ctx_t**, pony_type_t**, double);


_enc__class_String_String_t* _enc__global_fun__Ped_util_Global_funsyolo_string(pony_ctx_t**, pony_type_t**, option_t*);


int64_t _enc__global_fun__Ped_util_Global_funsyolo_int(pony_ctx_t**, pony_type_t**, option_t*);


int64_t _enc__global_fun__Ped_util_Global_funsmin(pony_ctx_t**, pony_type_t**, int64_t, int64_t);


int64_t _enc__global_fun__Ped_util_Global_funsmax(pony_ctx_t**, pony_type_t**, int64_t, int64_t);


array_t* _enc__global_fun__Ped_util_Global_funsflatten(pony_ctx_t**, pony_type_t**, array_t*);


array_t* _enc__global_fun__Ped_util_Global_funscopy_tr(pony_ctx_t**, pony_type_t**, array_t*);


array_t* _enc__global_fun__Ped_util_Global_funscopy_ar(pony_ctx_t**, pony_type_t**, array_t*);


tuple_t* _enc__global_fun__Ped_util_Global_funsextreme_check(pony_ctx_t**, pony_type_t**, tuple_t*, tuple_t*);


tuple_t* _enc__global_fun__Ped_util_Global_funsfind_extreme(pony_ctx_t**, pony_type_t**, array_t*);


double _enc__global_fun__Ped_util_Global_funsinv_sqrt(pony_ctx_t**, pony_type_t**, int64_t);


int64_t _enc__global_fun__Ped_util_Regionslink(pony_ctx_t**, pony_type_t**, _enc__class__Ped_util_Regions_Box_t*, _enc__class__Ped_util_Regions_Box_t*);


void* _enc__global_fun__Ped_util_Regionsregions(pony_ctx_t**, pony_type_t**, array_t*, int64_t);


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_bool(pony_ctx_t**, pony_type_t**, int64_t);


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_int(pony_ctx_t**, pony_type_t**, int64_t);


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_real(pony_ctx_t**, pony_type_t**, double);


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_array(pony_ctx_t**, pony_type_t**, array_t*);


_enc__class_String_String_t* _enc__global_fun_Stringstring_from_char(pony_ctx_t**, pony_type_t**, char);


value_t _enc__fun_wrapper__Ped_util_Global_funsparse_file(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsdistance_int(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsround(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsabs(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funssqrt(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsyolo_string(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsyolo_int(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsmin(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsmax(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsflatten(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funscopy_tr(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funscopy_ar(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsextreme_check(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsfind_extreme(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Global_funsinv_sqrt(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Regionslink(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper__Ped_util_Regionsregions(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper_Stringstring_from_bool(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper_Stringstring_from_int(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper_Stringstring_from_real(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper_Stringstring_from_array(pony_ctx_t**, pony_type_t**, value_t*, void*);


value_t _enc__fun_wrapper_Stringstring_from_char(pony_ctx_t**, pony_type_t**, value_t*, void*);


closure_t* _enc__closure__Ped_util_Global_funsparse_file;


closure_t* _enc__closure__Ped_util_Global_funsdistance_int;


closure_t* _enc__closure__Ped_util_Global_funsround;


closure_t* _enc__closure__Ped_util_Global_funsabs;


closure_t* _enc__closure__Ped_util_Global_funssqrt;


closure_t* _enc__closure__Ped_util_Global_funsyolo_string;


closure_t* _enc__closure__Ped_util_Global_funsyolo_int;


closure_t* _enc__closure__Ped_util_Global_funsmin;


closure_t* _enc__closure__Ped_util_Global_funsmax;


closure_t* _enc__closure__Ped_util_Global_funsflatten;


closure_t* _enc__closure__Ped_util_Global_funscopy_tr;


closure_t* _enc__closure__Ped_util_Global_funscopy_ar;


closure_t* _enc__closure__Ped_util_Global_funsextreme_check;


closure_t* _enc__closure__Ped_util_Global_funsfind_extreme;


closure_t* _enc__closure__Ped_util_Global_funsinv_sqrt;


closure_t* _enc__closure__Ped_util_Regionslink;


closure_t* _enc__closure__Ped_util_Regionsregions;


closure_t* _enc__closure_Stringstring_from_bool;


closure_t* _enc__closure_Stringstring_from_int;


closure_t* _enc__closure_Stringstring_from_real;


closure_t* _enc__closure_Stringstring_from_array;


closure_t* _enc__closure_Stringstring_from_char;


//////////////
// Class IDs


enum
{
  __ID_DUMMY__ = 1024,
  _ENC__ID__Ped_util_Agent_passive_Agent,
  _ENC__ID__Ped_util_IO_File,
  _ENC__ID__Ped_util_Quad_tree_Quad_tree,
  _ENC__ID__Ped_util_Quad_tree_Quad,
  _ENC__ID__Ped_util_Regions_Item,
  _ENC__ID__Ped_util_Regions_Box,
  _ENC__ID__Ped_util_Regions_Tiling_box,
  _ENC__ID__Ped_util_XML_XML_lib,
  _ENC__ID__Ped_util_XML_XML_node,
  _ENC__ID__parallel_small_Main,
  _ENC__ID_String_String,
  _ENC__ID_Std_Eq,
  _ENC__ID_Std_Id,
};


////////////////////
// Trace functions


void _enc__trace__Ped_util_Agent_passive_Agent(pony_ctx_t*, void*);


void _enc__trace__Ped_util_IO_File(pony_ctx_t*, void*);


void _enc__trace__Ped_util_Quad_tree_Quad_tree(pony_ctx_t*, void*);


void _enc__trace__Ped_util_Quad_tree_Quad(pony_ctx_t*, void*);


void _enc__trace__Ped_util_Regions_Item(pony_ctx_t*, void*);


void _enc__trace__Ped_util_Regions_Box(pony_ctx_t*, void*);


void _enc__trace__Ped_util_Regions_Tiling_box(pony_ctx_t*, void*);


void _enc__trace__Ped_util_XML_XML_lib(pony_ctx_t*, void*);


void _enc__trace__Ped_util_XML_XML_node(pony_ctx_t*, void*);


void _enc__trace__parallel_small_Main(pony_ctx_t*, void*);


void _enc__trace_String_String(pony_ctx_t*, void*);


////////////////////////////////
// Runtime type init functions


void _enc__type_init__Ped_util_Agent_passive_Agent(_enc__class__Ped_util_Agent_passive_Agent_t*, ...);


void _enc__type_init__Ped_util_IO_File(_enc__class__Ped_util_IO_File_t*, ...);


void _enc__type_init__Ped_util_Quad_tree_Quad_tree(_enc__class__Ped_util_Quad_tree_Quad_tree_t*, ...);


void _enc__type_init__Ped_util_Quad_tree_Quad(_enc__class__Ped_util_Quad_tree_Quad_t*, ...);


void _enc__type_init__Ped_util_Regions_Item(_enc__class__Ped_util_Regions_Item_t*, ...);


void _enc__type_init__Ped_util_Regions_Box(_enc__class__Ped_util_Regions_Box_t*, ...);


void _enc__type_init__Ped_util_Regions_Tiling_box(_enc__class__Ped_util_Regions_Tiling_box_t*, ...);


void _enc__type_init__Ped_util_XML_XML_lib(_enc__class__Ped_util_XML_XML_lib_t*, ...);


void _enc__type_init__Ped_util_XML_XML_node(_enc__class__Ped_util_XML_XML_node_t*, ...);


void _enc__type_init__parallel_small_Main(_enc__class__parallel_small_Main_t*, ...);


void _enc__type_init_String_String(_enc__class_String_String_t*, ...);


////////////
// Methods


tuple_t* _enc__method__Ped_util_Agent_passive_Agent_pos(pony_ctx_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, pony_type_t**);


void* _enc__method__Ped_util_Agent_passive_Agent_move_int(pony_ctx_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, pony_type_t**, int64_t, int64_t);


void* _enc__method__Ped_util_Agent_passive_Agent_next(pony_ctx_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, pony_type_t**, array_t*);


void* _enc__method__Ped_util_Agent_passive_Agent_init(pony_ctx_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, pony_type_t**, tuple_t*, array_t*, array_t*, int64_t, int64_t);


int64_t _enc__method__Ped_util_IO_File_eof(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**);


char* _enc__method__Ped_util_IO_File_readlineChar(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**);


_enc__class_String_String_t* _enc__method__Ped_util_IO_File_readline(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**);


void* _enc__method__Ped_util_IO_File_writeChar(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**, char*);


void* _enc__method__Ped_util_IO_File_write(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**, _enc__class_String_String_t*);


void* _enc__method__Ped_util_IO_File_close(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**);


void* _enc__method__Ped_util_IO_File_open(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**, _enc__class_String_String_t*, _enc__class_String_String_t*);


void* _enc__method__Ped_util_IO_File_init(pony_ctx_t**, _enc__class__Ped_util_IO_File_t*, pony_type_t**, _enc__class_String_String_t*, _enc__class_String_String_t*);


int64_t _enc__method__Ped_util_Quad_tree_Quad_tree_size(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_tree_t*, pony_type_t**);


int64_t _enc__method__Ped_util_Quad_tree_Quad_tree_remove(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_tree_t*, pony_type_t**, int64_t, int64_t);


void* _enc__method__Ped_util_Quad_tree_Quad_tree_set(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_tree_t*, pony_type_t**, int64_t, int64_t);


int64_t _enc__method__Ped_util_Quad_tree_Quad_tree_get_val(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_tree_t*, pony_type_t**, int64_t, int64_t);


void* _enc__method__Ped_util_Quad_tree_Quad_tree_init(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_tree_t*, pony_type_t**, tuple_t*, tuple_t*);


_enc__class__Ped_util_Quad_tree_Quad_t* _enc__method__Ped_util_Quad_tree_Quad_recur(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_t*, pony_type_t**, int64_t, int64_t);


int64_t _enc__method__Ped_util_Quad_tree_Quad_same(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_t*, pony_type_t**, int64_t, int64_t);


int64_t _enc__method__Ped_util_Quad_tree_Quad_isin(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_t*, pony_type_t**, int64_t, int64_t);


int64_t _enc__method__Ped_util_Quad_tree_Quad_remove(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_t*, pony_type_t**, int64_t, int64_t);


void* _enc__method__Ped_util_Quad_tree_Quad_add(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_t*, pony_type_t**, int64_t, int64_t);


void* _enc__method__Ped_util_Quad_tree_Quad_init(pony_ctx_t**, _enc__class__Ped_util_Quad_tree_Quad_t*, pony_type_t**, int64_t, int64_t, int64_t, int64_t);


void* _enc__method__Ped_util_Regions_Item_append(pony_ctx_t**, _enc__class__Ped_util_Regions_Item_t*, pony_type_t**, option_t*);


void* _enc__method__Ped_util_Regions_Item_delete(pony_ctx_t**, _enc__class__Ped_util_Regions_Item_t*, pony_type_t**);


void* _enc__method__Ped_util_Regions_Item_init(pony_ctx_t**, _enc__class__Ped_util_Regions_Item_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*);


void* _enc__method__Ped_util_Regions_Box_await(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


void* _enc__method__Ped_util_Regions_Box_suspend(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


int64_t _enc__method__Ped_util_Regions_Box_move(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


int64_t _enc__method__Ped_util_Regions_Box_is_something(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, option_t*);


void* _enc__method__Ped_util_Regions_Box_merge(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


int64_t _enc__method__Ped_util_Regions_Box_link(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Regions_Box_t*);


int64_t _enc__method__Ped_util_Regions_Box_add_internal(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t);


option_t* _enc__method__Ped_util_Regions_Box_external_move(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t);


int64_t _enc__method__Ped_util_Regions_Box_add(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*);


array_t* _enc__method__Ped_util_Regions_Box_agents(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


tuple_t* _enc__method__Ped_util_Regions_Box_max(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


tuple_t* _enc__method__Ped_util_Regions_Box_min(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void* _enc__method__Ped_util_Regions_Box_init(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, tuple_t*, tuple_t*);


void* _enc__method__Ped_util_Regions_Tiling_box_await(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, future_t*);


void* _enc__method__Ped_util_Regions_Tiling_box_suspend(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**);


array_t* _enc__method__Ped_util_Regions_Tiling_box_agents(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**);


void* _enc__method__Ped_util_Regions_Tiling_box_move(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, int64_t);


void* _enc__method__Ped_util_Regions_Tiling_box_init(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, array_t*, int64_t);


option_t* _enc__method__Ped_util_XML_XML_lib_file_to_xml(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method__Ped_util_XML_XML_lib_pair(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**, _enc__class_String_String_t*, _enc__class_String_String_t*);


int64_t _enc__method__Ped_util_XML_XML_lib_or_find_from(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**, _enc__class_String_String_t*, _enc__class_String_String_t*, int64_t, _enc__class_String_String_t*);


void* _enc__method__Ped_util_XML_XML_lib_ext_atribs(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**, _enc__class__Ped_util_XML_XML_node_t*, _enc__class_String_String_t*);


char _enc__method__Ped_util_XML_XML_lib_tag_type(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**, _enc__class_String_String_t*);


option_t* _enc__method__Ped_util_XML_XML_lib_new_XML_node(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**, _enc__class_String_String_t*);


void* _enc__method__Ped_util_XML_XML_lib_init(pony_ctx_t**, _enc__class__Ped_util_XML_XML_lib_t*, pony_type_t**);


_enc__class_String_String_t* _enc__method__Ped_util_XML_XML_node_attribute_value(pony_ctx_t**, _enc__class__Ped_util_XML_XML_node_t*, pony_type_t**, _enc__class_String_String_t*);


array_t* _enc__method__Ped_util_XML_XML_node_children_named(pony_ctx_t**, _enc__class__Ped_util_XML_XML_node_t*, pony_type_t**, _enc__class_String_String_t*);


void* _enc__method__Ped_util_XML_XML_node_init(pony_ctx_t**, _enc__class__Ped_util_XML_XML_node_t*, pony_type_t**);


void* _enc__method__parallel_small_Main_init(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**);


void* _enc__method__parallel_small_Main_await(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, future_t*);


void* _enc__method__parallel_small_Main_suspend(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**);


void* _enc__method__parallel_small_Main_main(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, array_t*);


option_t* _enc__method_String_String_to_int(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


array_t* _enc__method_String_String_split(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


array_t* _enc__method_String_String_to_array(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


option_t* _enc__method_String_String_char_at(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, int64_t);


_enc__class_String_String_t* _enc__method_String_String_format(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, array_t*);


_enc__class_String_String_t* _enc__method_String_String_delete(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_find_from(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*, int64_t);


int64_t _enc__method_String_String_find(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


_enc__class_String_String_t* _enc__method_String_String_replace(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*, _enc__class_String_String_t*);


_enc__class_String_String_t* _enc__method_String_String_trim(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


char* _enc__method_String_String_getData(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


_enc__class_String_String_t* _enc__method_String_String_join(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, array_t*);


int64_t _enc__method_String_String_occurrences(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_eq(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_equals(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


option_t* _enc__method_String_String_substring(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, int64_t, int64_t);


int64_t _enc__method_String_String_size(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


int64_t _enc__method_String_String_length(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


_enc__class_String_String_t* _enc__method_String_String_to_lower(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


_enc__class_String_String_t* _enc__method_String_String_to_upper(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


int64_t _enc__method_String_String_compare_ignore_case(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_compare(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_contains_ignore_case(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_contains(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


_enc__class_String_String_t* _enc__method_String_String_copy(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


_enc__class_String_String_t* _enc__method_String_String_concatenate(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, _enc__class_String_String_t*);


int64_t _enc__method_String_String_is_empty(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**);


void* _enc__method_String_String_init(pony_ctx_t**, _enc__class_String_String_t*, pony_type_t**, char*);


future_t* _enc__method__Ped_util_Regions_Box_await_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_suspend_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Box_move_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Box_is_something_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, option_t*);


future_t* _enc__method__Ped_util_Regions_Box_merge_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Box_link_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Regions_Box_t*);


future_t* _enc__method__Ped_util_Regions_Box_add_internal_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t);


future_t* _enc__method__Ped_util_Regions_Box_external_move_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t);


future_t* _enc__method__Ped_util_Regions_Box_add_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*);


future_t* _enc__method__Ped_util_Regions_Box_agents_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Box_max_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Box_min_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Box_init_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, tuple_t*, tuple_t*);


void _enc__method__Ped_util_Regions_Box_await_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


void _enc__method__Ped_util_Regions_Box_suspend_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Box_move_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Box_is_something_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, option_t*);


void _enc__method__Ped_util_Regions_Box_merge_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Box_link_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Regions_Box_t*);


void _enc__method__Ped_util_Regions_Box_add_internal_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t);


void _enc__method__Ped_util_Regions_Box_external_move_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t);


void _enc__method__Ped_util_Regions_Box_add_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*);


void _enc__method__Ped_util_Regions_Box_agents_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Box_max_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Box_min_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Box_init_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, tuple_t*, tuple_t*);


future_t* _enc__method__Ped_util_Regions_Box_await_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_suspend_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_move_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_is_something_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, option_t*, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_merge_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_link_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Regions_Box_t*, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_add_internal_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_external_move_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, int64_t, int64_t, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_add_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, _enc__class__Ped_util_Agent_passive_Agent_t*, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_agents_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_max_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_min_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Box_init_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Box_t*, pony_type_t**, tuple_t*, tuple_t*, future_t*);


future_t* _enc__method__Ped_util_Regions_Tiling_box_await_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Tiling_box_suspend_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Tiling_box_agents_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**);


future_t* _enc__method__Ped_util_Regions_Tiling_box_move_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, int64_t);


future_t* _enc__method__Ped_util_Regions_Tiling_box_init_future(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, array_t*, int64_t);


void _enc__method__Ped_util_Regions_Tiling_box_await_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, future_t*);


void _enc__method__Ped_util_Regions_Tiling_box_suspend_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Tiling_box_agents_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**);


void _enc__method__Ped_util_Regions_Tiling_box_move_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, int64_t);


void _enc__method__Ped_util_Regions_Tiling_box_init_one_way(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, array_t*, int64_t);


future_t* _enc__method__Ped_util_Regions_Tiling_box_await_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, future_t*, future_t*);


future_t* _enc__method__Ped_util_Regions_Tiling_box_suspend_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Tiling_box_agents_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, future_t*);


future_t* _enc__method__Ped_util_Regions_Tiling_box_move_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, int64_t, future_t*);


future_t* _enc__method__Ped_util_Regions_Tiling_box_init_forward(pony_ctx_t**, _enc__class__Ped_util_Regions_Tiling_box_t*, pony_type_t**, array_t*, int64_t, future_t*);


future_t* _enc__method__parallel_small_Main_init_future(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**);


future_t* _enc__method__parallel_small_Main_await_future(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, future_t*);


future_t* _enc__method__parallel_small_Main_suspend_future(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**);


future_t* _enc__method__parallel_small_Main_main_future(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, array_t*);


void _enc__method__parallel_small_Main_init_one_way(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**);


void _enc__method__parallel_small_Main_await_one_way(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, future_t*);


void _enc__method__parallel_small_Main_suspend_one_way(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**);


void _enc__method__parallel_small_Main_main_one_way(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, array_t*);


future_t* _enc__method__parallel_small_Main_init_forward(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, future_t*);


future_t* _enc__method__parallel_small_Main_await_forward(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, future_t*, future_t*);


future_t* _enc__method__parallel_small_Main_suspend_forward(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, future_t*);


future_t* _enc__method__parallel_small_Main_main_forward(pony_ctx_t**, _enc__class__parallel_small_Main_t*, pony_type_t**, array_t*, future_t*);


/////////////////
// Constructors


_enc__class__Ped_util_Agent_passive_Agent_t* _enc__constructor__Ped_util_Agent_passive_Agent();


_enc__class__Ped_util_IO_File_t* _enc__constructor__Ped_util_IO_File();


_enc__class__Ped_util_Quad_tree_Quad_tree_t* _enc__constructor__Ped_util_Quad_tree_Quad_tree();


_enc__class__Ped_util_Quad_tree_Quad_t* _enc__constructor__Ped_util_Quad_tree_Quad();


_enc__class__Ped_util_Regions_Item_t* _enc__constructor__Ped_util_Regions_Item();


_enc__class__Ped_util_Regions_Box_t* _enc__constructor__Ped_util_Regions_Box();


_enc__class__Ped_util_Regions_Tiling_box_t* _enc__constructor__Ped_util_Regions_Tiling_box();


_enc__class__Ped_util_XML_XML_lib_t* _enc__constructor__Ped_util_XML_XML_lib();


_enc__class__Ped_util_XML_XML_node_t* _enc__constructor__Ped_util_XML_XML_node();


_enc__class__parallel_small_Main_t* _enc__constructor__parallel_small_Main();


_enc__class_String_String_t* _enc__constructor_String_String();


////////////////////
// Main actor rtti


extern pony_type_t _enc__active_Main_type;


////////////////
// Trait types


enum
{
  __TRAIT_METHOD_DUMMY__ = 1024,
  _ENC__MSG_Std_Eq_eq,
  _ENC__FUT_MSG_Std_Eq_eq,
  _ENC__ONEWAY_MSG_Std_Eq_eq,
};


struct _enc__trait_Std_Eq_t
{
  pony_type_t* _enc__self_type;
};


struct _enc__trait_Std_Id_t
{
  pony_type_t* _enc__self_type;
};
#endif /* ifndef HEADER_H */
