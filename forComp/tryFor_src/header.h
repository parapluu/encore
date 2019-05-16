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





/////////////////////
// Class type decls


typedef struct _enc__class__tryFor_Main_t _enc__class__tryFor_Main_t;


/////////////////////
// Trait type decls


////////////////////////
// Passive class types


//////////////////
// Runtime types


extern pony_type_t _enc__class__tryFor_Main_type;


////////////////
// Message IDs


enum
{
  _MSG_DUMMY__ = 1024,
  _ENC__FUT_MSG__tryFor_Main_await,
  _ENC__FUT_MSG__tryFor_Main_suspend,
  _ENC__FUT_MSG__tryFor_Main_main,
  _ENC__FUT_MSG__tryFor_Main_init,
  _ENC__ONEWAY_MSG__tryFor_Main_await,
  _ENC__ONEWAY_MSG__tryFor_Main_suspend,
  _ENC__ONEWAY_MSG__tryFor_Main_main,
  _ENC__ONEWAY_MSG__tryFor_Main_init,
};


//////////////////
// Message types


typedef struct _enc__fut_msg__tryFor_Main_await_t _enc__fut_msg__tryFor_Main_await_t;


typedef struct _enc__oneway_msg__tryFor_Main_await_t _enc__oneway_msg__tryFor_Main_await_t;


typedef struct _enc__fut_msg__tryFor_Main_suspend_t _enc__fut_msg__tryFor_Main_suspend_t;


typedef struct _enc__oneway_msg__tryFor_Main_suspend_t _enc__oneway_msg__tryFor_Main_suspend_t;


typedef struct _enc__fut_msg__tryFor_Main_main_t _enc__fut_msg__tryFor_Main_main_t;


typedef struct _enc__oneway_msg__tryFor_Main_main_t _enc__oneway_msg__tryFor_Main_main_t;


typedef struct _enc__fut_msg__tryFor_Main_init_t _enc__fut_msg__tryFor_Main_init_t;


typedef struct _enc__oneway_msg__tryFor_Main_init_t _enc__oneway_msg__tryFor_Main_init_t;


struct _enc__fut_msg__tryFor_Main_await_t
{
  encore_fut_msg_t ;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__oneway_msg__tryFor_Main_await_t
{
  encore_oneway_msg_t msg;
  future_t* f1 /* f */;
  pony_type_t* _enc__type__t /* _enc__type__t */;
};


struct _enc__fut_msg__tryFor_Main_suspend_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__tryFor_Main_suspend_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__tryFor_Main_main_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__tryFor_Main_main_t
{
  encore_oneway_msg_t msg;
};


struct _enc__fut_msg__tryFor_Main_init_t
{
  encore_fut_msg_t ;
};


struct _enc__oneway_msg__tryFor_Main_init_t
{
  encore_oneway_msg_t msg;
};


/////////////////////
// Global functions


//////////////
// Class IDs


enum
{
  __ID_DUMMY__ = 1024,
  _ENC__ID__tryFor_Main,
};


////////////////////
// Trace functions


void _enc__trace__tryFor_Main(pony_ctx_t*, void*);


////////////////////////////////
// Runtime type init functions


void _enc__type_init__tryFor_Main(_enc__class__tryFor_Main_t*, ...);


////////////
// Methods


void* _enc__method__tryFor_Main_await(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*);


void* _enc__method__tryFor_Main_suspend(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


void* _enc__method__tryFor_Main_main(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, array_t*);


void* _enc__method__tryFor_Main_init(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


future_t* _enc__method__tryFor_Main_await_future(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*);


future_t* _enc__method__tryFor_Main_suspend_future(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


future_t* _enc__method__tryFor_Main_main_future(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


future_t* _enc__method__tryFor_Main_init_future(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


void _enc__method__tryFor_Main_await_one_way(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*);


void _enc__method__tryFor_Main_suspend_one_way(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


void _enc__method__tryFor_Main_main_one_way(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


void _enc__method__tryFor_Main_init_one_way(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**);


future_t* _enc__method__tryFor_Main_await_forward(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*, future_t*);


future_t* _enc__method__tryFor_Main_suspend_forward(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*);


future_t* _enc__method__tryFor_Main_main_forward(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*);


future_t* _enc__method__tryFor_Main_init_forward(pony_ctx_t**, _enc__class__tryFor_Main_t*, pony_type_t**, future_t*);


/////////////////
// Constructors


_enc__class__tryFor_Main_t* _enc__constructor__tryFor_Main();


////////////////////
// Main actor rtti


extern pony_type_t _enc__active_Main_type;


////////////////
// Trait types


enum
{
  __TRAIT_METHOD_DUMMY__ = 1024,
};
#endif /* ifndef HEADER_H */
