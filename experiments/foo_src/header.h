#include <pony/pony.h>
#include <stdlib.h>
#include <set.h>
#include <closure.h>
#include <future.h>
#include <string.h>
#include <stdio.h>

#define UNIT NULL - 1

/////////////////////
// Shared functions
pony_actor_t* create_and_send(pony_actor_type_t* type, uint64_t msg_id);

////////////////////
// Shared messages
pony_msg_t m_MSG_alloc;
pony_msg_t m_resume_get;
pony_msg_t m_run_closure;

////////////////
// Message IDs
enum
{
  MSG_alloc,
  MSG_Foo_foo,
  MSG_Bar_bar,
  MSG_Main_main,
  MSG_Foo__one_way_foo,
  MSG_Bar__one_way_bar,
  MSG_Main__one_way_main,
};

//////////////
// Class IDs
enum
{
  ID_Foo,
  ID_Bar,
  ID_Main,
};

/////////////////
// Data structs
typedef struct ___Foo_data Foo_data;

typedef struct ___Bar_data Bar_data;

typedef struct ___Main_data Main_data;

///////////////////////////////
// Passive class data structs
struct ___Bar_data
{
  pony_actor_t* f;
};

/////////////////////////////////
// Forward declarations for Foo
pony_actor_type_t Foo_actor;

void* Foo_foo(Foo_data*)
;

/////////////////////////////////
// Forward declarations for Bar
void* Bar_bar(Bar_data*, pony_actor_t*)
;

//////////////////////////////////
// Forward declarations for Main
pony_actor_type_t Main_actor;

void* Main_main(Main_data*, int64_t, char**)
;
