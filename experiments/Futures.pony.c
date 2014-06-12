#include <assert.h>
#include <pony/pony.h>
#include <stdlib.h>
#include <unistd.h>
#include <set.h>
#include <context.h>
#include <stdio.h>
#define _XOPEN_SOURCE 800
#include <ucontext.h>
#undef _XOPEN_SOURCE
#include <assert.h>

#include "future.h"
#include "closure.h"

#include <signal.h>


//#define _DBG

pony_actor_t* create_and_send(pony_actor_type_t* type, uint64_t msg_id) {
  pony_actor_t* ret = pony_create(type);
  pony_send(ret, msg_id);

  return ret;
}

static pony_msg_t m_MSG_alloc = {0, {{NULL, 0, PONY_PRIMITIVE}}};


enum
{
  MSG_alloc,
  MSG_Square_spawn,
  MSG_Main_main,
  MSG_Main_tell,
};


enum
{
  ID_Square,
  ID_Main
};


//////////////////////////////////////
// Forward declarations for Square


typedef struct ___Square_data Square_data;


static pony_actor_type_t Square_actor;


static void Square_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*);


//////////////////////////////////
// Forward declarations for Main


typedef struct ___Main_data Main_data;


static pony_actor_type_t Main_actor;


static void Main_dispatch(pony_actor_t*, void*, uint64_t, int, pony_arg_t*);


void Main_main(ctx_t*,void*);


void Main_tell(Main_data*, int64_t, int64_t);


/////////////////////////////////////
// Implementation of class Square


struct ___Square_data
{
  pony_actor_t* aref;
};

struct ___Main_data
{
  pony_actor_t* aref;
  future *fut;
  pony_actor_t* square;
};

static void Main_trace(void* p);

static void Square_trace(void* p)
{
  printf("########### Lying Square_trace\n");
  exit(1);
}

static pony_msg_t m_Square_spawn= {2, {{NULL, 0, PONY_PRIMITIVE}, {NULL, 0, PONY_PRIMITIVE}}};

static pony_msg_t* Square_message_type(uint64_t id)
{
  switch ( id )
  {
    case MSG_alloc:
    {
      return &m_MSG_alloc;
    }
    case MSG_Square_spawn:
    {
      return &m_Square_spawn;
    }
    default:
    {
      printf("########### Main_message_type: don't have\n");
      exit(1);
    }
  }
  return NULL;
}

static pony_actor_type_t Square_actor = { ID_Square, { Square_trace, sizeof(Square_data), PONY_ACTOR }, Square_message_type, Square_dispatch };

static void Square_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  switch ( id )
  {
    case MSG_alloc:
    {
      printf("############ sq alloc\n");
      p = pony_alloc(sizeof(Square_data));
      ((Square_data*)p)->aref = this;
      pony_set(p);
      break;
    }
    case MSG_Square_spawn:
    {
      printf("############ sq spawn\n");
      future *fut = argv[0].p;
      int n = argv[1].i;
      int *res = malloc(sizeof(int));
      *res = n*n;
      /* TODO: this result isn't handed on... */
      printf("result: %i @ %p\n", *res, res);
      fulfil(fut, res);
      break;
    }
    default:
    {
      printf("error, got invalid id: %llu",id);
    }
  }
}


/////////////////////////////////
// Implementation of class Main




static void Main_trace(void* p)
{
  printf("########### Lying Main_trace\n");
  exit(1);
  //  pony_actor_t* aref;
  //  future *fut;
  //  pony_actor_t* square;

  //  Main_data *this = (Main_data*)p;
  //  pony_trace(this->square, Square_trace, sizeof(Square_data), PONY_ACTOR);
}


static pony_msg_t m_Main_main= {0, {}};


static pony_msg_t m_Main_tell= {1, {{NULL, 0, PONY_PRIMITIVE}}};


static pony_msg_t* Main_message_type(uint64_t id)
{
  switch ( id )
  {
    case MSG_alloc:
    {
      return &m_MSG_alloc;
    }
    case MSG_Main_main:
    {
      return &m_Main_main;
    }
    case MSG_Main_tell:
    {
      return &m_Main_tell;
    }
    default:
    {
      printf("########### Main_message_type: don't have\n");
      exit(1);
    }
  }
  return NULL;
}


static pony_actor_type_t Main_actor = { ID_Main, { Main_trace, sizeof(Main_data), PONY_ACTOR }, Main_message_type, Main_dispatch };

struct result_env
{
  pony_actor_t *aref;
  ctx_t *ctx;
};

value send_me_result(value args[], void *state) {
  struct result_env *env = (struct result_env*)state;

  printf("sending tell (env@%p); env->aref = %p env->ctx = %p\n", env, env->aref, env->ctx);

  pony_sendp(env->aref, MSG_Main_tell, env->ctx);

  free(env);
  return int_to_val(0);
}

void Main_main(ctx_t *ctx, void* args)
{
  Main_data *this = args;
  printf("################ this->aref = %p\n",this->aref);
  printf("################ ctx        = %p\n",ctx);

  assert(this != NULL);

  this->square = create_and_send(&Square_actor, MSG_alloc);
  printf("actor_type: %p\n",this->square->actor_type);

  for (int i=1; i<=3; i++) {

    this->fut = createNewFuture();
    struct result_env * env = malloc(sizeof(struct result_env));
    env->aref = this->aref;
    env->ctx = ctx;
    struct closure *send_me_result_closure = closure_mk(send_me_result, env);

    //once the result is done, the future will execute this closure:
    chain(this->fut, NULL, send_me_result_closure);

    //printf("SENDIIIING\n");
    //pony_sendp(env->aref, MSG_Main_tell, env);

    // beginning the future spiel:
    // 1) send the work order, along with the context
    // 2) use ctx_await_result to return what you asked for
    pony_arg_t args2[2] = {{.p=this->fut}, {.i=5*i}};
    pony_sendv(this->square, MSG_Square_spawn, 2, args2);

    //if (!fulfilled(fut)) {
    ctx_await(ctx);
      //}

    int64_t *res = (int64_t*)getValue(this->fut); // whatever the future gave us
    printf("REINSTANTIATED %llu\n", *res);
    //assert(res == 25*i*i);
  }

  printf("DONE WITH MTHD\n");
  
  ctx_return(ctx);
}



static void Main_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv)
{
  switch ( id )
  {
    //case MSG_alloc:
    case PONY_MAIN:
    {
      printf("############ PONY_MAIN\n");
      p = pony_alloc(sizeof(Main_data));
      ((Main_data*)p)->aref = this;
      pony_set(p);
    }
    //      p = pony_alloc(sizeof(Main_data));
    //      pony_set(p);
    case MSG_Main_main:
    {
#ifdef _DBG
      printf("running main...\n");
#endif
      ((Main_data*)p)->aref = this;

      ctx_call(Main_main, (void*)p);
#ifdef _DBG
      printf("I'm back in dispatch!\n");
#endif
      break;
    }
    case MSG_Main_tell:
    {
      printf("############ main tell\n");
      ctx_t * ctx = argv[0].p;
      ctx_reinstate(ctx);
      break;
    }
    default:
    {
      printf("error, got invalid id: %llu",id);
    }
  }
}

int main(int argc, char** argv)
{
  return pony_start(argc, argv, pony_create(&Main_actor));
}
