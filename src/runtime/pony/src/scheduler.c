#define __STDC_FORMAT_MACROS
#include "scheduler.h"
#include "cpu.h"
#include "mpmcq.h"
#include "cycle.h"
#include "actor.h"
#include <pony/pony.h>
#include <pthread.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <assert.h>

#include <signal.h>

typedef struct
{
  pthread_t tid;
  uint32_t id;
  uint32_t cpu;
  uint32_t steal;
  uint64_t last;
  uint64_t app_msgs;
  uint64_t rc_msgs;
  bool quiet;
  bool finish;
  mpmcq_t q;
} scheduler_t __attribute__((aligned (64)));

static uint32_t scheduler_count;
static scheduler_t* scheduler;
static volatile bool finishcd;
static volatile bool stats;
static volatile bool terminate;
static volatile uint32_t quiet;
static volatile uint32_t done;
static volatile int exit_code;

static inline pony_actor_t* scheduler_worksteal(scheduler_t* s)
{
  scheduler_t* from;

  do
  {
    s->steal = rand_r(&s->steal);
    from = &scheduler[s->steal % scheduler_count];
  } while(from == s);

  return mpmcq_pop(&from->q);
}

static pony_actor_t* next_actor(scheduler_t* s, pony_actor_t* prev)
{
  pony_actor_t* next = mpmcq_pop(&s->q);

  if((next == NULL) && (scheduler_count > 1))
  {
    next = scheduler_worksteal(s);
  }

  if(next != NULL)
  {

    if(prev != NULL)
    {
      mpmcq_push(&s->q, prev);
    } else if(s->quiet) {
      s->quiet = false;
      __sync_fetch_and_sub(&quiet, 1);
    }

    return next;
  }

  if(prev == NULL && !s->quiet)
  {
    s->quiet = true;
    __sync_fetch_and_add(&quiet, 1);
  }

  return prev;
}

static __thread scheduler_t *memory;

static void* run_thread(void* arg)
{
  scheduler_t* s = arg;
  memory = s;
  cpu_affinity(s->cpu);

  pony_actor_t* actor = NULL;

  while(true)
  {
    if((actor = next_actor(s, actor)) != NULL)
    {
      if(!actor_run(actor, s->id, &s->app_msgs, &s->rc_msgs))
      {
        actor = NULL;
      }
    } else if(quiet < scheduler_count) {
      struct timespec ts = {0, 0};
      nanosleep(&ts, NULL);
    } else {
      __sync_fetch_and_add(&done, 1);

      while((done < scheduler_count) && (quiet == scheduler_count))
      {
        struct timespec ts = {0, 0};
        nanosleep(&ts, NULL);
      }

      if(quiet == scheduler_count)
      {
        if(s->finish)
        {
          if(finishcd || stats)
          {
            cycle_finish(finishcd, stats);
          } else {
            terminate = true;
          }

          s->finish = false;
        }

        if(terminate) break;
      }

      __sync_fetch_and_sub(&done, 1);
    }
  }

  if(stats)
  {
    printf(
      "Thread:\n"
      "\tapp_msgs: %"PRIu64"\n"
      "\trc_msgs: %"PRIu64"\n",
      s->app_msgs,
      s->rc_msgs
      );
  }

  return NULL;
}

void* run_thread_restart() {
  return run_thread(memory);
}

void pony_actor_unblock(pony_actor_t *actor) {
  mpmcq_push(&memory->q, actor);
}

static int parse_args(int argc, char** argv)
{
  int remove = 0;

  for(int i = 0; i < argc; i++)
  {
    if(!strcmp(argv[i], "--ponythreads"))
    {
      remove++;

      if(i < (argc - 1))
      {
        scheduler_count = atoi(argv[i + 1]);
        remove++;
      }
    } else if(!strcmp(argv[i], "--ponyfinishcd")) {
      remove++;
      finishcd = true;
    } else if(!strcmp(argv[i], "--ponystats")) {
      remove++;
      stats = true;
    } else if(!strcmp(argv[i], "--ponydisablecd")) {
      remove++;
      actor_disablecd(true);
    }

    if(remove > 0)
    {
      argc -= remove;
      memmove(&argv[i], &argv[i + remove], (argc - i) * sizeof(char*));
      remove = 0;
      i--;
    }
  }

  return argc;
}

void scheduler_add(pony_actor_t* actor, uint32_t thread)
{
  mpmcq_push(&scheduler[thread].q, actor);
}

void scheduler_terminate()
{
  terminate = true;
}

int pony_start(int argc, char** argv, pony_actor_t* actor)
{
  argc = parse_args(argc, argv);

  uint32_t physical, logical;
  cpu_count(&physical, &logical);
  assert(physical <= logical);

  // if no thread count is specified, use the physical core count
  if(scheduler_count == 0) { scheduler_count = physical; }

  scheduler = malloc(scheduler_count * sizeof(scheduler_t));
  memset(scheduler, 0, scheduler_count * sizeof(scheduler_t));

  if(scheduler_count <= physical)
  {
    // assign threads to physical processors
    uint32_t index = 0;

    for(uint32_t i = 0; i < scheduler_count; i++)
    {
      if(cpu_physical(i))
      {
        scheduler[index].cpu = i;
        index++;
      }
    }
  } else {
    // assign threads to logical processors
    for(uint32_t i = 0; i < scheduler_count; i++)
    {
      scheduler[i].cpu = i % logical;
    }
  }

  for(uint32_t i = 0; i < scheduler_count; i++)
  {
    scheduler[i].id = i;
    mpmcq_init(&scheduler[i].q);
  }

  scheduler[0].tid = pthread_self();
  scheduler[0].finish = true;
  cycle_create();

  for(uint32_t i = 1; i < scheduler_count; i++)
  {
    if(pthread_create(&scheduler[i].tid, NULL, run_thread, &scheduler[i]) != 0)
    {
      return -1;
    }
  }

  pony_arg_t arg[2];
  arg[0].i = argc;
  arg[1].p = argv;
  actor_sendv(actor, PONY_MAIN, 2, arg);

  run_thread(&scheduler[0]);

  for(uint32_t i = 1; i < scheduler_count; i++)
  {
    pthread_join(scheduler[i].tid, NULL);
  }

  for(uint32_t i = 0; i < scheduler_count; i++)
  {
    mpmcq_destroy(&scheduler[i].q);
  }

  if(stats)
  {
    uint64_t app_msgs = 0;
    uint64_t rc_msgs = 0;

    for(uint32_t i = 0; i < scheduler_count; i++)
    {
      app_msgs += scheduler[i].app_msgs;
      rc_msgs += scheduler[i].rc_msgs;
    }

    printf(
      "Process:\n"
      "\tapp_msgs: %"PRIu64"\n"
      "\trc_msgs: %"PRIu64"\n",
      app_msgs,
      rc_msgs
      );
  }

  free(scheduler);
  return exit_code;
}

void pony_exitcode(int code)
{
  exit_code = code;
}
