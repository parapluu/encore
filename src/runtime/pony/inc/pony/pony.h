#ifndef pony_pony_h
#define pony_pony_h

#include <stddef.h>
#include <stdint.h>
#include <stdbool.h>

#ifdef __cplusplus
extern "C" {
#endif

/** Opaque definition of an actor.
 *
 * The internals of an actor aren't visible to the programmer.
 */
typedef struct pony_actor_t pony_actor_t;

/** Message ID for the main function.
 *
 * When the pony runtime starts, it extracts runtime specific command line
 * arguments and then sends the the remaining command line to an initial actor.
 */
#define PONY_MAIN UINT64_MAX

enum
{
  FUT_MSG_RUN_CLOSURE = UINT64_MAX - 1000,
  FUT_MSG_RESUME
};

/** Argument to a message.
 *
 * This union type is used to make sending primitive types simpler.
 */
typedef union
{
  void* p;
  intptr_t i;
  double d;
} pony_arg_t;

/** Trace function.
 *
 * Each type supplies a trace function. It is invoked with the object being
 * traced. In this function, pony_trace() should be called for each field in
 * the object.
 */
typedef void (*pony_trace_fn)(void* p);

/// Describes a type to the runtime
typedef const struct _pony_type_t
{
  size_t size;
  pony_trace_fn trace;
  pony_trace_fn serialise;
  pony_trace_fn deserialise;
} pony_type_t;

/// Use this when sending non-allocated arguments in messages
#define PONY_NONE ((pony_type_t*)0)

/// Use this when sending actors in messages
#define PONY_ACTOR ((pony_type_t*)1)

/// Maximum argument count to pony_sendv
#define PONY_MAX_ARG 6

/// Describes the arguments an actor expects for a given message ID.
typedef const struct _pony_msg_t
{
  int argc;
  pony_type_t* type[PONY_MAX_ARG];
} pony_msg_t;

/** Message trace function.
 *
 * This function maps an expected message ID to a parameter count and a set of
 * trace functions for those parameters. The maximum count is PONY_MAX_ARG.
 */
typedef pony_msg_t* (*pony_msg_fn)(uint64_t id);

/** Dispatch function.
 *
 * Each actor has a dispatch function that is invoked when the actor handles
 * a message. The actor, a pointer to the actor's data, a type descriptor for
 * actor's data, the message ID and the message argument are provided.
 */
typedef void (*pony_dispatch_fn)(pony_actor_t* actor, void* p, uint64_t id,
  int argc, pony_arg_t* argv);

/** Actor finalizer.
 *
 *  Each actor supplies a finalizer, which is called
 *  before an actor is collected.
 */
typedef void (*pony_final_fn)(void* p);

/// Describes an actor type to the runtime
typedef const struct _pony_actor_type_t
{
  uint32_t id;
  pony_type_t type;
  pony_msg_fn msg;
  pony_dispatch_fn dispatch;
  pony_final_fn final;
} pony_actor_type_t;

/// This function must be supplied by the program, not the runtime
pony_actor_type_t* pony_actor_type(uint32_t id);

/** Create a new actor.
 *
 * When an actor is created, the type is set. This specifies the trace function
 * for the actor's data, the message type function that indicates what messages
 * and arguments an actor is able to receive, and the dispatch function that
 * handles received messages.
 */
pony_actor_t* pony_create(pony_actor_type_t* type);

/** Get the actor's data.
 *
 * This is only needed when manually scheduling actors.
 */
void* pony_get();

/** Set the actor's data.
 *
 * This sets the actor's data with a pointer. This can only be called while
 * handling a message. Generally, an actor will do this only once, when it
 * handles its constructor message.
 */
void pony_set(void* p);

/// Sends a message with no arguments.
void pony_send(pony_actor_t* to, uint64_t id);

/// Sends a vector of arguments in a message.
void pony_sendv(pony_actor_t* to, uint64_t id, int argc, pony_arg_t* argv);

/// Convenience function to send a pointer argument in a message
void pony_sendp(pony_actor_t* to, uint64_t id, void* p);

/// Convenience function to send an integer argument in a message
void pony_sendi(pony_actor_t* to, uint64_t id, intptr_t i);

/// Convenience function to send a floating point argument in a message
void pony_sendd(pony_actor_t* to, uint64_t id, double d);

/** Store a continuation.
 *
 * This puts a message at the front of the actor's queue, instead of at the
 * back. This is not concurrency safe: only a single actor should push a
 * continuation to another actor.
 *
 * Not used in pony.
 */
void pony_continuation(pony_actor_t* to, uint64_t id,
  int argc, pony_arg_t* argv);

/** Allocate memory on the current actor's heap.
 *
 * This is garbage collected memory. This can only be done while an actor is
 * handling a message, so that there is a current actor.
 */
void* pony_alloc(size_t size) __attribute__((malloc,alloc_size(1)));

// TODO: pony_free() for when escape analysis shows a reference doesn't survive

/// Trigger GC next time the current actor is scheduled
void pony_triggergc();

/**
 * If an actor is currently unscheduled, this will reschedule it. This is not
 * concurrency safe: only a single actor should reschedule another actor, and
 * it should be sure the target actor is actually unscheduled.
 */
void pony_schedule(pony_actor_t* actor);

/**
 * The current actor will no longer be scheduled. It will not handle messages on
 * its queue until it is rescheduled.
 */
void pony_unschedule();

/**
 * Call this to "become" an actor on a non-scheduler thread, i.e. from outside
 * the pony runtime. Following this, pony API calls can be made as if the actor
 * in question were the current actor, eg. pony_alloc, pony_send, pony_create,
 * etc.
 *
 * This can be called with NULL to make no actor the "current" actor for a
 * thread.
 */
void pony_become(pony_actor_t* actor);

/**
 * Call this to handle an application message on the actor your thread has
 * "become". This will do two things: first, it will possibly gc, and
 * second it will possibly handle a pending application message. If an
 * application message is handled, it will return true, otherwise false.
 */
bool pony_poll();

/** Trace memory
 *
 * Call this on allocated memory that contains no pointers to other allocated
 * memory. Also use this to mark tag aliases.
 */
void pony_trace(void* p);

/** Trace an actor
 *
 * This should be called for fields in an object that point to an actor.
 */
void pony_traceactor(pony_actor_t* p);

/** Trace an object.
 *
 * This should be called for every pointer field in an object when the object's
 * trace function is invoked.
 *
 * @param p The pointer being traced.
 * @param f The trace function for the object pointed to.
 */
void pony_traceobject(void* p, pony_trace_fn f);

/** Starts the pony runtime.
 *
 * Takes the command line arguments and the initial actor. The initial actor
 * will be sent (PONY_MAIN, argc, argv) once the runtime is initialised. Returns
 * -1 if the scheduler couldn't start, otherwise returns the exit code set with
 * pony_exitcode(), defaulting to 0.
 *
 * It is not safe to call this again before it has returned. For that sort of
 * behaviour, use pony_init() and pony_shutdown().
 */
int pony_start(int argc, char** argv, pony_actor_t* actor);

/** Starts the runtime as a service.
 *
 * This starts the runtime and leaves it running, even if there is no work to
 * be done, until pony_shutdown() is called. It returns -1 if the scheduler
 * couldn't start, otherwise it returns the remaining command line arguments
 * after pony specific arguments have been parsed and removed.
 *
 * It is not safe to call this again before pony_shutdown() has returned.
 */
int pony_init(int argc, char** argv);

/** Shuts down the pony runtime.
 *
 * Call this after pony_init() when the runtime service is no longer needed.
 * Returns the exit code set with pony_exitcode(), defaulting to 0.
 *
 * This will not return immediately: it will wait for actor quiescence.
 */
int pony_shutdown();

/** Set the exit code.
 *
 * The value returned by pony_start() will be 0 unless set to something else
 * with this call. If called more than once, the value from the last call is
 * returned.
 */
void pony_exitcode(int code);

#ifdef __cplusplus
}
#endif

#endif
