#ifndef pony_pony_h
#define pony_pony_h

#include <stddef.h>
#include <stdint.h>

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

/// Indicates the mode of an object.
typedef enum
{
  PONY_PRIMITIVE,
  PONY_ACTOR,
  PONY_MUTABLE,
  PONY_IMMUTABLE,
  PONY_OPAQUE
} pony_mode_t;

/** Trace function.
 *
 * Each type supplies a trace function. It is invoked with the object being
 * traced. In this function, pony_trace() should be called for each field in
 * the object.
 */
typedef void (*pony_trace_fn)(void* p);

/// Describes a type to the runtime
typedef const struct pony_type_t
{
  pony_trace_fn trace;
  size_t size;
  pony_mode_t mode;
} pony_type_t;

/// Maximum argument count to pony_sendv
#define PONY_MAX_ARG 6

/// Describes the arguments an actor expects for a given message ID.
typedef const struct pony_msg_t
{
  int argc;
  pony_type_t type[PONY_MAX_ARG];
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

/// Describes an actor type to the runtime
typedef const struct pony_actor_type_t
{
  uint32_t id;
  pony_type_t type;
  pony_msg_fn msg;
  pony_dispatch_fn dispatch;
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

/** Allocate memory on the current actor's heap.
 *
 * This is garbage collected memory. This can only be done while an actor is
 * handling a message, so that there is a current actor.
 */
void* pony_alloc(size_t size) __attribute__((malloc,alloc_size(1)));

/// Trigger GC next time the current actor is scheduled
void pony_triggergc();

/** Trace an object.
 *
 * This should be called for every pointer field in an object when the object's
 * trace function is invoked.
 *
 * @param p The address of the field, not the value of the field.
 * @param f The trace function of the field type, if any.
 * @param size The size of the object pointed to by the field.
 * @param mode The mode of the field, if any.
 */
void pony_trace(void* p, pony_trace_fn f, size_t size, pony_mode_t mode);

/// Trace an actor reference
void pony_traceactor(pony_actor_t** p);

/// Trace an array of objects.
void pony_tracearray(void* p, size_t asize, pony_mode_t amode,
  pony_trace_fn f, size_t size, pony_mode_t mode);

/// Trace an 8 bit primitive
void pony_trace8(void* p);

/// Trace a 16 bit primitive
void pony_trace16(void* p);

/// Trace a 32 bit primitive
void pony_trace32(void* p);

/// Trace a 64 bit primitive
void pony_trace64(void* p);

/** Starts the pony runtime.
 *
 * Takes the command line arguments and the initial actor. The initial actor
 * will be sent (PONY_MAIN, argc, argv) once the runtime is initialised.
 */
int pony_start(int argc, char** argv, pony_actor_t* actor);

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
