/*
 * exceptions4c lightweight version 2.1
 *
 * Copyright (c) 2016 Guillermo Calvo
 * Licensed under the GNU Lesser General Public License
 */

#ifndef EXCEPTIONS4C_LITE
#define EXCEPTIONS4C_LITE

#include <stddef.h>
#include <setjmp.h>

/* Maximum length (in bytes) of an exception message */
#ifndef E4C_MESSAGE_SIZE
# define E4C_MESSAGE_SIZE 128
#endif

/* Controls whether file/line info is attached to exceptions */
#ifndef NDEBUG
# define E4C_DEBUG_INFO __FILE__, __LINE__
#else
# define E4C_DEBUG_INFO NULL, 0
#endif

/* Represents an exception type */
typedef struct e4c_exception_type{
    const char * name;
    const struct e4c_exception_type * supertype;
} e4c_exception_type_t;

/* Declarations and definitions of exception types */
#define E4C_DECLARE_EXCEPTION(name) extern const e4c_exception_type_t name
#define E4C_DEFINE_EXCEPTION(name, supertype) const e4c_exception_type_t name = { #name, &supertype }

/* Predefined exception types */
E4C_DECLARE_EXCEPTION(RuntimeException);
E4C_DECLARE_EXCEPTION(NullPointerException);

/* Represents an instance of an exception type */
typedef struct e4c_exception{
    char message[E4C_MESSAGE_SIZE];
    char * file;
    int line;
    const e4c_exception_type_t * type;
} e4c_exception_t;

/* Retrieve current exception in specific context */
#define E4C_EXCEPTION(ctx) ctx->err

/* Retrieve current exception object, returns NULL if none exists. */
#define E4C_EXCEPTION_OBJ(ctx) ctx->err_obj

/* Returns whether current exception is of a given type */
#define E4C_IS_INSTANCE_OF(ctx,t) ( E4C_EXCEPTION(ctx).type == &t || e4c_extends(E4C_EXCEPTION(ctx).type, &t) )

/* Implementation details */
#define E4C_TRY(ctx) \
  ctx->frame=&E4C_NEW_FRAME(ctx);                             \
  if(setjmp(ctx->frame->jump) >= 0) while(e4c_hook(ctx, 0)) { \
    if(ctx->frame->stage == e4c_trying)
#define E4C_CATCH(ctx, type) else if(ctx->frame->stage == e4c_catching && E4C_IS_INSTANCE_OF(ctx,type) && e4c_hook(ctx, 1))
#define E4C_FINALLY(ctx) else if(ctx->frame->stage == e4c_finalizing)
#define E4C_TRY_END }

/*
 * Throw an exception. The exception is not propagated when the context has
 * no remaining frames, a null value is then returned. Check whether this
 * occured with `E4C_UNPROPAGATED`. Note: Uncaught exceptions in main will
 * cause main to return (exit the program).
 *
 * See: `E4C_UNPROPAGATED`
 * See: `E4C_FAIL`
 * See: `E4C_RETHROW`
 *
 * Example:
 * --------------------------------------------------------------------------
 * void thrower() { E4C_THROW(ctx,RuntimeException,""); }
 * int main(void){ thrower(); if(E4C_UNPROPAGATED(ctx)) {E4C_FAIL(ctx);} }
 * --------------------------------------------------------------------------
 * int main(void){ E4C_THROW(ctx,RuntimeException,""); puts("won't reach"); }
 * --------------------------------------------------------------------------
 */
#define E4C_THROW(ctx, type, message) e4c_throw(ctx, &type, E4C_DEBUG_INFO, message, NULL); return 0

/* Rethrow an exception.
 * `exc_obj` is a pointer the Encore exception object, NULL = none.
 *
 * See: `E4C_THROW`
 */
#define E4C_RETHROW(ctx, type_addr, file, line, message, exc_obj) e4c_throw(ctx, type_addr, file, line, message, exc_obj); return 0

/* Whether an exception could not be thrown due to reaching the last exception frame.  */
#define E4C_UNPROPAGATED(ctx) ctx->unpropagated

/* Use to exit the program due to some exception, e.g. after an unpropagated exception.  */
#define E4C_FAIL(ctx) e4c_fail(ctx)

enum e4c_stage{e4c_beginning, e4c_trying, e4c_catching, e4c_finalizing, e4c_done};
struct e4c_frame{
  jmp_buf jump;
  unsigned char stage;
  unsigned char uncaught;
  struct e4c_frame * prev;
};
typedef struct e4c_context{
  e4c_exception_t err;
  void * err_obj;
  struct e4c_frame * frame;
  unsigned char unpropagated;
} e4c_context_t;

// Done this way due to GCC bug 53119, might vary depending on OS.
// Can be replaced with `(e4c_context_t ) {0}` in newer versions of GCC.
#if defined(PLATFORM_IS_MACOSX)
 #define E4C_NEW_CTX (e4c_context_t){ {{0}}, 0, 0, 0 }
 #define E4C_NEW_FRAME(ctx) (struct e4c_frame){{{{0}}},e4c_beginning,0,ctx->frame}
#else
 #define E4C_NEW_CTX (e4c_context_t){ {{0}}, 0, 0, 0 }
 #define E4C_NEW_FRAME(ctx) (struct e4c_frame){{{{0}}},e4c_beginning,0,ctx->frame}
#endif

/* Get the current e4c_context. */
e4c_context_t * e4c_ctx();

/* Set the current e4c_context  */
void e4c_ctx_set(e4c_context_t * ctx);

/* This functions must be called only via E4C_TRY, E4C_CATCH, E4C_FINALLY and E4C_THROW */
extern int e4c_try(e4c_context_t * ctx, const char * file, int line);
extern int e4c_hook(e4c_context_t * ctx, int is_catch);
extern int e4c_extends(const e4c_exception_type_t * child, const e4c_exception_type_t * parent);
extern void e4c_throw(e4c_context_t * ctx, const e4c_exception_type_t * exception_type, char * file, int line, char * message, void * exception_obj);
extern void e4c_fail(e4c_context_t * ctx);

#endif
