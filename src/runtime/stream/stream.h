#ifndef __stream_h__
#define __stream_h__

#include <pony.h>
#include <stdbool.h>

typedef void stream_t;
#include "encore.h"

typedef struct scons scons_t;

bool scons_eos(pony_ctx_t **ctx, scons_t *scons);

encore_arg_t scons_element(pony_ctx_t **ctx, scons_t *scons);

stream_t *scons_next(pony_ctx_t **ctx, scons_t *scons);

scons_t *scons_tail(pony_ctx_t **ctx);

scons_t *scons_put_fut(pony_ctx_t **ctx, stream_t *s,
                            encore_arg_t value, pony_type_t *type);

pony_type_t *get_scons_type();

void scons_trace(pony_ctx_t *ctx, void *p);

/**
 *  Put a value in an EXISTING stream
 *
 *  @param s A stream
 *  @param fut An EXISTING stream
 *  @param value The value to be put in the stream
 *  @param type The runtime type of \p value
 *  @return The (empty) head of \p s
 */
stream_t *stream_put_fut(pony_ctx_t **ctx, future_t* fut, stream_t *s,
                         encore_arg_t value, pony_type_t *type);

struct scons *scons_mk(pony_ctx_t *ctx, pony_type_t *type);

/**
 *  Create a new stream
 *
 *  Relies on garbage collection for memory management
 *  @return An empty, open stream
 */
stream_t *stream_mk(pony_ctx_t **ctx);

/**
 *  Put a value in a stream
 *
 *  @param s A stream
 *  @param value The value to be put in the stream
 *  @param type The runtime type of \p value
 *  @return The (empty) head of \p s
 */
stream_t *stream_put(pony_ctx_t **ctx, stream_t *s, encore_arg_t value,
        pony_type_t *type);

/**
 *  Read a value from a stream
 *
 *  Wait for the stream to contain a value and return it.
 *
 *  @param s A stream
 *  @return The current element of \p s
 */
encore_arg_t stream_get(pony_ctx_t **ctx, stream_t *s);

/**
 *  Get the continuation of a stream
 *
 *  Wait for the stream to contain additional values and return
 *  the "tail" of the stream.
 *
 *  @param s A stream
 *  @return The current continuation of \p s
 */
stream_t *stream_get_next(pony_ctx_t **ctx, stream_t *s);

/**
 *  Close a stream
 *
 *  @param s A stream
 */
void stream_close(pony_ctx_t **ctx, stream_t *s);

/**
 *  Ask if a stream has additional values
 *
 *  Will block until result is available
 *
 *  @param s A stream
 */
bool stream_eos(pony_ctx_t **ctx, stream_t *s);

/**
 * Trace function for streams
 */
void stream_trace(pony_ctx_t *ctx, void *p);

#endif
