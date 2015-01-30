#ifndef __stream_h__
#define __stream_h__

#include <pony/pony.h>
#include <stdbool.h>

typedef void stream_t;

/**
 *  Create a new stream
 *  
 *  Relies on garbage collection for memory management
 *  @return An empty, open stream
 */
stream_t *stream_mk();

/**
 *  Put a value in a stream
 *  
 *  @param s A stream
 *  @param value The value to be put in the stream
 *  @param type The runtime type of \p value
 *  @return The (empty) head of \p s
 */
stream_t *stream_put(stream_t *s, pony_arg_t value, pony_type_t *type);

/**
 *  Read a value from a stream
 *  
 *  Wait for the stream to contain a value and return it. 
 *
 *  @param s A stream
 *  @return The current element of \p s
 */
pony_arg_t stream_get(stream_t *s);

/**
 *  Get the continuation of a stream
 *  
 *  Wait for the stream to contain additional values and return
 *  the "tail" of the stream.
 *
 *  @param s A stream
 *  @return The current continuation of \p s
 */
stream_t *stream_get_next(stream_t *s);

/**
 *  Close a stream
 *  
 *  @param s A stream
 */
void stream_close(stream_t *s);

/**
 *  Ask if a stream has additional values
 *
 *  Will block until result is available
 *  
 *  @param s A stream
 */
bool stream_eos(stream_t *s);

#endif