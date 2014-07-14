#include "streambuf.h"
#include "../ds/list.h"
#include "../mem/pool.h"

#include <string.h>
#include <stdint.h>
#include <stdio.h>

#define CHUNK_SIZE 1024

typedef unsigned char chunk_t[CHUNK_SIZE];

static void chunk_free(chunk_t* c);

DECLARE_LIST(chunklist, chunk_t);
DEFINE_LIST(chunklist, chunk_t, NULL, chunk_free);

struct streambuf_t
{
  chunklist_t* head;
  chunklist_t* tail;

  void* write_offset;
  void* read_offset;

  size_t bytes;
  size_t chunks;
};

static void chunk_free(chunk_t* c)
{
  POOL_FREE(chunk_t, c);
}

static streambuf_t* init()
{
  chunk_t* curr  = 0;
  streambuf_t* buf = 0;

  buf = POOL_ALLOC(streambuf_t);
  memset(buf, 0, sizeof(streambuf_t));

  curr = POOL_ALLOC(chunk_t);

  buf->head = buf->tail = chunklist_append(buf->head, curr);
  buf->chunks++;

  buf->read_offset = buf->write_offset = curr;

  return buf;
}

size_t streambuf_length(streambuf_t* buf)
{
  if(buf == NULL) return 0;
  return buf->bytes;
}

size_t streambuf_chunks(streambuf_t* buf)
{
  return buf->chunks;
}

void streambuf_write(streambuf_t** buf, void* data, size_t len)
{
  chunk_t* curr = 0;
  size_t avail   = 0;
  size_t to_copy = 0;

  streambuf_t* b = *buf;

  if(b == NULL) b = *buf = init();

  while(len > 0)
  {
    curr = chunklist_data(b->tail);
    avail = CHUNK_SIZE - (b->write_offset - (void*)curr);
    to_copy = avail > len ? len : avail;

    memcpy(b->write_offset, data, to_copy);

    avail -= to_copy;
    len -= to_copy;

    b->bytes += to_copy;

    if(avail == 0)
    {
      b->write_offset = curr = POOL_ALLOC(chunk_t);
      b->tail = chunklist_append(b->tail, curr);

      b->chunks++;

      continue;
    }

    b->write_offset += to_copy;
  }
}

streambuf_t* streambuf_read(streambuf_t* buf, size_t bytes, void* to)
{
  size_t to_copy = 0;

  while(true)
  {
    to_copy = bytes > CHUNK_SIZE ? CHUNK_SIZE : bytes;
    memcpy(to, buf->read_offset, to_copy);

    buf->bytes -= to_copy;
    bytes -= to_copy;
    buf->read_offset += to_copy;

    to += to_copy;

    if(bytes > 0)
    {
      buf->head = chunklist_pop(buf->head, NULL);
      buf->read_offset = chunklist_data(buf->head);

      buf->chunks--;
    }
    else break;
  }

  return buf;
}

void streambuf_vectorize(streambuf_t* buf, struct iovec* iov)
{
  chunklist_t* curr = buf->head;
  chunk_t* end = chunklist_data(buf->tail);

  size_t len = buf->write_offset - (void*)end;

  for(size_t i = 1; i <= buf->chunks; i++)
  {
    chunk_t* data = chunklist_data(curr);

    iov[i-1].iov_base = data;
    iov[i-1].iov_len = (i < buf->chunks) ? CHUNK_SIZE : len;

    curr = chunklist_next(curr);
  }
}

void* streambuf_get_space(streambuf_t** buf, size_t* avail)
{
  streambuf_t* b = *buf;

  if(b == NULL) b = *buf = init();

  chunk_t* n = chunklist_data(b->tail);

  size_t left = CHUNK_SIZE - (b->write_offset - (void*)n);

  if(left > 0)
    *avail = left;
  else
  {
    b->write_offset = n = POOL_ALLOC(chunk_t);
    b->tail = chunklist_append(b->tail, n);

    *avail = CHUNK_SIZE;
    b->chunks++;
  }

  return b->write_offset;
}

bool streambuf_write_advance(streambuf_t* buf, size_t bytes)
{
  chunk_t* curr = chunklist_data(buf->tail);
  size_t left = CHUNK_SIZE - (buf->write_offset - (void*)curr);

  if(left < bytes) return false; //TODO: this should never happen (see get_space)

  buf->write_offset += bytes;
  buf->bytes += bytes;

  return buf->write_offset == ((void*)curr + CHUNK_SIZE);
}

bool streambuf_read_advance(streambuf_t* buf, size_t bytes)
{
  chunk_t* curr = chunklist_data(buf->head);

  size_t left = CHUNK_SIZE - (buf->read_offset - (void*)curr);
  size_t seek = 0;

  while(true)
  {
    seek = left > bytes ? bytes : left;

    buf->read_offset += seek;
    buf->bytes -= seek;

    bytes -= seek;

    if(bytes > 0)
    {
      buf->head = chunklist_pop(buf->head, NULL);
      buf->read_offset = chunklist_data(buf->head);

      left = CHUNK_SIZE;

      continue;
    }

    break;
  }

  return buf->read_offset == buf->write_offset;
}

void streambuf_free(streambuf_t** buf)
{
  if(*buf != NULL)
  {
    chunklist_free((*buf)->head);
    POOL_FREE(streambuf_t, *buf);

    *buf = NULL;
  }
}
