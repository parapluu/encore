#define __STDC_FORMAT_MACROS
#include <pony/pony.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <inttypes.h>
#include <assert.h>
#include "future_actor.h"
#include "future.h"

typedef struct yielded_entry {
    pony_actor_t *actor;
    void *resume;
} yielded_entry;

typedef struct chained_entry {
    pony_actor_t *actor;
    void *closure;
} chained_entry;


// FIXME -- 2nd arg in chain should be a closure, not an actor!
static pony_msg_t m_chain = {2,
    {
        {NULL, 0, PONY_ACTOR},
        {NULL, 0, PONY_ACTOR}}};
// FIXME -- 2nd arg in chain should be a resume message, not an actor!
static pony_msg_t m_yield = {2,
    {
        {NULL, 0, PONY_ACTOR},
        {NULL, 0, PONY_ACTOR}}};
static pony_msg_t m_block = {1,
    {
        {NULL, 0, PONY_ACTOR}}};
// FIXME -- 2nd arg in chain should be any kind of Encore value
static pony_msg_t m_fulfil = {2,
    {
        {NULL, 0, PONY_ACTOR},
        {NULL, 0, PONY_ACTOR}}};

pony_msg_t* future_actor_message_type(uint64_t id) {
    switch (id) {
        case MSG_CHAIN: return &m_chain;
        case MSG_BLOCK: return &m_block;
        case MSG_YIELD: return &m_yield;
        case MSG_FULFIL: return &m_fulfil;
    }

    return NULL;
}

static void init(pony_actor_t* this) {
    if (this->p) return;
    pony_set(pony_alloc(sizeof (future_actor_fields)));
    // FIXME: are the following inits really necessary? (is memory nulled from start?)
    future_actor_fields *fields = this->p;
    fields->blocked = NULL;
    fields->chained = NULL;
    fields->yielded = NULL;
}

static Set getBlocked(pony_actor_t* this) {
    future_actor_fields *fields = this->p;
    if (fields->blocked == NULL) {
        fields->blocked = set_new();
    }
    return fields->blocked;
}

static Set getChained(pony_actor_t* this) {
    future_actor_fields *fields = this->p;
    if (fields->chained == NULL) {
        fields->chained = set_new();
    }
    return fields->chained;
}

static Set getYielded(pony_actor_t* this) {
    future_actor_fields *fields = this->p;
    if (fields->yielded == NULL) {
        fields->yielded = set_new();
    }
    return fields->yielded;
}

void future_actor_dispatch(pony_actor_t* this, void* p, uint64_t id, int argc, pony_arg_t* argv) {
    init(this);

    switch (id) {
        case MSG_CHAIN:
        {
            // TODO: Add the closure argument to an internal list of closures 
            // This entry should record the closure and the actor to run it      
            fprintf(stderr, "Chaining on a future");
            Set chained = getChained(this);
            chained_entry *new_entry = pony_alloc(sizeof(chained_entry));
            new_entry->actor = argv[0].p;
            new_entry->closure = argv[1].p;
            set_add(chained, new_entry);
            break;
        }
        case MSG_BLOCK:
        {
            // TODO: Record the actor as one that needs to be woken up when the
            // future value is set
            fprintf(stderr, "Blocking on a future");
            Set blocked = getBlocked(this);
            set_add(blocked, argv[0].p);
            break;
        }
        case MSG_YIELD:
        {
            // TODO: Record the actor as one that should be sent the resume 
            // message (2nd argument) when the future value is set
            fprintf(stderr, "Yielding on a future");
            Set yielded = getYielded(this);
            yielded_entry *new_entry = pony_alloc(sizeof(yielded_entry));
            new_entry->actor = argv[0].p;
            new_entry->resume = argv[1].p;
            set_add(yielded, new_entry);
            break;
        }
        case MSG_FULFIL:
        {
            fulfil((future*) this, argv[1].p);
            // TODO: 
            // - put all blocking actors back into the scheduler queue
            // - call all actors with chained closures  with the corresponding closures (see https://trello.com/c/kod5Ablj)
            // - send the appropriate resume messages to all yielding actors
            fprintf(stderr, "Fulfilling on a future");
            // Set chained = getChained(this);
            // Set blocked = getBlocked(this);
            // Set yielded = getYielded(this);
            // For unblocking, see pony's scheduler.c line 70
            break;
        }
    }
}

