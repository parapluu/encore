/*
 * exceptions4c lightweight version 2.0
 *
 * Copyright (c) 2016 Guillermo Calvo
 * Licensed under the GNU Lesser General Public License
 */

#include <stdlib.h>
#include <stdio.h>
#include "e4c_lite.h"

E4C_DEFINE_EXCEPTION(RuntimeException, RuntimeException);
E4C_DEFINE_EXCEPTION(NullPointerException, RuntimeException);

static const char * err_msg[] = {"\n\nError: %s (%s)\n\n", "\n\nUncaught %s: %s\n\n    thrown at %s:%d\n\n"};

void e4c_fail(e4c_context_t * ctx){
    if(fprintf(stderr, ctx->err.file ? err_msg[1] : err_msg[0], ctx->err.type->name, ctx->err.message, ctx->err.file, ctx->err.line) > 0){
        (void)fflush(stderr);
    }
    exit(EXIT_FAILURE);
}

static void e4c_propagate(e4c_context_t * ctx){
    if(ctx->frame != NULL) {
        ctx->frame->uncaught = 1;
        ctx->unpropagated = 0;
        longjmp(ctx->frame->jump, 1);
    }
    ctx->unpropagated = 1;
}

int e4c_hook(e4c_context_t * ctx, int is_catch){
    if(is_catch){
        ctx->frame->uncaught = 0;
        return 1;
    }

    ctx->frame->stage++;
    if(ctx->frame->stage == e4c_catching && !ctx->frame->uncaught){
        ctx->frame->stage++;
    }

    if(ctx->frame->stage < e4c_done){
        return 1;
    }

    if(ctx->frame->uncaught){
        ctx->frame = ctx->frame->prev;
        e4c_propagate(ctx);
    } else {
        ctx->frame = ctx->frame->prev;
    }

    return 0;
}

int e4c_extends(const e4c_exception_type_t * child, const e4c_exception_type_t * parent){

    for(; child && child->supertype != child; child = child->supertype){
        if(child->supertype == parent){
            return 1;
        }
    }

    return 0;
}

void e4c_throw(e4c_context_t * ctx, const e4c_exception_type_t * exception_type, char * file, int line, char * message, void * exception_obj){
    ctx->err.type = (exception_type ? exception_type : &NullPointerException);
    ctx->err.file = file;
    ctx->err.line = line;
    ctx->err_obj  = exception_obj;

    (void)sprintf(ctx->err.message, "%.*s", (int)E4C_MESSAGE_SIZE - 1, (message ? message : NULL));

    e4c_propagate(ctx);
}

