
#ifndef __context_h__
#define __context_h__


typedef struct ctx *Ctx;

Ctx ctx_empty(void);

int ctx_capture(Ctx);

void ctx_reinstate(Ctx);

#endif
