
#include "testing.h"

struct e4c_context * ctx = &E4C_NEW_CTX;

/**
 * Cleanup
 */
TEST_CASE{

    volatile int created     = 0;
    volatile int destroyed   = 0;
    volatile int started     = 0;
    volatile int finished    = 0;

    E4C_TRY(ctx){

        created = 1;

        E4C_TRY(ctx){

            started = 1;

            E4C_THROW(ctx,NullPointerException, "Get me out of here");

            finished = 1; /* this should not happen */

        }E4C_FINALLY(ctx){

            destroyed = 1;
        }E4C_TRY_END;

    }E4C_CATCH(ctx,NullPointerException){

        printf("No problem :-)");
    }E4C_TRY_END;

    TEST_ASSERT(created);
    TEST_ASSERT(started);
    TEST_ASSERT(!finished);
    TEST_ASSERT(destroyed);
}
