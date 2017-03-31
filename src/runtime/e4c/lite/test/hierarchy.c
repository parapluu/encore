
#include "testing.h"

struct e4c_context * ctx = &E4C_NEW_CTX;
E4C_DEFINE_EXCEPTION(ColorException, RuntimeException);
E4C_DEFINE_EXCEPTION(RedException, ColorException);


/**
 * Exception hierarchy
 */
TEST_CASE{

    E4C_TRY(ctx){

      E4C_THROW(ctx,RedException, "This is a red exception");

        TEST_FAIL; /* this should not happen */

    }E4C_CATCH(ctx,ColorException){

        printf("The color exception was caught: %s\n", E4C_EXCEPTION(ctx).type->name);

        TEST_ASSERT( E4C_IS_INSTANCE_OF(ctx,RedException) );
        TEST_ASSERT( E4C_IS_INSTANCE_OF(ctx,RuntimeException) );
    }E4C_TRY_END;
}
