
#include "testing.h"

struct e4c_context * ctx = &E4C_NEW_CTX;
E4C_DEFINE_EXCEPTION(CustomException, RuntimeException);


/**
 * Caught exception
 */
 TEST_CASE{

    E4C_TRY(ctx){

        E4C_THROW(ctx,CustomException, "This is a custom exception");

    }E4C_CATCH(ctx,CustomException){

      printf("The custom exception was caught: %s\n", E4C_EXCEPTION(ctx).type->name);
    }E4C_TRY_END;
}
