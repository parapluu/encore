
#include "testing.h"

struct e4c_context * ctx = &E4C_NEW_CTX;

/**
 * Uncaught exception
 */
TEST_CASE{

    E4C_THROW(ctx,NullPointerException, "This is an uncaught exception");
}
