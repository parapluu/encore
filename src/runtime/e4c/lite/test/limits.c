
#include "testing.h"

struct e4c_context * ctx = &E4C_NEW_CTX;
void nest_try_block(int keep_nesting, int * acc);

/**
 * Reach high number of exception frames
 */
TEST_CASE{
    int n = 100;
    int acc = 0;
    nest_try_block(n, &acc);
    int expected = 50*(100+1);
    TEST_ASSERT(acc == expected);
}

void nest_try_block(int keep_nesting, int * acc){

    if(keep_nesting){
        E4C_TRY(ctx){
            *acc += keep_nesting;
            nest_try_block(--keep_nesting, acc);
        }E4C_TRY_END;
    }
}
