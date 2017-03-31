
#include "testing.h"

struct e4c_context * ctx = &E4C_NEW_CTX;

/**
 * Simple nesting
 */
TEST_CASE {
  int check = 1;
  E4C_TRY(ctx) {
    check += 2;
    E4C_TRY(ctx) {
      check += 4;
      E4C_THROW(ctx,RuntimeException,"woops2");
    } E4C_CATCH(ctx,RuntimeException) {
      check += 8;
      E4C_THROW(ctx,RuntimeException,"woops1");
    } E4C_TRY_END;
  } E4C_CATCH(ctx,RuntimeException) {
    check += 16;
  } E4C_TRY_END;
  TEST_ASSERT(check == 31);
}
