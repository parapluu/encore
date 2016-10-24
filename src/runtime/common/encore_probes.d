provider encore {
  /**
   * Fired when a future blocks an actor
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__block(uintptr_t ctx, uintptr_t fut);

  /**
   * Fired when a future is about to be chained
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   * @param type the pony type being returned by the future
   */
  probe future__chaining(uintptr_t ctx, uintptr_t fut, uintptr_t type);

  /**
   * Fired when a future has been created
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   * @param type the pony type of the chained future
   */
  probe future__create(uintptr_t ctx, uintptr_t fut, uintptr_t type);

  /**
   * Fired when a future is being destroyed
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__destroy(uintptr_t ctx, uintptr_t fut);

  /**
   * Fired when a future is starting to be fulfilled
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__fulfil_start(uintptr_t ctx, uintptr_t fut);

  /**
   * Fired when a future has been fulfilled
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__fulfil_end(uintptr_t ctx, uintptr_t fut);

  /**
   * Fired when a future's value is being returned
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__get(uintptr_t ctx, uintptr_t fut);

  /**
   * Fired when a future unblocks an actor
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__unblock(uintptr_t ctx, uintptr_t fut);
};
