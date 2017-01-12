provider encore {
  /**
   * Fired when a closure has been created
   * @param ctx the pony context
   * @param closure the pointer to the closure structure
   */
  probe closure__create(uintptr_t ctx, uintptr_t closure);

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
  probe future__fulfil__start(uintptr_t ctx, uintptr_t fut);

  /**
   * Fired when a future has been fulfilled
   * @param ctx the pony context
   * @param fut the pointer to the future structure
   */
  probe future__fulfil__end(uintptr_t ctx, uintptr_t fut);

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

  /**
   * Fired on field access
   * @param name the name of the field
   */
  probe field__access__probe(uintptr_t scheduler, uintptr_t name);

  /**
   * Fired on field write
   * @param name the name of the field
   */
  probe field__write__probe(uintptr_t scheduler, uintptr_t name);

  /**
   * Fired on method call
   * @param name method name
   * @param args arguments used
   */
  probe method__call__probe(uintptr_t scheduler, uintptr_t name, uintptr_t args);

  /**
   * Fired on method entry
   * @param name method name
   * @param args arguments used
   */
  probe method__entry__probe(uintptr_t scheduler, uintptr_t name, uintptr_t args);

  /**
   * Fired on method exit
   * @param name method name
   */
  probe method__exit__probe(uintptr_t scheduler, uintptr_t name);

  /**
   * Fired on function call
   * @param name function name
   * @param args arguments used
   */
  probe function__call__probe(uintptr_t scheduler, uintptr_t name, uintptr_t args);

  /**
   * Fired on function entry
   * @param name function name
   * @param args arguments used
   */
  probe function__entry__probe(uintptr_t scheduler, uintptr_t name, uintptr_t args);

  /**
   * Fired on function exit
   * @param name function name
   */
  probe function__exit__probe(uintptr_t scheduler, uintptr_t name);

  /**
   * Fired on closure call
   * @param name closure name
   * @param args arguments used
   */
  probe closure__call__probe(uintptr_t scheduler, uintptr_t name,  uintptr_t args);

  /**
   * Fired on closure entry
   * @param name closure name
   */
  probe closure__entry__probe(uintptr_t scheduler, uintptr_t name);

  /**
   * Fired on closure exit
   */
  probe closure__exit__probe();
};
