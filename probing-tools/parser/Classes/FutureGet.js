'use strict'
/**
 *  Represents a Future get operation.
 *
 *  This class has the ID of an actor, the id of a future, and the number
 *  of times it has been called (not necessary?).
 *
 *  All these values are integers.
 */
class FutureGet {

	constructor(actor, future, count) {
		this.actor = actor;
		this.future = future;
		this.count  = count;
	}

}

module.exports = FutureGet;
