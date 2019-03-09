'use strict'
/**
 *  Represents an Encore Future
 *
 *  This class has the ID of a future, the duration the future is active
 *  as well as the number of actors it has blocked, the total number of
 *  times actors has called a get on it.
 *
 *  All these values are integers.
 *
 *  The future object also holds references to FutureBlock objects, representing
 *  a future blocking.
 */
class Future {

	constructor(id, duration) {
		this.id = id;
		this.duration = duration / 1000000.0;
		this.blocks = [];
		this.actorsBlocked = {};
		this.numberOfGets = 0;
		this.numberOfBlocks = 0;
	}

}

module.exports = Future;
