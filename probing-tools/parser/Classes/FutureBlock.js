'use strict'
/**
 *  Represents a block made by a Future.
 *
 *  This class has the ID of an actor, the ID of a future, the duration of
 *  the block. This object also holds a reference to the Actor object.
 */
class FutureBlock {

	constructor(id, actor, duration) {
		this.futureId = id;
		this.actor = actor;
		this.duration = duration / 1000000.0;
		this.actorId = actor.id;
	}

}

module.exports = FutureBlock;
