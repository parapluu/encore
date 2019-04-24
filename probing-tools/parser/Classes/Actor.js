'use strict'
/**
 *  Represents an Encore Actor
 *
 *  This class has the ID of an actor, as well as the number of times the
 *  actor is blocked by a future, the number of times the actor calls get
 *  and the number of times a scheduler steals it from another scheduler.
 *
 *  All these values are integers.
 *
 *  The actor object also holds references to Method objects, representing
 *  the methods of an actor called.
 */
class Actor {

	constructor(id) {
		this.id = id;
		this.numberOfTimesBlocked = 0;
		this.numberOfGets = 0;
		this.numberOfTimesStolen = 0;
		this.methods = {};
	}

}

module.exports = Actor;
