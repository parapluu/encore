'use strict'
/**
 *  Represents a Method.
 *
 *  This class has the ID of an actor, as well as the name of the method,
 *  and the total number of milliseconds the method was alive.
 */
class Method {

	constructor(actorId, name, duration) {
		this.actorId = actorId;
		this.name = name;
		this.duration = duration / 1000000.0;
	}

}

module.exports = Method;
