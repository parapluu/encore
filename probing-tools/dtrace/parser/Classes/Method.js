'use strict'

class Method {

	constructor(actorId, name, duration) {
		this.actorId = actorId;
		this.name = name;
		this.duration = duration / 1000000.0;
	}

}

module.exports = Method;
