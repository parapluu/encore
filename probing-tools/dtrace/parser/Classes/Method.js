'use strict'

class Method {

	constructor(name, duration) {
		this.name = name;
		this.duration = duration / 1000000.0;
	}

}

module.exports = Method;
