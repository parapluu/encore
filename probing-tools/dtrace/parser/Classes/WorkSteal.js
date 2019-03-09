'use strict'

class WorkSteal {

	constructor(byId, fromId, count) {
		this.scheduler = byId;
		this.victim = fromId;
		this.count = count;
	}

}

module.exports = WorkSteal;
