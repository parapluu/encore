'use strict'
/**
 *  Represents a schedulers steal.
 *
 *  This class has the IDs of the schedulers and the number of times the
 *  scheduler has stolen from the other scheduler.
 */
class WorkSteal {

	constructor(byId, fromId, count) {
		this.scheduler = byId;
		this.victim = fromId;
		this.count = count;
	}

}

module.exports = WorkSteal;
