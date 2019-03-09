'use strict'

class Scheduler {

	constructor(id, successfulSteals, failedSteals) {
		this.id = id;
		this.successfulSteals = successfulSteals;
		this.failedSteals = failedSteals;
		this.stolenFrom = [];
		this.failedToStealFrom = [];
	}

}

module.exports = Scheduler;
