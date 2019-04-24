'use strict'
/**
 *  Represents a Scheduler
 *
 *  This class has the ID of a scheduler, the number of times it has successfully
 *  and unsuccessfully stolen work from another scheduler , as well as the a list
 *  of which schedulers it has stolen or attempted to steal from.
 */
class Scheduler {

	constructor(id, successfulSteals, failedSteals) {
		this.id = id;
		this.successfulSteals = successfulSteals;
		this.failedSteals = failedSteals;
		this.stolenFrom = {};
		this.failedToStealFrom = {};
	}

}

module.exports = Scheduler;
