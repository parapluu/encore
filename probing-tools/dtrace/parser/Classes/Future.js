class Future {

	constructor(id, duration) {
		this.id = id;
		this.duration = duration;
		this.blocks = [];
		this.actorsBlocked = {};
		this.numberOfGets = 0;
		this.numberOfBlocks = 0;
	}

}

module.exports = Future;
