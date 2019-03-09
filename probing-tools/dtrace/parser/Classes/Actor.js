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
