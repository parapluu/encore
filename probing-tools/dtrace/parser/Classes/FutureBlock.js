class FutureBlock {

	constructor(id, actor, duration) {
		this.futureId = id;
		this.actor = actor;
		this.duration = duration / 1000000.0;
		this.actorId = actor.id;
	}

}

module.exports = FutureBlock;
