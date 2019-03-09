'use strict';

const Actor = require('./Classes/Actor.js');
const Method = require('./Classes/Method.js');
const Future = require('./Classes/Future.js');
const FutureGet = require('./Classes/FutureGet.js');
const FutureBlock = require('./Classes/FutureBlock.js');
const Scheduler = require('./Classes/Scheduler.js');
const WorkSteal = require('./Classes/WorkSteal.js');

class Parser {

	constructor(nodes) {
		this.nodes = nodes;
		this.counts = {};
		this.blocks = {};
		this.futures = {};
		this.actors = {};
		this.futureGets = [];
		this.schedulers = {};
		this.workStealSuccess = [];
		this.workStealFailure = [];
	}

	start() {
		for (const root in this.nodes) {
			for (const parent in this.nodes[root]) {
				const elements = this.nodes[root][parent];
				switch (parent) {
					case "counts":
						this.parseCounts(elements);
						break;
					case "futures":
						this.parseFutures(elements);
						break;
					case "future-blocks":
						this.parseFutureBlocks(elements);
						break;
					case "future-gets":
						this.parseFutureGets(elements);
						break;
					case "work-steal-success":
						this.parseWorkStealSuccess(elements);
						break;
					case "work-steal-success-from":
						this.parseWorkStealSuccessFrom(elements);
						break;
					case "work-steal-failure":
						this.parseWorkStealFailure(elements);
						break;
					case "work-steal-failure-from":
						this.parseWorkStealFailureFrom(elements);
						break;
					case "actor-stolen":
						this.parseActorStolen(elements);
						break;
					case "methods":
						this.parseMethods(elements);
						break;
					default:
						console.log("Error: Unknown tag: " + parent);
						break;
				}
			}
		}
	}
	/**
	 * Parses the <counts> element.
	 * @param  Object   rootNode  The <counts> root node.
	 */
	parseCounts(rootNode) {
		const elements = rootNode[0];
		for (const key in elements) {
			const count = elements[key][0]["$"]["count"];
			this.counts[key] = parseInt(count);
		}
	}
	/**
	 * Parses the <futures> element.
	 * @param  Object   rootNode  The <counts> root node.
	 */
	parseFutures(rootNode) {
		// As there is only one root node for futures
		// we can just get the first element in the list
		const elements = rootNode[0]["future"];

		for (const key in elements) {
			const id = elements[key]['id'][0];
			const duration = elements[key]['duration'][0];
			const future = new Future(id, parseInt(duration));
			this.futures[id] = future;
		}
	}
	/**
	 * Parses the <future-blocks> element.
	 * @param  Object   rootNode  The <counts> root node.
	 */
	parseFutureBlocks(rootNode) {
		const elements = rootNode[0];

		for (const key in elements) {
			switch (key) {
				case "future-block-lifetime":
					this.parseFutureBlockLifetime(elements[key]);
					break;
				case "future-block-actor-count":
					this.parseFutureBlockActorCount(elements[key]);
					break;
				case "future-block-count":
					this.parseFutureBlockCount(elements[key]);
					break;
				case "actor-block-count":
					this.parseActorBlockCount(elements[key]);
					break;
				default:
					console.log("Error: Unknown tag: " + key);
					break;
			}
		}
	}
	/**
	 * Parses the <future-block-lifetime> element.
	 * @param  Object   elements  The root node.
	 */
	parseFutureBlockLifetime(elements) {
		for (let key in elements) {
			const element  = elements[key]["future"][0];
			const id       = element['id'][0];
			const actorID  = element['actor'][0]["id"][0];
			const duration = element['duration'][0];
			let actor      = this.actors[actorID];

			if (actor == null) {
				actor = new Actor(actorID);
				this.actors[actorID] = actor;
			}

			if (!(id in this.blocks)) { this.blocks[id] = []; }

			const block = new FutureBlock(id, actor, parseInt(duration));
			this.blocks[id].push(block);

			if (id in this.futures) {
				this.futures[id].blocks.push(block);
			}
		}
	}
	/**
	 * Parses the <future-block-actor-count> elements.
	 * @param  Object   elements  The root node.
	 */
	parseFutureBlockActorCount(elements) {
		for (let key in elements) {
			const element = elements[key]["future"][0];
			const id      = element['id'][0];

			if (id in this.futures) {
				const actor = element['actor'][0]["id"][0];
				const count = element['count'][0];
				let future  = this.futures[id];
				future.actorsBlocked[actor] = parseInt(count);
			}
		}
	}
	/**
	 * Parses the <future-block-count> elements.
	 * @param  Object   elements  The root node.
	 */
	parseFutureBlockCount(elements) {
		for (let key in elements) {
			const element = elements[key]["future"][0];
			const id      = element['id'][0];

			if (id in this.futures) {
				const count = element['count'][0];
				let future  = this.futures[id];
				future.numberOfBlocks = parseInt(count);
			}
		}
	}
	/**
	 * Parses the <actor-block-count> elements.
	 * @param  Object   elements  The root node.
	 */
	parseActorBlockCount(elements) {
		for (let key in elements) {
			const element = elements[key]['actor'][0];
			const id      = element['id'][0];
			const count   = element['count'][0];

			if (!(id in this.actors)) {
				this.actors[id] = new Actor(id);
			}

			this.actors[id].numberOfTimesBlocked = parseInt(count);
		}
	}
	/**
	 * Parses the <future-get> elements.
	 * @param  Object   rootNode  The root node.
	 */
	parseFutureGets(rootNode) {
		// As there is only one node for future-gets
		// we can just get the first element in the list
		const futures = rootNode[0]["future-get"];

		for (const key in futures) {
			const actorID  = futures[key]["actor"][0]["id"][0];
			const futureID = futures[key]["future"][0]["id"][0];
			const count    = futures[key]["count"][0];

			if (!(actorID in this.actors)) {
				this.actors[actorID] = new Actor(actorID);
			}
			if (!(futureID in this.futures)) {
				this.futures[futureID] = new Future(futureID, -1);
			}

			this.actors[actorID].numberOfGets += 1;
			this.futures[futureID].numberOfGets += 1;

			const futureGet = new FutureGet(this.actors[actorID], this.futures[futureID]);
			this.futureGets.push(futureGet);
		}
	}
	/**
	 * Parses the <work-steal-success> element.
	 * @param  Object   rootNode  The root node.
	 */
	parseWorkStealSuccess(rootNode) {
		const schedulers = rootNode[0]["schedulers"][0]["scheduler"];
		for (const key in schedulers) {
			const id    = schedulers[key]["id"][0];
			const count = schedulers[key]["count"][0];

			if (!(id in this.schedulers)) {
				this.schedulers[id] = new Scheduler(id, parseInt(count), 0);
			} else {
				this.schedulers[id].successfulSteals = parseInt(count);
			}
		}
	}
	/**
	 * Parses the <work-steal-success-from> element.
	 * @param  Object   rootNode  The root node.
	 */
	parseWorkStealSuccessFrom(rootNode) {
		const schedulers = rootNode[0]["schedulers"][0]["scheduler"];
		for (const key in schedulers) {
			const byId   = schedulers[key]["id"][0];
			const fromId = schedulers[key]["from"][0];
			const count  = schedulers[key]["count"][0];

			if (!(byId in this.schedulers)) {
				this.schedulers[byId] = new Scheduler(byId, parseInt(count), 0);
			}

			this.workStealSuccess.push(new WorkSteal(byId, fromId, parseInt(count)));
			this.schedulers[byId].stolenFrom[fromId] = parseInt(count);
		}
	}
	/**
	 * Parses the <work-steal-failure> element.
	 * @param  Object   rootNode  The root node.
	 */
	parseWorkStealFailure(rootNode) {
		const schedulers = rootNode[0]["schedulers"][0]["scheduler"];
		for (const key in schedulers) {
			const id    = schedulers[key]["id"][0];
			const count = schedulers[key]["count"][0];

			if (!(id in this.schedulers)) {
				this.schedulers[id] = new Scheduler(id, 0, parseInt(count));
			} else {
				this.schedulers[id].failedSteals = parseInt(count);
			}
		}
	}
	/**
	 * Parses the <work-steal-failure-from> element.
	 * @param  Object   rootNode  The root node.
	 */
	parseWorkStealFailureFrom(rootNode) {
		const schedulers = rootNode[0]["schedulers"][0]["scheduler"];
		for (const key in schedulers) {
			const byId   = schedulers[key]["id"][0];
			const fromId = schedulers[key]["from"][0];
			const count  = schedulers[key]["count"][0];

			if (!(byId in this.schedulers)) {
				this.schedulers[byId] = new Scheduler(byId, 0, parseInt(count));
			}

			this.workStealFailure.push(new WorkSteal(byId, fromId, parseInt(count)));
			this.schedulers[byId].failedToStealFrom[fromId] = parseInt(count);
		}
	}
	/**
	 * Parses the <actor-stolen> elements.
	 * @param  Object   rootNode  The root node.
	 */
	parseActorStolen(rootNode) {
		const actors = rootNode[0]["actor"];
		for (const key in actors) {
			const id    = actors[key]["id"][0];
			const count = actors[key]["count"][0];

			if (!(id in this.actors)) {
				this.actors[id] = new Actor(id);
			}

			this.actors[id].numberOfTimesStolen = parseInt(count);
		}
	}
	/**
	 * Parses the <methods> element.
	 * @param  Object   rootNode  The root node.
	 */
	parseMethods(rootNode) {
		const methods = rootNode[0]["method"];
		for (const key in methods) {
			const id       = methods[key]["actor"][0]["id"][0];
			const name     = methods[key]["name"][0];
			const duration = methods[key]["duration"][0];

			if (!(id in this.actors)) {
				this.actors[id] = new Actor(id);
			}

			this.actors[id].methods[name] = new Method(name, parseInt(duration));
		}
	}

}

module.exports = Parser;
