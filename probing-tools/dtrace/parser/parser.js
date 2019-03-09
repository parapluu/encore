'use strict';

const Actor = require('./Classes/Actor.js');
const Future = require('./Classes/Future.js');
const FutureGet = require('./Classes/FutureGet.js');
const FutureBlock = require('./Classes/FutureBlock.js');
const Scheduler = require('./Classes/Scheduler.js');

class Parser {

	constructor(nodes) {
		this.nodes = nodes;
		this.blocks = {};
		this.futures = {};
		this.actors = {};
		this.futureGets = [];
		this.schedulers = {};
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
					default:
						console.log(parent);
						break;
				}
			}
		}

		// for (const id in this.futures) {
		// 	console.log("Future " + this.futures[id].id);
		// 	console.log(`\tDuration: ${this.futures[id].duration} nanoseconds`);
		// 	console.log(`\tNumber of gets: ${this.futures[id].numberOfGets}`);
		// 	console.log(`\tNumber of blocks: ${this.futures[id].numberOfBlocks}`);
		//
		// 	const blocks = this.futures[id].blocks;
		// 	for (const key in blocks) {
		// 		// A future blocking
		// 	}
		//
		// 	const actorsBlocked = this.futures[id].actorsBlocked;
		// 	for (const key in actorsBlocked) {
		// 		// actorsBlocked[key] == number of times the future blocks the actor key
		// 	}
		//
		// 	console.log(this.futures[id]);
		//
		// }
	}

	parseCounts(rootNode) {
		const elements = rootNode[0];
		for (const key in elements) {
			// TODO: Add counts to some data structure
		}
	}

	parseFutures(rootNode) {
		// As there is only one root node for futures
		// we can just get the first element in the list
		const elements = rootNode[0]["future"];

		for (const key in elements) {
			const id = elements[key]['id'][0];
			const duration = elements[key]['duration'][0];
			const future = new Future(id, duration);
			this.futures[id] = future;
		}
	}

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

			const block = new FutureBlock(id, actor, duration);
			this.blocks[id].push(block);

			if (id in this.futures) {
				this.futures[id].blocks.push(block);
			}
		}
	}

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

	parseWorkStealSuccess(rootNode) {
		const schedulers = rootNode[0]["schedulers"];
		for (const key in schedulers) {
			const id    = schedulers[key]["scheduler"][0]["id"][0];
			const count = schedulers[key]["scheduler"][0]["count"][0];

			if (!(id in this.schedulers)) {
				this.schedulers[id] = new Scheduler(id, count, 0);
			} else {
				this.schedulers[id].successfulSteals = count;
			}
		}
	}

	parseWorkStealSuccessFrom(rootNode) {
		const schedulers = rootNode[0]["schedulers"];
		for (const key in schedulers) {
			const byId   = schedulers[key]["scheduler"][0]["id"][0];
			const fromId = schedulers[key]["scheduler"][0]["from"][0];
			const count  = schedulers[key]["scheduler"][0]["count"][0];

			if (!(byId in this.schedulers)) {
				this.schedulers[byId] = new Scheduler(byId, count, 0);
			}

			this.schedulers[byId].stolenFrom[fromId] = count;
		}
	}

}

module.exports = Parser;
