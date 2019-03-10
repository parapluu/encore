'use strict'
const fs = require('fs');
const xml2js = new require('xml2js').Parser();
const Parser = require('./parser');
const argv = process.argv.slice(2);

if (argv.length == 0) {
	console.log("No input file given.");
	process.exit();
}

fs.readFile(process.cwd() + "/" + argv[0], (err, data) => {

	if (err) {
		console.log(err);
		process.exit()
	}

	xml2js.parseString(data, (err, nodes) => {
		if (err) {
			console.log(err);
			process.exit();
		}

		const parser = new Parser(nodes);
		parser.start();

		const counts = parser.counts;
		const futures = parser.futures;
		const futureGets = parser.futureGets;
		const futureBlocks = parser.blocks;
		const actors = parser.actors;
		const schedulers = parser.schedulers;
		const workStealSuccess = parser.workStealSuccess;
		const workStealFailure = parser.workStealFailure;

		let methods = [];

		for (const key in actors) {
			for (const method in actors[key].methods) {
				methods.push(actors[key].methods[method]);
			}
		}

		let blocks = [];

		for (const key in futureBlocks) {
			for (const block in futureBlocks[key]) {
				blocks.push(futureBlocks[key][block]);
			}
		}

		console.table(counts);
		console.log("--------------------------- ACTORS ---------------------------");
		console.table(actors, ["id", "numberOfGets", "numberOfTimesBlocked", "numberOfTimesStolen"]);
		console.log("--------------------------- FUTURES ---------------------------");
		console.table(futures, ["id", "duration", "numberOfBlocks"]);
		console.log("--------------------------- BLOCKS ---------------------------");
		console.table(blocks, ["futureId", "actorId", "duration"]);
		console.log("--------------------------- FUTURE GETS ---------------------------");
		console.table(futureGets);
		console.log("--------------------------- Schedulers ---------------------------");
		console.table(schedulers, ["id", "successfulSteals", "failedSteals"]);
		console.log("--------------------------- Work steal success ---------------------------");
		console.table(workStealSuccess);
		console.log("--------------------------- Work steal failure ---------------------------");
		console.table(workStealFailure);
		console.log("--------------------------- Methods ---------------------------");
		console.table(methods);

	});

});
