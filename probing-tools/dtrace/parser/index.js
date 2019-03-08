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
	});

});
