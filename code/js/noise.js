var engine = require("./brick_engine")
var data_source = require('./data_source')
var data_source2 = require('./data_source2')
var jsonfile = require('jsonfile')

/*

run this like:
node noise.js torque-local-above 1.5 0.5 0 100
node noise.js dx-local 1.2 0.3 5 50

create a 'simulations/' folder beforehand to avoid errors!
*/

var type = process.argv[2]
var level = process.argv[3]
var fric = process.argv[4]
var trial = process.argv[5]
var numSims = process.argv[6]

var trials = {}
if (trial === '0') {
	trials = data_source.trials;
} else if (trial.split('.').pop() === 'json') {
	trials['trial_'+trial.split('.')[0]] = jsonfile.readFileSync(trial)
} else {
	trials['trial_'+trial] = data_source['trial_'+trial];
}

var noiseSimulation = function (dt) {
	var noiseSims = {}
	for (var i in dt) {
		if (dt.hasOwnProperty(i)) {
			noiseSims[i] = engine.getNoiseResults(dt[i], type, level, fric, 0, numSims, true, false);
		}
	}
	return noiseSims;
}


var file = './simulations/' + type + '_' + level + '_' + fric + '.json';

var sims = noiseSimulation(trials);
jsonfile.writeFile(file, sims, function (err) {
	//console.error(err)
})
