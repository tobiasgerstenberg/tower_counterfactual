var engine = require("./brick_engine")
var data_source = require('./data_source')
var data_source2 = require('./data_source2')
var jsonfile = require('jsonfile')

/*

run this like:
node features.js [world|brick]

create a 'features/' folder beforehand to avoid errors!
*/

var option = process.argv[2]

var trials = data_source2.trials;

var worldFeatureSimulation = function (dt) {
	var worldFeatList = {}
	for (var i in dt) {
		if (dt.hasOwnProperty(i)) {
			worldFeatList[i] = engine.getWorldFeatures(dt[i]);
		}
	}
	return worldFeatList;
}

var brickFeatureSimulation = function (dt) {
	var brickFeatList = []
	for (var i in dt) {
		worldBricks = []
		if (dt.hasOwnProperty(i)) {
			for (var j = 0; j < 20; j++) {
				if (dt[i][j]['x'] !== -5) {
					worldBricks.push(engine.getBrickFeatures(dt[i], j))
				}
			}
		}
		brickFeatList.append(worldBricks)
	}
	return brickFeatList;
}

if (option === 'world') {
	var file = './features/world.json';
	var feats = worldFeatureSimulation(trials);
	jsonfile.writeFile(file, feats, function (err) {
		console.error(err)
	})
} else if (option === 'brick') {
	var brickFeats = brickFeatureSimulation(trials);
	for (var i = 0; i < trials.length; i++) {
		jsonfile.writeFile('./features/trial_'+i, brickFeats[i], function (err) {
			console.error(err)
		})
	}
}
