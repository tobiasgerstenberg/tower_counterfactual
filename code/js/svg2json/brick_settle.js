var Box2D = require('box2dweb');
var jsonfile = require('jsonfile');

//box2d variables             
var b2Vec2 = Box2D.Common.Math.b2Vec2;
var b2BodyDef = Box2D.Dynamics.b2BodyDef;
var b2Body = Box2D.Dynamics.b2Body;
var b2FixtureDef = Box2D.Dynamics.b2FixtureDef;
var b2World = Box2D.Dynamics.b2World;
var b2PolygonShape = Box2D.Collision.Shapes.b2PolygonShape;
var b2WorldManifold = Box2D.Collision.b2WorldManifold;

var gravity = 10;
var scale = 100;
var ratio = 1;

var fullX = 8; 
var fullY = 6;	

//variables 
var brick_density = 10;
var default_brick_friction = 0.5;
var brick_friction = 0.5;
var brick_restitution = 0.2;
var bricks_num_default = 20;
var brick_width = 0.4;
var brick_height = 0.2;

var table_density = 10;
var table_friction = 1;
var table_restitution = 0.2;

//placeholders 
var world;
var stage;
var bodies = []; // instances of b2Body (from Box2D)
var actors = []; // instances of Bitmap (from IvanK)
var bricks_array = [];
var brick_special = 0;

//control variables 
var settle_time = 1000;
var brick_init_positions;
var brick_positions_loaded;
var stop_simulation;
var timer = 0;
var ffRate = 1;

var visualize = false;
var loading_data = true;

// sets up the world by creating the stage if it doesn't exist, table, ground, etc.
// this is always done when loading data and generating brick positions
function setupWorld() {
	world = new b2World(new b2Vec2(0, gravity), false); //gravity in Y direction

	var centerX = fullX / 2;
	var centerY = fullY / 2;

	//ground
	createBox(width = fullX / 2,
		height = 0.1,
		x = centerX,
		y = fullY - 0.1,
		angle = 0,
		type = b2Body.b2_staticBody,
		density = 10,
		friction = 1,
		restitution = 0,
		userData = "box",
		img = "figures/ground.png"
	);

	//table
	createBox(0.1, 0.4, centerX - 1, fullY - 0.4 - 0.2, 0, b2Body.b2_staticBody, table_density, table_friction, table_restitution, "left_leg", "figures/table_leg.png");
	createBox(0.1, 0.4, centerX + 1, fullY - 0.4 - 0.2, 0, b2Body.b2_staticBody, table_density, table_friction, table_restitution, "right_leg", "figures/table_leg.png");
	createBox(1.5, 0.1, centerX, fullY - 0.1 - 0.80 - 0.18, 0, b2Body.b2_staticBody, table_density, table_friction, table_restitution, "table", "figures/table_top.png");
	settle(100);
}

//create a box (brick) of given parameters
function createBox(width, height, x, y, angle, type, density, friction, restitution, userData, img) {
	// Create the fixture definition
	var fixDef = new b2FixtureDef;
	fixDef.density = density; // Set the density
	fixDef.friction = friction; // Set the friction
	fixDef.restitution = restitution; // Set the restitution - bounciness
	fixDef.shape = new b2PolygonShape;
	fixDef.shape.SetAsBox(width,height);

	// Create the body definition
	var bodyDef = new b2BodyDef;
	bodyDef.type = type;
	bodyDef.position.x = x;
	bodyDef.position.y = y;
	bodyDef.angle = angle;
	bodyDef.linearDamping = 1;

	// Create the body in the box2d world
	var b = world.CreateBody(bodyDef);
	b.CreateFixture(fixDef);
	if (typeof userData !== 'undefined') {
		b.SetUserData(userData);
	}

	return b;
}


//allow bricks to settle, without animation
function settle(steps) {
	var numSteps = (typeof steps !== 'undefined') ? steps : settle_time;
	for (var j = 0; j < numSteps; j++) {
		world.Step(1 / 60, 5, 5);
		world.ClearForces();
		removeDeadBricks();
	}
}

//get rid of bricks that have fallen below the table
function removeDeadBricks() {
	bricks_array.forEach(function (brick) {
		if (brick.GetPosition().y > 5) {
			world.DestroyBody(brick);
			brick.SetPosition(new b2Vec2(-5, -5));
		}
	})
}


// /*

// HELPER FUNCTIONS *********************************************

// */


//counts number of bricks still on the table
function countGoodBricks() {
	var b = 0;
	bricks_array.forEach(function (brick) {
		if (brick.GetPosition().x !== -5) {
			b++;
		}
	})
	return b;
}
//converts current array of bricks into JSON
function pushData() {
	array = []
	bricks_array.forEach(function (brick) {
		array.push({
			"name": brick.GetUserData(),
			"x": brick.GetPosition().x,
			"y": brick.GetPosition().y,
			"angle": brick.GetAngle()
		});
	})
	return array;
}
//gives default value for a variable
function defaultFor(arg, val) {
	return typeof arg !== 'undefined' ? arg : val;
}

//resets brick array and loads data into the brick array
function loadData(data) {
	setupWorld();
	bricks_array = [];
	for (var i = 0; i < data.length; i++) {
		bricks_array.push(
			createBox(brick_width, brick_height, data[i].x, data[i].y, data[i].angle, b2Body.b2_dynamicBody, brick_density, brick_friction, brick_restitution, data[i]['name'], null)
		);
		if (data[i].x === -5) {
			world.DestroyBody(bricks_array[bricks_array.length - 1]);
		}
	}
}

args = process.argv
if (args.length > 3) {
	for (var i = 0; i < args[3];i++) {
		obj = jsonfile.readFileSync('tmp_'+i+'.json')
		loadData(obj);
		settle();
		jsonfile.writeFileSync(args[2]+'_'+i+'.json', pushData());
	}
} else {
	jsonfile.readFile('tmp.json', function(err, obj) {
		loadData(obj);
		settle();
		jsonfile.writeFile(process.argv[2], pushData(), function (err) {
		})
	})
}
