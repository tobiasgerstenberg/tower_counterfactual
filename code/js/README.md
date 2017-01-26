# Tower Counterfactual Simulations

## General Purpose
These experiments test general human perception of structural stability. Participants judge the stability of an arrangement of bricks in an interface by imagining a scenario where a particular brick were absent.

## Dependencies
This repo uses node.js; dependencies (i.e. node modules) include box2dweb, graceful-fs, and jsonfile.

## Interface
Code is in the `interface/` folder, but run everything from the wrapping `node` folder. Run the server by running `node web_interface.js` and navigating to `localhost:8000/interface/physics_interface.html` in your browser. You can choose to run experiment 1 or experiment 2 with varying trials, noise levels, and noise models.

To open the debug interface, navigate to `localhost:8000/interface/physics_debug.html` Here, there are more options, you can animate random brick assortments or load existing brick assortments, remove bricks with noise parameters, etc. Further instructions are in the HTML page.

The bulk of the JavaScript code to run the simulations is in `brick_engine.js`; the HTML acts as a wrapper.

## Simulations
To simulate many different noise parameters at once, simply run

```
node noise.js [options]
```

`data_source.js` contains data from experiment 1; `data_source2.js` contains data from experiment 2 (as noted in the paper).

As an example, you can try `node main.js noise torque-global 1.5 0.4 0 10` which will create a file `torque-global_1.5_0.4.json` in the `simulations` folder.

The first argument is the noise type (`torque-global`), the second argument is the noise level (`1.5`), the third argument is the brick friction level (`0.4`), the fourth argument is which trial(s) to run, from 1 to 42, with `0` as an option to run all trials (`0`). The final argument is how many simulations to run (`10`). Throughout our experiments, we consistently run 100 simulations, but computational time scales linearly with simulation count.

The options are detailed in the file. You can run many instances (i.e. with different noise parameters) by editing `jobs.sh` to generate a list of commands.

The bulk of the heavy-duty JavaScript code is in `brick_engine.js`.

Results of the simluations are under `sim_archives/simulations/` folders; for recent runs, the `run_open.sh` code has been copied into `params.txt`.

## Hacky Areas
Simulating with Box2d is a trick thing - getting precise and completely replicable results, while still being able to save brick positions, was rather difficult. We decided on several standardizations:

- Using the npm port of Box2dweb. We found that the npm port and flash port gave different simulation results.
- After generating sample brick positions and allowing them to settle for 1000 timesteps, we save the data in a json format.
- This data is then reloaded, allowed to settle for 10 timesteps for contacts to form, and then the target brick is removed according to the removal rule we choose.
- Everything is then allowed to settle for 1000 timesteps, and then we record bricks.
- Bricks that are "destroyed" (i.e. fall of the table) are represented by destroying the brick in the world, as well as replacing their x-y position with b2Vec2(-5, -5). This may not be robust to global noise, although this is probably patched.

## Noise Models
There are multiple noise models implemented to create different, random results for the final configuration for the brick arrangements. They are:

- *dxy-local*: Applies a small x-y displacement to bricks contacting the given brick.
- *dxy-local-above*: Applies a small x-y displamcent to bricks contacting the given brick, where the contact location is above the center of mass of the given brick.
- *dx-local*: Applies a small x displacement to bricks contacting the given brick.
- *dx-local-above*: Applies a small x displamcent to bricks contacting the given brick, where the contact location is above the center of mass of the given brick.
- *dxy-global*: Applies a small x-y displacement to all bricks.
- *torque-brick*: Applies a small torque to the given brick, then quickly removes the brick.
- *torque-local*: Applies a small torque to bricks contacting the given brick.
- *torque-local-above*: Applies a small torque to bricks contacting the given brick, where the contact location is above the center of mass of the given brick.
- *torque-global*: Applies a small torque to all bricks.
- *impulse-local*: Applies a small impulse to bricks contacting the given brick.
- *impulse-local-above*: Applies a small impulse to bricks contacting the given brick, where the contact location is above the center of mass of the given brick.
- *impulse-local-above-extended*: Applies a small impulse to bricks contacting the given brick, where the contact location is above the center of mass of the given brick. An impulse is also applied recursively to all bricks that were affected.
- *impulse-global*: Applies a small impulse to all bricks.

## Features
We also tested a variety of features, mostly as a result of running `features.js`.

The world-related features:
- number of bricks above the given brick
- number of bricks above the given brick, with some form of contact
- number of bricks above the given brick, with contacts that must also be recursively above
- distance of given brick to either edge of the table
- average x position on table
- average y position on table
- average angle deviation of bricks from perpendicular axes
- height of the tower

## Stimulus Creation Pipeline
1. Use Adobe Illustrator to create a visual version of the given stimuli. In a horizontal position, a brick should have a width of 80 units and a height of 40 units. Give your "chosen" brick an `id` attribute of `special`. Save this as `stimulus.svg`.
2. Run `bash svg2json/svg2json.sh stimulus.svg stimulus.json`. This is the json file version of your stimulus.
3. Run `bash jobs.sh stimulus.json`. This creates a `run.sh` file which can then be run on Openmind.
4. The results of running on Openmind will be in the `simulations/` folder. Move these to a `sim_archives/simulationsX` folder where `X` increments on the last simulation.
5. Append the contents of `jobs.sh` onto the top of `params.txt`, and note the simulation index number (`X`).

**Note: most of the stimulus testing pipeline has been condensed into `run_stimulus.sh` and `run_stimulus_all.sh`. The difference is that `run_stimulus.sh` runs a given svg; `run_stimulus_all.sh` runs it for all possible combinations of the given brick in the stimulus.
