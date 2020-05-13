# Part 3
The primary aim of Part 3 is to **maximise the number of oracles that you visit** (as that gives you the best chance of finding your identity).   
The secondary aim is to **minimise the number of steps in the solution**.  
Apart from that you should also ensure that your *code terminates properly*, *does not use or redefine non-exported library predicates*, that it is *not excessively inefficient*, and it is *well-commented*.


## brief
- Part 3 is a combination of Parts 1 and 2. 
- random grids of about 20×20 
- 10 oracles, 2 charging stations and up to about 100 obstacles
- Instead of the special agent “oscar”, you will use a regular agent that you join to the game
- You will have to physically move your agent next to an oracle to ask it a question
- only able to ask each oracle one question
- each query costs 10 energy
- As oracles are independent, they may repeat a link you had previously been given
- Thus, sometimes, you may fail to determine your identity even after visiting the maximum possible number of oracles.


## to-dos
* ~~top up if energy is less than cost~~
* make find_all/1 work
    * by keeping track of oracles
* keep an internal memory of previously discovered oracles and charging stations so that you may quickly direct your agent to a nearby charging station or (unvisited) oracle.


## optimization ideas
* also we should probably include a topup if energy < threshold thing, but as an optimization? because if energy(38) < cost(37), then theres only 1 energy left at which point the agent cant make any moves anymore, which means game over.
* if it passes any charging station on the way to its goal it should do top up, regardless if its less than cost or threshold. basically it should always take the path that passes a charging station.


## fixes
1. the part where it does find_charge works fine, but it does the moves then fails if there's not enough energy, rather than just failing. Should probably fix this idk??? maybe

2. MORE IMPORTANTLY! We have to check that the coordinate for go/ the object for find actually exists, and if not fail at the beginning of solve_task


## terminating properly
should fail if  
* goal is an obstacle
* goal is surround by obstacles (no way to get to it)
* goal has values that aren't on the grid
    (eg: go(p(40,40)) should fail on a 20x20 grid)
    