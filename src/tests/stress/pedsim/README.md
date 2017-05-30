This folder contains the implementation of a pedestrian simulation in Encore. The simulation uses input XML files, which by conventions have the name X_scenario.xml, where X is the number of agents they contain. The various test instances uses different files split into different numbers of regions.

The benchmark is used like this:

regions(A,B,S)

Where A is an array of agents, which can be created by calling parse_file("...") for some valid instance file, B is the square rot of the number of parallel regions to be created, and S is the number of time steps to simulate. 

Note: The number of parallel regions/active-object workers is the input number squared. So regions(agents,3,100) will make 9 workers.

Author: David Escher
