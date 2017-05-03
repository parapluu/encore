This folder contains the implementation of a pedestrian simulation in Encore. The simulation uses input XML files, which by conventions have the name X_scenario.xml, where X is the number of agents they contain. The various test instances uses different files split into different numbers of regions.

Note: The number of parallel regions/active-object workers is the input number squared. So regions(agents,3) will make 9 workers.

Author: David Escher
