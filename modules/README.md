This is the location of library code shipped with Encore.

The directory structure is as follow:
- standard  = the location of the standard library
            * this code is tested and the interfaces are relatively stable
- prototype = the location of experimental libraries
            * each library in this package should be named in a hierarch named "Proto.Something"
- joy       = packages downloaded from the interwebs
            * this directory is reserved for that purpose

In the future, this structure will be replicated in ~/.encore/modules for installed versions
of the compiler to find automatically. Downloaded packages will be placed in the joy directory.

For development purposes, the current locations of the modules is used by default.
