== Extension to PonyRT from UU side.

`./actor` contains the modified `actor.c`, which includes all the changes to
achieve coroutine and future. It's rather intrusive modification to PonyRT.
Given that we are integrating with the new PonyRT, it's good time to rethink so
that the modification to PonyRT could be limited in a manageable manner.

`./actor` is not used in the build process, and is only used to provide as one
quick way to compare the modified `actor.c` with the one in new PonyRT.
