### Examples

A number of examples to help write the compiler. 
These are the expected C code for several simple cases.

HelloPonyworld.pony.c: 
	performs printf of simple string in the Main.main method

Countdown.pony.c:
	Adds a field to the main class. Uses this field to do a countdown,
	again printing to screen. 
	
TheOthers.pony.c:
	Adds another class to the mix. Creates an instance of this class,
	calls a method on it. The method prints a greeting and does a countdown.

PrimitiveSend.pony.c:
	Something that sends primitives around

StringSend.pony.c:
	Sends strings around.

ActorSend.pony.c:
	Sends one actor to another. The first sends a message to the second,
	who prints a pleasant message to the screen.

Something that depends on the return value

Something that involves refactoring a class into a separate file.

ArgumentsPassing.pony.c:
	Pass dyn. allocd args around. Make sure that it follows the
	correct protocol of the pony allocator. Need to ask Sylvain!

### Todo for next sprint

* Add passive objects
* Set library in C to use as backend for fields-as-sets
* Integrate this with Pony to make fields into sets
  - add
  - remove (id)
  - call 
* Change field access to work with targets-as-sets
* Change field access to return a set
* Add Fold AST node (fold binop operand set)
* Assignment between fields copies references from
  source set to sink set, removing all existing
  references in the sink 
