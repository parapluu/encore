### Todo for next sprint

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
