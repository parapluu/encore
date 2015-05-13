
## Implementation Strategy

1. Implement Par data structure as C union.
2. Implement sequential versions of core functions, as given in Haskell code, using mock versions of futures, tasks, etc.
3. Implement support for required kind of tasks and new operations on futures in Encore backend
3. Start integrating party types with Encore backend.
4. Develop language primitives.
6. Develop type checker for language primitives
7. Extend code generation for language primitives
8. Experiment with different evaluation strategies -- when to use fork, when to merge futures (= merging computations)


## M data contructor

The idea of the M data constructor for the Par types is to have an array of size `s`
with an index of how much has been used.

M :: Int -> Int -> Array -> Par a
     total size -> used -> Array -> Par a

Needs to figure out the details for joining a Par M to some other Par t type, etc.

## Prune combinator

The problem of the prune combinator arises when there are two / more Par t types
of Fut Maybe t. In this case, it's not enough to get the first result that finishes
from the Par ts (parties) since a value of `Nothing` is of no use. Therefore,
we need to assign the value of the first Par t that finishes or the last Par t
(since all the previous ones returned Nothing). How do we know which one is the last one?

## Chaining on Fut Fut t

when there's some chaining on a Par t of Fut Fut t, the task runner that fulfils
the future needs to pass the continuation (chained lambda) to the next lambda. In a way,
we would like to execute the lambda on the type t, and not on the Fut t.

e.g.


  Fut Fut t ~~> \(x: t) -> x + 1      -- doesn't typecheck, but we want this
  Fut Fut t ~~> \(x: Fut t) -> 1 + get x -- typechecks but it's not that flexible for the Par t types


## Join

if there are many small tasks with dependencies between them,
you would like to not spawn a new computation for every one of them, and run them with the
sequence combinator. There needs to be futher discussion about detecting (at runtime?)
when to run some tasks in sequence and when to run them in parallel.


