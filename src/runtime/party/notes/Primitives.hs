
module Primitives
  ( fut
  , (~~>)
  , get
  , async
  , (~~~>)
  , fjoin
  , buildFancyTask
  , Fut
  , Delay
  ) where

import Control.Applicative
import Control.Monad(ap)

-- LIBRARY STUFF, BUILT IN TO ENCORE, ETC

data Fut a = Futt a

instance Functor Fut where
  fmap f (Futt a) = Futt (f a)

(~~>) :: Fut a -> (a -> b) -> Fut b
(~~>) = flip fmap

fut :: a -> Fut a
fut = Futt

get :: Fut a -> a
get (Futt a) = a

async :: a -> Fut a
async = Futt

(~~~>) :: Fut (Fut t) -> (t -> t') -> Fut t'
Futt (Futt v) ~~~> f = Futt (f v)

fjoin :: Fut (Fut t) -> Fut t
fjoin x = x ~~~> id


instance Monad Fut where
  return = fut
  a >>= f = fjoin $ fmap f a

instance Applicative Fut where
  pure = return
  (<*>) = ap

-- A join task takes a collection of futures of type Fut t and a task
-- from type t to type t'. Construction of the task returns a future
-- of type Fut t'
joinTask :: [Fut t] -> (t -> t') -> Fut t'
-- This takes a collection of futures (= dependencies).
-- It creates a task to wait until one of the dependencies is fulfilled, and then runs the function on them.
joinTask = undefined
--- Unrelated to joiner below.


buildFancyTask :: [Fut (Maybe t)] -> Fut (Maybe t)
buildFancyTask = undefined

buildFancyTaskPD :: [Fut t] -> (t -> Bool) -> t -> Fut t
buildFancyTaskPD = undefined


type Delay t = () -> t


-- Functions such as the following will be required to reign in (join)
-- parallel tasks, without doing a get.
joinAndContinue :: Fut a -> Fut b -> (a -> b -> c) -> Fut c
joinAndContinue (Futt a) (Futt b) f = fut $ f a b

joinFut :: Fut a -> Fut b -> Fut (a,b)
joinFut (Futt a) (Futt b) = Futt (a,b)


