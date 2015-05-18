{-# LANGUAGE GADTs #-}

module Party where

import Primitives

import Control.Applicative
import Control.Monad(ap)

-- PARTY STUFF
type Array a = [a]

data Par a where
  S :: Par a
  V :: a -> Par a
  F :: Fut a -> Par a
  P :: Par a -> Par a -> Par a
  J :: Par (Par a) -> Par a
  FP :: Fut (Par a) -> Par a
  M  :: Int -> Int -> Array a -> Par a
       -- M a b c is an array of data, with array size and amount of array used


-- used to implement Sequence
instance Functor Par where
  fmap f S = S
  fmap f (V e) = V (f e)
  fmap f (F e) = F (e ~~> f)
  fmap f (P e1 e2)  = P (fmap f e1) (fmap f e2)
  fmap f (J e) = J (fmap (fmap f) e)
  fmap f (FP e) = FP (fmap (fmap f) e)
  fmap f (M a b l) = M a b (map f l)

-- not used, but it is simple
instance Monad Par where
  return v = V v
  v >>= f = pjoin $ fmap f v

-- not used, but Haskell needed it
instance Applicative Par where
  pure = return
  (<*>) = ap


-- SEQUENCE
sequence :: Par t -> (t -> t') -> Par t'
sequence = flip fmap

-- JOIN
pjoin :: Par (Par a) -> Par a
pjoin S = S
pjoin (V e) = e
pjoin (F f) = FP f   -- could also be F $ f ~~> pjoin
pjoin (P e1 e2) = p (pjoin e1) (pjoin e2)
  where p S v = v
        p v S = v
        p v w = P v w
pjoin (FP f) = FP $ f ~~> pjoin
pjoin (J l) = pjoin (pjoin l)
-- pjoin (M a b l) = M a b (pjoin l)   TODO: not clear what to do

-- EXTRACT
extract :: Par t -> [t]
extract S = []
extract (V v) = [v]
extract (F f) = [get f]
extract (P e1 e2) = extract e1 ++ extract e2
extract (J l) = concat $ map extract (extract l)
extract (FP f) = extract $ get f
extract (M a b l) = l


-- PRUNE  (could be more polymorphic -- Par t' could be just a)
prune :: (Fut (Maybe t) -> Par t') -> Par t -> Par t'
prune k p = FP $ async (peek p) ~~> k


-- OTHERWISE using Maybe (under the hood)
otherwise :: Par t -> Delay (Par t) -> Par t
otherwise e1 e2 = prune k x
   where
      x = e1    --- this is of course meaningless in Haskell, don't want to duplicate e1
      g Nothing = e2 ()
      g (Just _) = x
      k f = FP $ f ~~> g

-- simpler version of PRUNE using Maybe  (could be more polymorphic, see PRUNE)
select :: (Maybe t -> Par t') -> Par t -> Par t'
select k p = FP $ workhorse k p


-- peek -- notice type has changed. Getting rid of Fut requires a get
peek :: Par t -> Fut (Maybe t)
peek = workhorse id


-- the work horse (polymorphic)
workhorse :: (Maybe t -> a) -> Par t -> Fut a
workhorse k p = process k $ sift p
        where
            process :: (Maybe t -> a) -> Either t [Par t] -> Fut a
            process k (Left v)   = fut $ k $ Just v
            process k (Right []) = fut $ k Nothing
            process k (Right l)  = (buildFancyTask $ map w l) ~~> k
                where
                  w (F f)  = f ~~> Just
                  w (FP f) = fjoin (f ~~> workhorse id)

            sift :: Par t -> Either t [Par t]
            sift S         = Right []
            sift (V v)     = Left v
            sift (P e1 e2) = sift e1 +++ sift e2
            sift (J j)     = sift $ pjoin j
            sift (M _ _ []) = Right []
            sift (M _ _ (a:_)) = Left a
            sift x         = Right [x]   -- cases F and FP

            (+++) :: Either a [b] -> Either a [b] -> Either a [b]
            (Left a) +++ _ = Left a
            (Right a) +++ (Right b) = Right (a ++ b)


-- FUTURE: it'd be nice to have a version of extract that doesn't block,
-- but return a future and uses the trick inside prune to notify when
-- the value is ready. Type: Par t -> Fut [t] instead



schedulers = 4

-- build Par t type from list of values/ futures sequential
seqToPar :: [t] -> Par t
seqToPar = undefined

-- sequential implementation of seqToPar
-- foldl buildPar S
--   where
--     buildPar p f@(Fut t) = P p (F f)
--     buildPar p v = P p (V v)


splitIntoChunks :: Int -> [t] -> [[t]]
splitIntoChunks chunk_size xs =
  if length xs < chunk_size then [xs]
  else let (ys, zs) = splitAt chunk_size xs in ys:splitIntoChunks chunk_size zs

combinePar :: Fut t -> Fut t -> Par t
combinePar f1 f2 = P (F f1) (F f2)

mapTask :: [t] -> [Fut (Par t)]
mapTask xs = map (async . seqToPar) $ splitIntoChunks schedulers xs

fut2Par :: [Fut (Par t)] -> Par t
fut2Par fs = foldl parFut S fs
  where
    parFut :: Par t -> Fut (Par t) -> Par t
    parFut S f = FP f
    parFut p f = P p (FP f)

each :: [t] -> Par t
each xs = fut2Par $ mapTask xs
