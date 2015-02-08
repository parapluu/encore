
-- MODULE PAR

type Par t = [t]

pmap :: (a -> b) -> Par a -> Par b
pmap = map

par :: [t] -> Par t
par = id

papply :: Par (a -> b, a) -> Par b
papply = pmap (\(f,a) -> f a)


broadcast :: a -> Par t -> Par (t,a)
broadcast a l = map (\x -> (x,a)) l 

-- MODULE STREAM 

type Stream t = [t]

andthen = (:)
yield v = v
eos = []

data StreamConsumer state inn res = SC ((state, inn) -> state) (state -> res)

consumer :: StreamConsumer state inn res -> state -> Stream inn -> res
consumer (SC f g) s []     = g s
consumer (SC f g) s (a:as) = consumer (SC f g) (f (s,a)) as


type StreamGenerator state out res = state -> Either (state, out) res
 
generate :: state -> StreamGenerator state out res -> (Stream out, res)
generate s f = generate' [] s f
   where generate' acc s f = cont acc $ f s 
         cont acc (Left (s', o)) = generate' (o : acc) s' f
         cont acc (Right res)    = (reverse acc, res)

-- MODULE ARRAY

type Array t = [t]


---


workers :: Int -> Int -> Int -> StreamConsumer (Array Int) Int [Int] -> Par (Stream Int -> Array Int)
workers number minnum maxnum sc = 
  pmap (consumer sc) $  
  pmap (\(min,max) -> [min,min+2..max]) $ 
  par $ slices number minnum maxnum


-- this part needs to be reimplemented so that it just computes
-- the boundary of each chunk
slices n min max = map (\x -> (head x, last x)) candidates
  where candidates = spread n min max 2

spread n min max by = split n [min,min+by..max]

split n x = chunk (length x `quot` n) x

chunk n [] = []
chunk n xs = as : chunk n bs
  where (as,bs) = splitAt n xs
-- END OF STUFF THAT NEEDS TO BE REDONE


-- master
primegenerator :: Int -> Stream Int
primegenerator last = primegenerator' [3,5..last] 3
  where
    primegenerator' array p = yield p `andthen` cont (remove p array) 
    cont         [] = eos
    cont (p': rest) = primegenerator' rest p'


remove p l = filter (\n -> n `mod` p /= 0) l


-- primes :: Int -> Int -> Int
primes n p = 2 : pg ++ concat (papply $ broadcast pg wo)
   where
     batch = p `div` n   -- calculate more precisely to avoid pathological cases
     pg = primegenerator batch
     wo = workers (n - 1) (batch + 1) p (SC next id)
     next (l, p) = remove p l


{-


;; determines whether prime candidate will appear in given block
;; must be odd and must be within range
(define (valid i first last)
  (and (<= i first) (<= i last) (= (modulo i 2) 1)))

;;; calulates the index of a particular candidate (odd)
(define (index i first last) 
  (quotient (- i first) 2)) 

;; determines size of block
(define (size first last)
  (add1 (- (index last first last)(index first first last) (* (- 1 (modulo first 2)) (- 1 (modulo last 2))))))

;; converts an index into an odd number
(define (deindex i first)
  (let ((offset (+ first (- 1 (modulo first 2)))))
    (+ offset (* 2 i))))


-}