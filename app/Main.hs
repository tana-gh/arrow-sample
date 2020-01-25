{-# LANGUAGE TupleSections #-}

module Main where

import qualified Control.Arrow    as Arr
import qualified Control.Category as Cat

(><) :: (a -> a') -> (b -> b') -> (a, b) -> (a', b')
(><) f g (a, b) = (f a, g b)

assoc :: ((a, b), c) -> (a, (b, c))
assoc ((a, b), c) = (a, (b, c))

unassoc :: (a, (b, c)) -> ((a, b), c)
unassoc (a, (b, c)) = ((a, b), c)

newtype State s a b = ST ((s, a) -> (s, b))

instance Cat.Category (State s) where
    id = ST id
    ST g . ST f = ST (g . f)

instance Arr.Arrow (State s) where
    arr f = ST (id >< f)
    first (ST f) = ST (assoc . (f >< id) . unassoc)

newtype NonDet a b = ND (a -> [b])

instance Cat.Category NonDet where
    id = ND (: [])
    ND g . ND f = ND (\a -> [c | b <- f a, c <- g b])

instance Arr.Arrow NonDet where
    arr f = ND (\a -> [f a])
    first (ND f) = ND (\(a, b) -> [(a', b) | a' <- f a])

newtype MapTrans s a b = MT ((s -> a) -> (s -> b))

instance Cat.Category (MapTrans s) where
    id = MT id
    MT g . MT f = MT (g. f)

instance Arr.Arrow (MapTrans s) where
    arr f = MT (f .)
    first (MT f) = MT (zipMap . (f >< id) . unzipMap)
        where
        zipMap :: (s -> a, s -> b) -> (s -> (a, b))
        zipMap (f, g) s = (f s, g s)
        unzipMap :: (s -> (a, b)) -> (s -> a, s -> b)
        unzipMap f = (fst . f, snd . f)

newtype Auto a b = A (a -> (b, Auto a b))

instance Cat.Category Auto where
    id = A (, Cat.id)
    A g . A f = A (\a -> let (b, f') = f a; (c, g') = g b in (c, g' Cat.. f'))

instance Arr.Arrow Auto where
    arr f = A (\a -> (f a, Arr.arr f))
    first (A f) = A (\(a, b) -> let (a', f') = f a in ((a', b), Arr.first f'))

add :: Arr.Arrow a => a Int Int -> a Int Int -> a Int Int
add f g = f Arr.&&& g Cat.>>> Arr.arr (uncurry (+))

main :: IO ()
main = print $ let ST f = add Cat.id (Arr.arr (* 2)) in f (0, 5)
