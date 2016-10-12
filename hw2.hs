-- Author: Paul McELroy
-- Date: 10/12/2016
-- A demonstation of a summing a tree named myTree using:
-- summation1 - a function that takes a Tree and uses pure recursion to sum it's leaf nodes.
-- summation2 - a function that takes a Tree and uses the State monad to sum it's leaf nodes.
-- GitHub Repository: https://github.com/Greendogo/EECS_776_HW2

{-# LANGUAGE GADTs, KindSignatures #-}

import Control.Monad (ap)

data Tree :: * where
  Leaf :: Int -> Tree
  Node :: Tree -> Tree -> Tree
  deriving (Show, Eq, Ord)

myTree :: Tree
myTree =
  Node
    (Node
      (Leaf 3)
      (Node
        (Leaf 4)
        (Leaf 1)
      )
    )
    (Node
      (Node
        (Leaf 8)
        (Node
          (Leaf 3)
          (Leaf 5)
        )
      )
      (Leaf 5)
    )

--Part 1
summation1 :: Tree -> Int
summation1 (Leaf a) = a
summation1 (Node l r) = summation1 l + summation1 r



-- so you can make infix operators!
-- (%) :: Int -> Int -> Int
-- a % b = a `mod` b

--also for functions consisting of letters:
--(+3) `map` [3]
--[6]


--Part 2
newtype State s a = State (s -> (a,s))

instance Monad (State s) where
    return a = State $ \s' -> (a,s')
    (>>=) (State passed) bound = State $ \s'' -> (runState $ bound $ fst $ passed s'') $ snd $ passed s''


-- Templates
instance Applicative (State s) where
  pure = return
  (<*>) = ap

instance Functor (State s) where
  fmap f m = pure f <*> m
--
add :: Int -> State Int ()
add a = State $ \x -> ((), a + x)

runState :: State s a -> s -> (a,s)
runState (State s) a = s a

--For this simple exercise I feel like I'm just working
--backwards using :t and holes ('_') to defeat the State monad
--so I can reimplement my first summation function in a slightly
--altered manner. :P
summationM :: Tree -> State Int ()
summationM (Leaf n) = do
  add n
summationM (Node l r) = do
  summationM l
  summationM r

summation2 :: Tree -> Int
summation2 t = snd $ runState (summationM t) 0
