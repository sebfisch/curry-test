module Heap (

  Heap, emptyHeap, insertHeap,

  isEmptyHeap, splitMinHeap, isValidHeap,

  size, depth

  ) where

import Arbitrary
import Answer

-- abstract data type for heaps (constructors not exported)
data Heap a = Empty | Fork a [Heap a]
  deriving Show

-- defined here, because it uses |Heap| constructors
instance Arbitrary a => Arbitrary (Heap a) where
  arbitrary = Empty ? Fork arbitrary arbitrary

isEmptyHeap :: Heap a -> Bool
isEmptyHeap Empty       = True
isEmptyHeap (Fork _ _)  = False

-- constructor functions
emptyHeap :: Heap a
emptyHeap = Empty

insertHeap :: Ord a => a -> Heap a -> Heap a
insertHeap x = merge (Fork x [])

-- selector function
splitMinHeap :: Ord a => Heap a -> (a,Heap a)
splitMinHeap (Fork x xs) = (x,mergeAll xs)

-- auxiliary functions
merge :: Ord a => Heap a -> Heap a -> Heap a
merge Empty        h                         = h
merge (Fork x xs)  Empty                     = Fork x xs
merge (Fork x xs)  (Fork y ys)  | x <= y     = Fork x (Fork y ys:xs)
                                | otherwise  = Fork y (Fork x xs:ys)

-- check heap property
isValidHeap :: Ord a => Heap a -> Bool
isValidHeap = fromAnswer . isValid

-- check heap property using fair predicates
isValid :: Ord a => Heap a -> Answer
isValid Empty        =   true
isValid (Fork x hs)  =   Answer.all (answer.(x<=)) [ y | Fork y _ <- hs ]
                     /\  Answer.all isValid hs

-- Boolean version for benchmarks
isValidHeap' :: Ord a => Heap a -> Bool
isValidHeap' Empty        =   True
isValidHeap' (Fork x hs)  =   Prelude.all (x<=) [ y | Fork y _ <- hs ]
                          &&  Prelude.all isValidHeap' hs

mergeAll :: Ord a => [Heap a] -> Heap a
mergeAll []        = emptyHeap
mergeAll [x]       = x
mergeAll (x:y:zs)  = merge (merge x y) (mergeAll zs)

-- compute size and depth of a heap
size, depth :: Heap a -> Int
size   = traverseHeap 0 (succ . foldr (+) 0)
depth  = traverseHeap 0 (succ . foldr max 0)

traverseHeap :: a -> ([a] -> a) -> Heap b -> a
traverseHeap x _ Empty        = x
traverseHeap x f (Fork _ hs)  = f (map (traverseHeap x f) hs)
