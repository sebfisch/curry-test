module TreeSearch (

  SearchTree, getSearchTree,

  StdGen, getStdGen, splitGen, shuffled,

  dfs, depthSlice, discrSlice, shuffledST

  ) where

import AllSolutions
import Random

dfs :: SearchTree a -> [a]
dfs = allValuesD

depthSlice :: Int -> Int -> SearchTree a -> SearchTree a
depthSlice _ _ Fail     =  Fail
depthSlice w d (Val x)  |  d <= w     = Val x
                        |  otherwise  = Fail
depthSlice w d (Or ts)
  |  d <= 0     = Fail
  |  otherwise  = Or (map (depthSlice w (d-1)) ts)

discrSlice :: Int -> Int -> SearchTree a -> SearchTree a
discrSlice _ _ Fail     =  Fail
discrSlice w d (Val x)  |  d <= w     = Val x
                        |  otherwise  = Fail
discrSlice w d (Or ts)
  |  d <= 0     = Fail
  |  otherwise  = Or (zipWith (discrSlice w) [d-1,d-2..0] ts)

shuffledST :: StdGen -> SearchTree a -> SearchTree a
shuffledST _ Fail     = Fail
shuffledST _ (Val x)  = Val x
shuffledST g (Or ts)  = Or (shuffled r (zipWith shuffledST rs ts))
  where r:rs = splitGen g

splitGen :: StdGen -> [StdGen]
splitGen g = l : splitGen r
  where (l,r) = split g

shuffled :: StdGen -> [a] -> [a]
shuffled g l = shuffleWithLen (randoms g) (length l) l

shuffleWithLen :: [Int] -> Int -> [a] -> [a]
shuffleWithLen (r:rs) len xs
  | len == 0  = []
  | otherwise = z : shuffleWithLen rs (len-1) (ys++zs)
 where
  (ys,z:zs) = splitAt (abs r `mod` len) xs
