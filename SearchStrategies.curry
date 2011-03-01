module SearchStrategies (

  module TreeSearch,

  Strategy, strategy, iterative, randomised

  ) where

import Monad
import TreeSearch

import Unsafe ( unsafeInterleaveIO )

type Strategy a = IO (SearchTree a) -> IO [a]

strategy :: (SearchTree a -> [a]) -> Strategy a
strategy = fmap

iterative :: (Int -> Strategy a) -> Int -> Strategy a
-- iterative f n = runAll . zipWith f [0..n] . repeat
iterative f n a = do  putStr "iterations:   "
                      runAll (map pass [0..n])
  where  pass k =
           do  putStr (  replicate (length (show (k-1))) (chr 8)
                         ++ show k)
               f k a

randomised :: (StdGen -> Strategy a) -> Int -> Strategy a
randomised f n a = do  rs <- fmap (take n . splitGen) getStdGen
                       runAll (zipWith f rs (repeat a))

runAll :: [IO [a]] -> IO [a]
runAll = fmap concat . sequenceLazy

sequenceLazy :: [IO a] -> IO [a]
sequenceLazy []      = return []
sequenceLazy (m:ms)  =
  liftM2 (:) m (unsafeInterleaveIO (sequenceLazy ms))
