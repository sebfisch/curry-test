module GlassCheck (

  module Arbitrary, module Answer,

  (==>),

  quickCheck, rndCheck, smallCheck, 
  depthCheck, sparseCheck, discrCheck

  ) where

import Arbitrary
import SearchStrategies
import Answer

import IO ( hSetBuffering, stdout, BufferMode(..) )

(==>) :: Bool -> a -> a
True ==> a = a

type Test = Maybe [String] -- list of arguments that lead to failure

class Testable a where
  test :: a -> Test

instance Testable Bool where
  test True   = Nothing
  test False  = Just []

instance Testable Answer where
  test = test . fromAnswer

instance (Show a, Arbitrary a, Testable b) => Testable (a -> b)
  where  test p = fmap (show x:) (test (p x)) 
           where x = arbitrary

quickCheck :: Testable a => a -> IO ()
quickCheck = rndCheck 100

rndCheck :: Testable a => Int -> a -> IO ()
rndCheck  =  check
          .  randomised (strategy.((take 1 . dfs).).shuffledST)

smallCheck :: Testable a => Int -> a -> IO ()
smallCheck = check . iterative (strategy.(dfs.).depthSlice 5)

depthCheck :: Testable a => Int -> a -> IO ()
depthCheck = check . strategy . (dfs.) . (\d -> depthSlice d d)

sparseCheck :: Testable a => Int -> a -> IO ()
sparseCheck = check .  iterative (\d -> 
                       randomised (\r -> 
                       strategy (dfs . discrSlice 0 d . shuffledST r)) 10)

discrCheck :: Testable a => Int -> a -> IO ()
discrCheck b = check (  randomised (\r -> 
                        strategy (dfs . discrSlice b b . shuffledST r)) 10)

check :: Testable a => Strategy Test -> a -> IO ()
check s a = do  hSetBuffering stdout NoBuffering
                s (getSearchTree (test a)) >>= checkAll 0

checkAll :: Int -> [Test] -> IO ()
checkAll n []      = putStrLn  $   "\nOK, passed "
                               ++  show n ++ " tests."
checkAll n (t:ts)  = maybe ((checkAll $! (n+1)) ts) (printFail n) t

printFail :: Int -> [String] -> IO ()
printFail n args =
  do  putStr ("\nFailure in test " ++ show (n+1))
      if null args  then putStrLn "."
                    else do  putStrLn " for input:"
                             mapM_ putStrLn args
