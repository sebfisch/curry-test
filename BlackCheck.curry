module BlackCheck (

  Arbitrary(..),

  Property, (==>), (~>), (<~), (<~>),

  label, classify, trivial, collect, collectAs,

  blackCheck, quickCheck, smallCheck, 
  depthCheck, iterCheck, rndCheck, diagCheck

  ) where

import Arbitrary

import List
import AllSolutions
import Random

-- We use lazy IO to handle infinite search trees
-- in the Search monad.
import Unsafe ( unsafeInterleaveIO )

newtype Search a = Search { search :: IO (SearchTree a) }

newtype Property = Property { propTests :: Search Test }

data Test = Test { input :: [String], result :: Maybe Bool, info :: [String] }

noTest :: Test
noTest = Test [] Nothing []

class Testable a where
  tests :: a -> Search Test

instance Testable Property where
  tests = propTests

instance Testable Bool where
  tests b = test1 b (\xs -> case xs of  [x]  -> x
                                        _    -> False)

instance (Show a, Arbitrary a, Testable b) => Testable (a -> b) where
  tests p = do  x <- Search (getSearchTree arbitrary)
                fmap (\t -> t {input = show x : input t}) (tests (p x))

-- BlackCheck can also check I/O properties.
instance Testable a => Testable (IO a) where
  tests a = liftIO a >>= tests

(==>) :: Testable a => Bool -> a -> Property
False  ==> _ = Property (return noTest)
True   ==> a = Property (tests a)

(~>), (<~), (<~>) :: Eq a => a -> a -> Property
x ~>   y = y <~ x
x <~   y = Property (test2 isSubsetOf x y)
x <~>  y = Property (test2  (\a b  ->  a `isSubsetOf` b
                                   &&  b `isSubsetOf` a)
                            x y)
label :: Testable a => String -> a -> Property
label s = Property . fmap (\t -> t { info = s : info t}) . tests

classify :: Testable a => Bool -> String -> a -> Property
classify True   name  = label name
classify False  _     = Property . tests

trivial :: Testable a => Bool -> a -> Property
trivial = (`classify`"trivial")

collect :: (Show s, Testable a) => s -> a -> Property
collect = label . show

collectAs :: (Show s, Testable a) => String -> s -> a -> Property
collectAs name = label . ((name++": ")++) . show

type Strategy = Search Test -> [IO [Test]]

check :: Testable a => Strategy -> a -> IO ()
check s a = checkTraversals (s (tests a))

depthCheck :: Testable a => Int -> a -> IO ()
depthCheck = iterCheck 1

smallCheck :: Testable a => Int -> a -> IO ()
smallCheck = flip iterCheck 1

iterCheck :: Testable a => Int -> Int -> a -> IO ()
iterCheck m n = check (iterDepth m n)

iterDepth :: Int -> Int -> Strategy
iterDepth m n a = map  (\k -> searchBetween k (k+n-1) a)
                       (take m [0,n ..])

searchBetween :: Int -> Int -> Search a -> IO [a]
searchBetween from to = fmap (betweenLevels from to) . search

betweenLevels :: Int -> Int -> SearchTree a -> [a]
betweenLevels from to = go 0
  where  go level Fail =  []
         go level (Or ts)
            |  level >= to  = []
            |  otherwise    = concatMap (go (level+1)) ts
         go level (Val x)
            |  from <= level && level <= to  = [x]
            |  otherwise                     = []

quickCheck :: Testable a => a -> IO ()
quickCheck = rndCheck 100 1 10

rndCheck :: Testable a => Int -> Int -> Int -> a -> IO ()
rndCheck = checkPasses allValuesD

checkPasses  :: Testable a
             => (SearchTree Test -> [Test]) -> Int -> Int -> Int
             -> a -> IO ()
checkPasses traverse m n max p =
    do  gs <- fmap splitGen getStdGen
        check (\a -> [ values g a | g <- take m gs ]) p
  where
    values g a  = fmap (takeTests . traverse . shuffleST g) (search a)
    takeTests   = take n . filter isValid . take max

blackCheck :: Testable a => a -> IO ()
blackCheck = diagCheck 10 10 100

diagCheck :: Testable a => Int -> Int -> Int -> a -> IO ()
diagCheck = checkPasses allValuesDiag

allValuesDiag :: SearchTree a -> [a]
allValuesDiag t = [ x | Val x <- concat (diagonals (levels [t])) ]

levels :: [SearchTree a] -> [[SearchTree a]]
levels ts  | null ts    = []
           | otherwise  = ts : levels [ u | Or us <- ts, u <- us ]

diagonals :: [[a]] -> [[a]]
diagonals []      = []
diagonals (l:ls)  = zipCons l ([] : diagonals ls)

zipCons :: [a] -> [[a]] -> [[a]]
zipCons []      ls      = ls
zipCons (x:xs)  []      = [ [y] | y <- x:xs ]
zipCons (x:xs)  (l:ls)  = (x:l) : zipCons xs ls

isSubsetOf :: Eq a => [a] -> [a] -> Bool
isSubsetOf = flip (all . flip elem)

isValid :: Test -> Bool
isValid = maybe False (const True) . result

test1 :: Eq a => a -> ([a] -> Bool) -> Search Test
test1 x p = do  xs <- getValues x
                return (noTest { result = Just (p (nub xs)) })

test2 :: Eq a => ([a] -> [a] -> Bool) -> a -> a -> Search Test
test2 p x y = do  ys <- getValues y
                  test1 x (`p`nub ys)

getValues :: a -> Search [a]
getValues = liftIO . fmap allValuesB . getSearchTree

liftIO :: IO a -> Search a
liftIO a = Search (a >>= return . Val)

instance Functor Search where
  fmap f = Search . fmap (fmap f) . search

instance Monad Search where
  return   = Search . return . Val
  a >>= f  = Search (search a >>= fmap joinST . sequenceST . fmap (search . f))

joinST :: SearchTree (SearchTree a) -> SearchTree a
joinST Fail     = Fail
joinST (Val x)  = x
joinST (Or ts)  = Or (map joinST ts)

-- here we need lazy IO to be able to handle infinite search trees
sequenceST :: SearchTree (IO a) -> IO (SearchTree a)
sequenceST Fail     = return Fail
sequenceST (Val x)  = fmap Val x
sequenceST (Or ts)  = fmap Or (mapM (unsafeInterleaveIO . sequenceST) ts)

splitGen :: StdGen -> [StdGen]
splitGen g = l : splitGen r
  where (l,r) = split g

shuffle :: StdGen -> [a] -> [a]
shuffle g l = shuffleWithLen (randoms g) (length l) l

shuffleWithLen :: [Int] -> Int -> [a] -> [a]
shuffleWithLen (r:rs) len xs
  | len == 0  = []
  | otherwise = z : shuffleWithLen rs (len-1) (ys++zs)
 where
  (ys,z:zs) = splitAt (abs r `mod` len) xs

shuffleST :: StdGen -> SearchTree a -> SearchTree a
shuffleST _ Fail     = Fail
shuffleST _ (Val x)  = Val x
shuffleST g (Or ts)  = Or (shuffle r (zipWith shuffleST rs ts))
  where r:rs = splitGen g

checkTraversals :: [IO [Test]] -> IO ()
checkTraversals ts = do  putStr "0"
                         go ts (0,[])
  where  go []      (_,is)  = passedOK is
         go (x:xs)  nis     = x >>= checkTests nis >>= maybe done (go xs)

passedOK :: [[String]] -> IO ()
passedOK is = do  putStrLn " tests passed."
                  mapM_ putStrLn (table is)
  where  table  =  map entry
                .  sortBy (flip compare `on` fst)
                .  runLength
                .  sort
                .  filter (not . null)

         entry (n,i) = pad 5 (show n) ++ ' ':concat (intersperse ", " i)

pad :: Int -> String -> String
pad n s = replicate (n-length s) ' ' ++ s

on :: (b -> b -> c) -> (a -> b) -> a -> a -> c
(.*.) `on` f = \x y -> f x .*. f y

runLength :: Eq a => [a] -> [(Int,a)]
runLength = map (length &&& head) . group

(&&&) :: (a -> b) -> (a -> c) -> a -> (b,c)
(f &&& g) x = (f x,g x)

checkTests :: (Int,[[String]]) -> [Test] -> IO (Maybe (Int,[[String]]))
checkTests nis l = go l nis
  where  go []      nis = return (Just nis)
         go (t:ts)  nis = checkTest nis t >>= maybe (return Nothing) (go ts)

checkTest :: (Int,[[String]]) -> Test -> IO (Maybe (Int,[[String]]))
checkTest (n,is) t = do  putStr (replicate (length count) (chr 8))
                         putStr count
                         maybe  (return (Just (n,is)))
                                (notify (info t))
                                (result t)
  where  count           = show (n+1)
         notify i True   = return (Just (n+1,i:is))
         notify i False  = do  putStr (nth ++ " test failed")
                               if  null (input t) then putStrLn "."
                                   else do  putStrLn ", arguments:"
                                            mapM_ putStrLn (input t)
                               return Nothing
         nth  | ((n+1)`mod`100) `elem` [11,12,13] = "th"
              | otherwise = maybe "th" id (lookup ((n+1)`mod`10) 
                                             [(1,"st"),(2,"nd"),(3,"rd")])
