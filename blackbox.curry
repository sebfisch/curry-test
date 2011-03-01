import BlackCheck
import Heap

dropWhileYieldsValidSuffix :: [Int] -> Bool
dropWhileYieldsValidSuffix xs = null suffix || not (p (head suffix))
  where  suffix  = dropWhile p xs
         p x     = x == 0

test1 = blackCheck dropWhileYieldsValidSuffix

dropWhileYieldsValidSuffix2 :: [Int] -> Property
dropWhileYieldsValidSuffix2 xs =
  (length xs >= 2) ==> dropWhileYieldsValidSuffix xs

test2 = blackCheck dropWhileYieldsValidSuffix2

insert :: a -> [a] -> [a]
insert x ys      = x:ys
insert x (y:ys)  = y:insert x ys

permute :: [a] -> [a]
permute []      = []
permute (x:xs)  = insert x (permute xs)

test3 = blackCheck (\x l -> collect (x,l) (insertAsFirstOrLast x l))

insertAsFirstOrLast :: Int -> [Int] -> Property
insertAsFirstOrLast x xs = insert x xs ~> (x:xs ? xs++[x])

permutePreservesLength :: [Int] -> Property
permutePreservesLength xs = length (permute xs) <~ length xs

-- quickCheck blows up the stack ~ every second run
test4 = blackCheck (\l -> collect l (permutePreservesLength l))

newtype ValidHeap a = Valid { validHeap :: Heap a }
  deriving Show

instance (Arbitrary a, Ord a) => Arbitrary (ValidHeap a) where
  arbitrary () = Valid emptyHeap
  arbitrary () = Valid (insertHeap (arbitrary ()) (validHeap (arbitrary ())))

test5 = blackCheck (isValidHeap . validHeap :: ValidHeap Int -> Bool)

newtype CustomHeap = Custom { customHeap :: Heap Int }
  deriving Show

instance Arbitrary CustomHeap where
  arbitrary () = Custom emptyHeap
  arbitrary () = Custom (insertHeap digit (customHeap (arbitrary ())))

digit :: Int
digit = 0; digit = 1; digit = 2; digit = 3; digit = 4;
digit = 5; digit = 6; digit = 7; digit = 8; digit = 9;

test6 = blackCheck (isValidHeap . customHeap)

aHeap = customHeap (arbitrary ())

heapProperty :: Heap Int -> Property
heapProperty h = isValidHeap h ==> True

heapSize :: Heap Int -> Property
heapSize h = collectAs "size" (size h) (heapProperty h)

heapDepth :: Heap Int -> Property
heapDepth h = collectAs "depth" (depth h) (heapProperty h)
