module Answer (

  Answer, answer, fromAnswer,

  true, false, neg, (/\), (\/),

  and, or, all, any, elem, allDifferent

  ) where

import Prelude hiding ( and, or, all, any, elem )

infixr 3 /\
infixr 2 \/

data Answer = Yes | No | Undecided Answer
 
instance Show Answer where
  show a = if fromAnswer a then "true" else "false"

instance Eq Answer where
  a == b = fromAnswer a == fromAnswer b

answer :: Bool -> Answer
answer b = Undecided (if b then true else false)

fromAnswer :: Answer -> Bool
fromAnswer Yes            = True
fromAnswer No             = False
fromAnswer (Undecided a)  = fromAnswer a

true, false :: Answer
true = Yes; false = No

neg :: Answer -> Answer
neg a = Undecided (negation a)
  where  negation Yes            = No
         negation No             = Yes
         negation (Undecided a)  = neg a

(/\), (\/) :: Answer -> Answer -> Answer

a /\ b = Undecided $  case (a,b) of
                        (Yes          ,_            ) -> b
                        (No           ,_            ) -> No
                        (_            ,Yes          ) -> a
                        (_            ,No           ) -> No
                        (Undecided x  ,Undecided y  ) -> x /\ y

a \/ b = Undecided $  case (a,b) of
                        (Yes          ,_            ) -> Yes
                        (No           ,_            ) -> b
                        (_            ,Yes          ) -> Yes
                        (_            ,No           ) -> a
                        (Undecided x  ,Undecided y  ) -> x \/ y

and, or :: [Answer] -> Answer
and  = foldr (/\) true
or   = foldr (\/) false

all, any :: (a -> Answer) -> [a] -> Answer
all p = and  . map p
any p = or   . map p

elem :: Eq a => a -> [a] -> Answer
elem x = any (answer . (x==))

allDifferent :: Eq a => [a] -> Answer
allDifferent []      = true
allDifferent (x:xs)  = all (answer . (x/=)) xs /\ allDifferent xs
