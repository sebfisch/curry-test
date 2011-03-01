module Arbitrary where

class     Arbitrary a     where arbitrary :: a
instance  Arbitrary ()    where arbitrary = ()
instance  Arbitrary Bool  where arbitrary = False ? True

instance Arbitrary Int where
  arbitrary = 0
  arbitrary = nat
  arbitrary = -nat

-- we produce a balanced tree of natural numbers
nat = 1; nat = 2*nat; nat = 2*nat+1

-- we generate upper- and lower-case characters
-- as well as spaces and newlines
instance Arbitrary Char where
  arbitrary = oneOf (['A'..'z'] ++ ['0'..'9'] ++ " \t\r\n")

oneOf :: [a] -> a
oneOf (x:xs) = x ? oneOf xs

instance Arbitrary a => Arbitrary [a] where
  arbitrary = [] ? (arbitrary:arbitrary)

instance Arbitrary a => Arbitrary (Maybe a) where
  arbitrary = Nothing ? Just arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = Left arbitrary ? Right arbitrary

instance (Arbitrary a, Arbitrary b) => Arbitrary (a,b) where
  arbitrary = (arbitrary,arbitrary)

instance (Arbitrary a, Arbitrary b, Arbitrary c) => Arbitrary (a,b,c) where
  arbitrary = (arbitrary,arbitrary,arbitrary)

cons1 c = c arbitrary
cons2 c = cons1 (c arbitrary)
cons3 c = cons2 (c arbitrary)
cons4 c = cons3 (c arbitrary)
cons5 c = cons4 (c arbitrary)
cons6 c = cons5 (c arbitrary)
cons7 c = cons6 (c arbitrary)
cons8 c = cons7 (c arbitrary)
cons9 c = cons8 (c arbitrary)
cons10 c = cons9 (c arbitrary)
cons11 c = cons10 (c arbitrary)
cons12 c = cons11 (c arbitrary)
cons13 c = cons12 (c arbitrary)
cons14 c = cons13 (c arbitrary)
cons15 c = cons14 (c arbitrary)
cons16 c = cons15 (c arbitrary)
cons17 c = cons16 (c arbitrary)
cons18 c = cons17 (c arbitrary)
cons19 c = cons18 (c arbitrary)
cons20 c = cons19 (c arbitrary)
