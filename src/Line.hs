module Line(Line(..), Point2, intersect, lineFunc, invLineFunc)
where

import Test.QuickCheck

import Math

data Line = Line { a :: Flt
                 , b :: Flt
                 }
  deriving (Read, Show, Eq)

instance Arbitrary Line where
  arbitrary = do
    a' <- choose (-100, 100)
    b' <- choose (-100, 100)
    return $ Line a' b'

type Point2 = (Flt, Flt)

intersect :: Line -> Line -> Maybe Point2
intersect l1 l2 =
  let x = (a' - c') / divis
      y = a' + b' * x
      a' = a l1
      b' = b l1
      c' = a l2
      d' = b l2
      divis = d' - b'
  in if divis == 0 then Nothing else Just (x, y)

lineFunc :: Line -> (Flt -> Flt)
lineFunc (Line a b) = \x -> a + x * b

invLineFunc :: Line -> (Flt -> Flt)
invLineFunc (Line a b) = \y -> (y - a) / b

