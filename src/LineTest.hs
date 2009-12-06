module LineTest
where

import Test.QuickCheck
import Data.Maybe

import Math
import Line

instance Arbitrary Line where
  arbitrary = do
    a' <- choose (-100, 100)
    b' <- choose (-100, 100)
    return $ Line a' b'

prop_intpoint l1@(Line a' b') l2@(Line c' d') = 
  isJust (intersect l1 l2) ==>
    let (x,y) = fromJust $ intersect l1 l2 
    in tolerance (lineFunc l1 x - y) 0.0000000001 && 
       tolerance (lineFunc l2 x - y) 0.0000000001

prop_funcs :: Line -> Flt -> Bool
prop_funcs l y = 
  let x  = lineFunc l y
      y' = invLineFunc l x
  in diffTolerance y y' 0.000000001

prop_linenum :: Line -> Bool
prop_linenum l = abs l * signum l == l


