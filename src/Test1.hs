module Main
where

import Test.QuickCheck
import Data.Maybe

import Math
import Line
import SD

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

main = do
    let runT s a = do print s; a

    runT "Line" $ do
        quickCheck prop_intpoint
        quickCheck prop_funcs

    runT "SD" $ do
        quickCheck prop_isSupply
        quickCheck prop_isDemand
        quickCheck prop_mklinearFromEP1
        quickCheck prop_mklinearFromEP2
        quickCheck prop_mklinearFromEP3
        quickCheck prop_mklinearFromEP4
        quickCheck prop_elasticity1
        quickCheck prop_elasticity2

