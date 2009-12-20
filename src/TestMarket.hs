module TestMarket
where

import Text.Printf

import qualified Production as P
import qualified Utility as U
import Curve

pf1 = P.CobbDouglas 1 0.45 0.45
pf2 = P.Substitute  1 1

rental = 100
wage = 1
i = 100

uf1 = U.CobbDouglas 0.5

tendLin :: (Num a, Ord a) => a -> a -> a -> a
tendLin start target maxchange =
  let diff   = target - start
      change = if diff > 0
                 then min maxchange diff
                 else max (-maxchange) diff
  in start + change

tendLog :: (Num a, Ord a) => a -> a -> a -> a
tendLog start target coeff =
  let diff   = target - start
      change = diff * coeff
  in start + change

run (p1, p2) (q1, q2) = 
  let (demand1, demand2) = U.demandCurve uf1 i p1 p2
      supply1            = P.marginalCosts' pf1 rental wage
      supply2            = P.marginalCosts' pf2 rental wage
      np1                = lookupX demand1 q1
      np2                = lookupX demand2 q2
      nq1                = tendLog q1 (lookupY supply1 np1) 0.08
      nq2                = tendLog q2 (lookupY supply2 np2) 0.08
  in ((np1, np2), (nq1, nq2))

ppTuple :: (PrintfArg a, PrintfArg b) => (a, b) -> String
ppTuple (a, b) = printf "%3.3f %3.3f" a b

ppTuple2 (a, b) = ppTuple a ++ "\t" ++ ppTuple b
