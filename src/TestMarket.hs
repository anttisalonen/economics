module TestMarket
where

import Text.Printf
import Text.Regex(subRegex, mkRegex)

import qualified Production as P
import qualified Utility as U
import Curve

pf1 = P.CobbDouglas 1 0.25 0.25
pf2 = P.Substitute  1 1

rental = 1000
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
      nq1                = tendLin q1 (tendLog q1 (lookupY supply1 np1) 0.1) 10
      nq2                = tendLin q2 (tendLog q2 (lookupY supply2 np2) 0.1) 10
  in ((np1, np2), (nq1, nq2))

ppTuple :: (PrintfArg a, PrintfArg b) => (a, b) -> String
ppTuple (a, b) = printf "%3.3f %3.3f" a b

ppTuple2 (a, b) = ppTuple a ++ "\t" ++ ppTuple b

ppTuple3 :: (Show a, Show b, Show c, Show d) => ((a, b), (c, d)) -> String
ppTuple3 t = substitute "(\\(|\\))" "" (show t)

ppTuple4 :: (PrintfArg a, PrintfArg b) => (a, b) -> String
ppTuple4 (a, b) = printf "%3.2f,%3.2f" a b

ppTuple5 :: (Show a, Show b, Show c, Show d,
             PrintfArg a,
             PrintfArg b,
             PrintfArg c,
             PrintfArg d) => ((a, b), (c, d)) -> String
ppTuple5 (a, b) = ppTuple4 a ++ "," ++ ppTuple4 b

-- substitute x with y in s
substitute :: String -> String -> String -> String
substitute x y s = subRegex (mkRegex x) s y

showMarket = mapM_ putStrLn . map ppTuple5 . take 30 . drop 2 $ iterate (uncurry run) ((0, 0), (0, 0))

