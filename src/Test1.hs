module Main
where

import Test.QuickCheck
import Data.Maybe
import Control.Exception (catch)
import Prelude hiding (catch)

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

prop_linenum :: Line -> Bool
prop_linenum l = abs l * signum l == l

main = do
    let runT s a = putStrLn s >> a
    let check s a = putStr (s ++ ": ") >> quickCheck a
    let test s a = putStr (s ++ ": ") >> (a >> putStrLn "OK") `catch` \e -> putStrLn ("Error: " ++ show e)

    runT "Line" $ do
        check "intpoint" prop_intpoint
        check "funcs" prop_funcs
        check "linenum" prop_linenum

    runT "SD" $ do
        check "isSupply" prop_isSupply
        check "isDemand" prop_isDemand
        check "mkLinearFromEP1" prop_mklinearFromEP1
        check "mkLinearFromEP2" prop_mklinearFromEP2
        check "mkLinearFromEP3" prop_mklinearFromEP3
        check "mkLinearFromEP4" prop_mklinearFromEP4
        check "prop_elasticity1" prop_elasticity1
        check "prop_elasticity2" prop_elasticity2
        check "prop_balance" prop_balance
        test  "test24" test24
        test  "test26" test26
