module Main
where

import Test.QuickCheck

import QCPolynomial
import ProductionTest
import SupplyTest
import CostTest
import TestMarket
import UtilityTest

main = do
    let runT s a = putStrLn s >> a
    let check s a = putStr (s ++ ": ") >> quickCheck a
    let test s a = putStr (s ++ ": ") >> (a >> putStrLn "OK")

    runT "Polynomial" $ do
        check "num" prop_num

    runT "Production" $ do
        test  "test72" test72
        test  "test73" test73
        test  "test83" test83
        test  "test86" test86

    runT "Supply" $ do
        test "test1" supplytest1

    runT "Cost" $ do
        test "Substitute" testSubstitute
        test "Complement" testComplement

{-
    runT "Test market" $ do
        putStrLn . showLatestEconomy $ runEconomy
        -- test "Utility tree" testUtree
-}

    runT "Utility" $ do
        check "factors" prop_factors1
        test "test4a4" test4a4
