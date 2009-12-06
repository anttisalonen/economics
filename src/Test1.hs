module Main
where

import Test.QuickCheck
import Control.Exception (catch)
import Prelude hiding (catch)

import LineTest
import SDTest

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
