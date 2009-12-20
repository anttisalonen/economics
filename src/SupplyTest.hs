module SupplyTest
where

import Data.Maybe
import Control.Monad (liftM2)

import Test.HUnit

import TestUtils
import Types
import Curve
import Supply

f1 = ExponentialFunction 2 0.5 0
f2 = LinearFunction (-2) 100
f3 = LinearFunction 1 0
-- g1 = curveToPol f1
-- g2 = curveToPol f2
-- g3 = curveToPol f3
-- h1 = derivePolynomial g1
-- h2 = derivePolynomial g2
-- h3 = derivePolynomial g3

checkBalance :: Maybe (Flt, Flt) -> Maybe (Flt, Flt) -> Bool
checkBalance b1 b2 = fromMaybe (b1 == b2) (liftM2 (closeEnough2 0.001) b1 b2)

supplytest1 = do
  let b1 = balance f1 f2
  let b2 = balance f1 f3
  let b3 = balance f2 f3
  let b4 = balance f2 f1
  let b5 = balance f3 f1
  let b6 = balance f3 f2
  assertBool ("Balance 1: " ++ show b1) (checkBalance b1 (Just (12.282,75.434)))
  assertBool ("Balance 2: " ++ show b2) (checkBalance b2 Nothing)
  assertBool ("Balance 3: " ++ show b3) (checkBalance b3 (Just (33.333,33.333)))
  assertBool ("Balance 4: " ++ show b4) (checkBalance b4 (Just (12.282,75.434)))
  assertBool ("Balance 5: " ++ show b5) (checkBalance b5 Nothing)
  assertBool ("Balance 6: " ++ show b6) (checkBalance b6 (Just (33.333,33.333)))

