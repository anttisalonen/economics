module QCPolynomial
where

import Test.QuickCheck

import Libaddutil.LawProperties (prop_num1)

import Polynomial

instance (Arbitrary a) => Arbitrary (Polynomial a) where
  arbitrary = do
    n <- arbitrary
    return $ Polynomial n

prop_num :: Polynomial Int -> Bool
prop_num = prop_num1

