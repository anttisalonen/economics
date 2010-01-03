module FactorMarket
where

import Types
import Production
import Curve

cobbDouglasDerived1 :: Flt 
                    -> Elasticity 
                    -> Elasticity 
                    -> Quantity 
                    -> Quantity 
                    -> Quantity
cobbDouglasDerived1 prod a b k l = prod * l ** b * a * k ** (a - 1)

cobbDouglasDerived2 :: Flt 
                    -> Elasticity 
                    -> Elasticity 
                    -> Quantity 
                    -> Quantity 
                    -> Quantity
cobbDouglasDerived2 prod a b k l = prod * k ** a * b * l ** (b - 1)

substituteDerived1 :: Flt 
                   -> Flt
                   -> Quantity 
                   -> Quantity 
                   -> Quantity
substituteDerived1 prod a _ _ = prod * a

substituteDerived2 :: Flt 
                   -> Flt
                   -> Quantity 
                   -> Quantity 
                   -> Quantity
substituteDerived2 prod a _ _ = prod

complementDerived1 :: Flt 
                   -> Flt
                   -> Quantity 
                   -> Quantity 
                   -> Quantity
complementDerived1 p a k l | (k / a) < l = p / a
                           | otherwise   = 0

complementDerived2 :: Flt 
                   -> Flt
                   -> Quantity 
                   -> Quantity 
                   -> Quantity
complementDerived2 p a k l | (k / a) < l = 0
                           | otherwise   = p

marginalProduct1 :: ProductionFunction -> Labor -> Capital -> Quantity
marginalProduct1 (CobbDouglas prod alpha beta) l k = cobbDouglasDerived1 prod alpha beta k l
marginalProduct1 (Substitute a b) l k = substituteDerived1 a b k l
marginalProduct1 (Complement a b) l k = complementDerived1 a b k l

marginalProduct2 :: ProductionFunction -> Capital -> Labor -> Quantity
marginalProduct2 (CobbDouglas prod alpha beta) k l = cobbDouglasDerived2 prod alpha beta k l
marginalProduct2 (Substitute a b) k l = substituteDerived2 a b k l
marginalProduct2 (Complement a b) k l = complementDerived2 a b k l

-- Marginal revenue product
mrp1 :: ProductionFunction -> Labor -> Capital -> Price
mrp1 p l k = marginalProduct1 p l k * k

mrp2 :: ProductionFunction -> Capital -> Labor -> Price
mrp2 p k l = marginalProduct2 p k l * l

mrps :: ProductionFunction -> Capital -> Labor -> (Price, Price)
mrps p k l = (mrp1 p l k, mrp2 p k l)

marginalRevenue :: ProductionFunction -> Labor -> Capital -> (Curve, Curve)
marginalRevenue (CobbDouglas p a b) l k = 
  (mkCurve $ ExponentialFunction (a - 1) (max epsilon (p * l ** b * a)) 0, mkCurve $ ExponentialFunction (b - 1) (max epsilon (p * k ** a * b)) 0)
marginalRevenue (Substitute p a) l k =
  (mkCurve $ LinearFunction 0 (p * a), mkCurve $ LinearFunction 0 p)
marginalRevenue (Complement p a) l k =
  (mkCurve $ LinearFunction (-epsilon) (p / a),
   mkCurve $ LinearFunction (-epsilon) p)

factorDemand :: Price -> Curve -> Curve
factorDemand p c = [LinearFunction 0 p] * c

