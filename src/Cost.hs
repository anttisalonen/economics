module Cost
where

import Types
import Production
import Curve

type MarginalCostFunction = Curve

type CostFunction = (Price, ProductionFunction)

-- Derive CostFunction on quantity - result is MR.
-- CostFunction is of type f(q) = fc + vc(q)
-- Variable costs must raise for large q.
-- Otherwise an unlimited quantity will be produced 
-- (horizontal supply function).
costs :: ProductionFunction -> Rental -> Wage -> Quantity -> Price
costs prodfunc r w q = 
  let (k, l) = factors prodfunc r w q
  in w * l + r * k

totalCosts :: CostFunction -> Rental -> Wage -> Quantity -> Price
totalCosts (fc, pf) r w q = costs pf r w q + fc

marginalCosts :: CostFunction -> Rental -> Wage -> MarginalCostFunction
marginalCosts (_, pf) = marginalCosts' pf

marginalCosts' :: ProductionFunction -> Rental -> Wage -> MarginalCostFunction
marginalCosts' (CobbDouglas prod alpha beta) r w =
  if alpha + beta == 0.5
    then LinearFunction (cobbDouglasCostDerivedConstant prod alpha beta r w) 0
    else ExponentialFunction (cobbDouglasCostDerivedExponent alpha beta) (cobbDouglasCostDerivedConstant prod alpha beta r w) 0
marginalCosts' (Substitute prod alpha) r w =
  let dp = r / w
  in if dp < alpha
       then LinearFunction ((1 / prod) * r) 0
       else LinearFunction ((1 / prod) * w) 0
marginalCosts' (Complement prod alpha) r w =
  LinearFunction ((alpha / prod) * r + (1 / prod) * w) 0

productionQuantity :: MarginalCostFunction -> Price -> Quantity
productionQuantity = lookupX

productionQuantity' :: ProductionFunction -> Rental -> Wage -> Price -> Quantity
productionQuantity' prodfunc r w p =
  lookupX (marginalCosts' prodfunc r w) p

cost :: CostFunction -> Rental -> Wage -> Quantity -> Price
cost (fc, CobbDouglas productivity alpha beta) r w q = fc + (cobbDouglasCost productivity alpha beta r w q)
cost (fc, Substitute  productivity alpha)      r w q = fc + (substituteCost productivity alpha r w q)
cost (fc, Complement  productivity alpha)      r w q = fc + (complementCost productivity alpha r w q)

