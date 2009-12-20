module Production
where

import Libaddutil.Misc (quadr)

import Types

cobbDouglasProduction :: Flt         -- | Total factor productivity
                      -> Elasticity  -- | Labor output elasticity
                      -> Elasticity  -- | Capital output elasticity
                      -> Quantity    -- | Capital input
                      -> Quantity    -- | Labor input
                      -> Quantity    -- | Quantity of output
cobbDouglasProduction productivity a b k l = productivity * k ** a * l ** b

-- | Total costs of production of /quantity/ units of output for given 
-- production factor prices and output elasticities.
cobbDouglasCost :: Flt        -- | Total factor productivity
                -> Elasticity -- | Output elasticity of labor
                -> Elasticity -- | Output elasticity of capital
                -> Price      -- | Price of one unit of labor
                -> Price      -- | Price of one unit of capital
                -> Quantity   -- | Quantity to produce
                -> Price      -- | Cost for production
cobbDouglasCost productivity a b rental wage quantity = 
  (wage   ** (b / (a + b))) * 
  (rental ** (a / (a + b))) * 
  ((a / b) ** (b    / (a + b)) + 
   (a / b) ** ((-a) / (a + b))) * 
  (quantity / productivity) ** (1 / (a + b))

cobbDouglasCostDerivedConstant :: Flt
                               -> Elasticity
                               -> Elasticity -- | Output elasticity of capital
                               -> Price      -- | Price of one unit of labor
                               -> Price      -- | Price of one unit of capital
                               -> Flt
cobbDouglasCostDerivedConstant productivity a b rental wage =
  (wage   ** (b / (a + b))) * 
    (rental ** (a / (a + b))) * 
    ((a / b) ** (b    / (a + b)) + 
     (a / b) ** ((-a) / (a + b))) * 
    (1 / (a + b)) *
    (1 / (productivity ** (1 / (a + b))))

cobbDouglasCostDerived :: Flt        -- | Total factor productivity
                       -> Elasticity -- | Output elasticity of labor
                       -> Elasticity -- | Output elasticity of capital
                       -> Price      -- | Price of one unit of labor
                       -> Price      -- | Price of one unit of capital
                       -> Quantity   -- | Quantity to produce
                       -> Price      -- | Cost for production
cobbDouglasCostDerived productivity a b rental wage quantity = 
  (cobbDouglasCostDerivedConstant productivity a b rental wage) *
  quantity ** (cobbDouglasCostDerivedExponent a b)

cobbDouglasCostDerivedExponent a b =
  ((1 / (a + b)) - 1)

cobbDouglasMinimizeCost :: Elasticity
                        -> Elasticity
                        -> Price
                        -> Price
                        -> (Quantity -> Quantity,
                            Quantity -> Quantity)
cobbDouglasMinimizeCost a b rental wage =
  (\capital -> b * rental * capital / a * wage,
   \labor   -> a * wage   * labor   / b * rental)

cobbDouglasMinimizeCost' :: Flt 
                         -> Elasticity
                         -> Elasticity
                         -> Price
                         -> Price
                         -> Quantity
                         -> (Quantity, Quantity)
cobbDouglasMinimizeCost' productivity a b rental wage quantity =
  let k = (((a * wage)   / (b * rental)) ** (b / (a + b))) * ((quantity / productivity) ** (1 / (a + b)))
      l = (((b * rental) / (a * wage))   ** (a / (a + b))) * ((quantity / productivity) ** (1 / (a + b)))
  in (k, l)

-- f :: CobbDouglasProduction -> Price -> Price -> Quantity -> (Quantity, Quantity)

substituteProduction :: Flt         -- | Total factor productivity
                     -> Flt         -- | Marginal rate of technical substitution
                     -> Quantity    -- | Capital input
                     -> Quantity    -- | Labor input
                     -> Quantity    -- | Quantity of output
substituteProduction p a k l = p * (a * k + l)

substituteMinimizeCost' :: Flt
                        -> Flt
                        -> Price
                        -> Price
                        -> Quantity
                        -> (Quantity, Quantity)
substituteMinimizeCost' prod a rental wage q =
  let dp = rental / wage
  in if dp < a
       then (q / prod, 0)
       else (0, q / prod)

substituteCost :: Flt        -- | Total factor productivity
               -> Flt        -- | Marginal rate of technical substitution
               -> Price      -- | Price of one unit of labor
               -> Price      -- | Price of one unit of capital
               -> Quantity   -- | Quantity to produce
               -> Price      -- | Cost for production
substituteCost prod a rental wage q = 
  rental * k + wage * l
   where (k, l) = substituteMinimizeCost' prod a rental wage q

data ProductionFunction = CobbDouglas { tfp   :: Flt
                                      , alpha :: Elasticity
                                      , beta  :: Elasticity
                                      }
                        | Substitute { tfp   :: Flt
                                     , coeff :: Flt
                                     }
    deriving (Eq, Show, Read)

production :: ProductionFunction -> Quantity -> Quantity -> Quantity
production (CobbDouglas productivity alpha beta) = cobbDouglasProduction productivity alpha beta
production (Substitute productivity alpha) = substituteProduction productivity alpha

cost :: CostFunction -> Rental -> Wage -> Quantity -> Price
cost (fc, CobbDouglas productivity alpha beta) r w q = fc + (cobbDouglasCost productivity alpha beta r w q)
cost (fc, Substitute  productivity alpha)      r w q = fc + (substituteCost productivity alpha r w q)

factors :: ProductionFunction -> Rental -> Wage -> Quantity -> (Capital, Labor)
factors (CobbDouglas prod alpha beta) = cobbDouglasMinimizeCost' prod alpha beta
factors (Substitute  prod alpha)      = substituteMinimizeCost' prod alpha

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

data MarginalCostFunction = LinearFunction Flt Flt
                          | QuadraticFunction Flt Flt Flt
                          | ExponentialFunction Flt Flt Flt
    deriving (Eq, Show, Read)

productionQuantity :: MarginalCostFunction -> Price -> Quantity
productionQuantity (LinearFunction a b)      p = (p - b) / a
productionQuantity (QuadraticFunction a b c) p = 
  let val = quadr a b (p - c)
  in if null val 
       then 0 
       else maximum val
productionQuantity (ExponentialFunction a b c) p =
  ((p - c) / b) ** (1 / a)

productionQuantity' :: ProductionFunction -> Rental -> Wage -> Price -> Quantity
productionQuantity' prodfunc r w p =
  productionQuantity (marginalCosts' prodfunc r w) p

