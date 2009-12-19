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

data ProductionFunction = CobbDouglas { tfp   :: Flt
                                      , alpha :: Elasticity
                                      , beta  :: Elasticity
                                      }
    deriving (Eq, Show, Read)

type Wage = Flt
type Rental = Flt
type Capital = Flt
type Labor = Flt

production :: ProductionFunction -> Quantity -> Quantity -> Quantity
production (CobbDouglas productivity alpha beta) = cobbDouglasProduction productivity alpha beta

cost :: CostFunction -> Rental -> Wage -> Quantity -> Price
cost (fc, CobbDouglas productivity alpha beta) r w q = fc + (cobbDouglasCost productivity alpha beta r w q)

factors :: ProductionFunction -> Rental -> Wage -> Quantity -> (Capital, Labor)
factors (CobbDouglas prod alpha beta) = cobbDouglasMinimizeCost' prod alpha beta

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

marginalCosts :: CostFunction -> Rental -> Wage -> MarginalCostFunction
marginalCosts (_, pf) = marginalCosts' pf

marginalCosts' :: ProductionFunction -> Rental -> Wage -> MarginalCostFunction
marginalCosts' (CobbDouglas prod alpha beta) r w =
  if alpha + beta == 0.5
    then LinearFunction (cobbDouglasCostDerivedConstant prod alpha beta r w) 0
    else ExponentialFunction (cobbDouglasCostDerivedExponent alpha beta) (cobbDouglasCostDerivedConstant prod alpha beta r w) 0

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

-- g :: Price -> Labor -> Capital -> ProductionFunction -> CostFunction

-- h :: Rental -> Labor -> ProductionFunction

-- g :: BudgetConstraint -> Isocost -> (Capital, Labor)

-- h :: Wage -> Labor -> Rental -> Capital -> Isocost

-- i :: CostFunction -> Quantity -> BudgetConstraint

-- j :: SupplyFunction -> Price -> Quantity

-- k :: DemandFunction -> Quantity -> Price

-- l :: CostFunction -> Quantity -> Price

-- m :: X -> Labor -> Rental -> Price

-- Given: DemandFunction, Quantity
-- -> Price
-- Needed: SupplyFunction
-- For SupplyFunction: CostFunction
-- For CostFunction: Fix costs, Labor, Capital, Wage, Rental
-- For Labor and Capital: BudgetConstraint, ProductionFunction
-- For BudgetConstraint: Wage, Rental, Income
-- For ProductionFunction: TFP, Output elasticities
-- TFP -> Elasticity -> Elasticity -> Wage -> Rental -> Income -> Fix costs -> SupplyFunction

