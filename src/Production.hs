module Production
where

import Types
import Curve

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
      k' = if b * rental == 0 || a + b == 0 || productivity == 0 then maxCurveValue else k
      l' = if a * wage   == 0 || a + b == 0 || productivity == 0 then maxCurveValue else l
  in (k', l')

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
  in if prod == 0 
       then if dp < a
              then (maxCurveValue, 0)
              else (0, maxCurveValue)
       else if dp < a
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

complementProduction :: Flt -> Flt -> Quantity -> Quantity -> Quantity
complementProduction p a k l = p * (min (k / a) l)

complementMinimizeCost' prod a _ _ q =
  (if prod * a == 0 then maxCurveValue else q / (prod * a), if prod == 0 then maxCurveValue else q / prod)

complementCost :: Flt -> Flt -> Price -> Price -> Quantity -> Price
complementCost p a r w q =
  r * k + w * l
   where (k, l) = complementMinimizeCost' p a r w q

data ProductionFunction = CobbDouglas { tfp   :: Flt
                                      , alpha :: Elasticity
                                      , beta  :: Elasticity
                                      }
                        | Substitute { tfp   :: Flt
                                     , coeff :: Flt
                                     }
                        | Complement { tfp   :: Flt
                                     , coeff :: Flt
                                     }
                        | Constant { tfp :: Flt
                                   }
    deriving (Eq, Show, Read)

production :: ProductionFunction -> Quantity -> Quantity -> Quantity
production (CobbDouglas productivity alpha beta) k l = cobbDouglasProduction productivity alpha beta k l
production (Substitute productivity alpha) k l = substituteProduction productivity alpha k l
production (Complement productivity alpha) k l = complementProduction productivity alpha k l
production (Constant productivity) _ _ = productivity

factors :: ProductionFunction -> Rental -> Wage -> Quantity -> (Capital, Labor)
factors (CobbDouglas prod alpha beta) r w q = cobbDouglasMinimizeCost' prod alpha beta r w q
factors (Substitute  prod alpha)      r w q = substituteMinimizeCost' prod alpha r w q
factors (Complement  prod alpha)      r w q = complementMinimizeCost' prod alpha r w q
factors (Constant prod)               _ _ _ = (0, 0)

factorsMRTS :: ProductionFunction -> Flt -> Quantity -> (Capital, Labor)
factorsMRTS p mrts = factors p 1 mrts

factorsL :: ProductionFunction -> [Price] -> Quantity -> [Quantity]
factorsL pf [p1, p2] q =
  let (c, l) = factors pf p1 p2 q
  in [c, l]
factorsL _ _ _ = error "TODO: production factors for multiple inputs not defined"


