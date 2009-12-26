module Production
where

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

complementProduction :: Flt -> Flt -> Quantity -> Quantity -> Quantity
complementProduction p a k l = p * (min (k / a) l)

complementMinimizeCost' prod a _ _ q =
  (q / prod * a, q / prod)

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
    deriving (Eq, Show, Read)

production :: ProductionFunction -> Quantity -> Quantity -> Quantity
production (CobbDouglas productivity alpha beta) = cobbDouglasProduction productivity alpha beta
production (Substitute productivity alpha) = substituteProduction productivity alpha
production (Complement productivity alpha) = complementProduction productivity alpha

factors :: ProductionFunction -> Rental -> Wage -> Quantity -> (Capital, Labor)
factors (CobbDouglas prod alpha beta) = cobbDouglasMinimizeCost' prod alpha beta
factors (Substitute  prod alpha)      = substituteMinimizeCost' prod alpha
factors (Complement  prod alpha)      = complementMinimizeCost' prod alpha

factorsMRTS :: ProductionFunction -> Flt -> Quantity -> (Capital, Labor)
factorsMRTS p mrts = factors p 1 mrts

