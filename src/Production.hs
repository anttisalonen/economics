module Production
where

import Types

cobbDouglasProduction :: Flt         -- | Total factor productivity
                      -> Elasticity  -- | Labor output elasticity
                      -> Elasticity  -- | Capital output elasticity
                      -> Quantity    -- | Capital input
                      -> Quantity    -- | Labor input
                      -> Quantity    -- | Total production (Monetary
                                     -- value of all goods produced in
                                     -- one time unit)
cobbDouglasProduction productivity a b k l = productivity * k ** a * l ** b

-- | Total costs of production of /quantity/ units of output for given 
-- production factor prices and output elasticities.
cobbDouglasCost :: Elasticity -- | Output elasticity of labor
                -> Elasticity -- | Output elasticity of capital
                -> Price      -- | Price of one unit of labor
                -> Price      -- | Price of one unit of capital
                -> Flt        -- | Total factor productivity
                -> Quantity   -- | Quantity to produce
                -> Price      -- | Cost for production
cobbDouglasCost a b wage rental productivity quantity = 
  (wage   ** (b / (a + b))) * 
  (rental ** (a / (a + b))) * 
  ((a / b) ** (b    / (a + b)) + 
   (a / b) ** ((-a) / (a + b))) * 
  (quantity / productivity) ** (1 / (a + b))

