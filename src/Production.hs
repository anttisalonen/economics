module Production
where

cobbDouglasProduction :: (Floating a) => a -- | Total factor productivity
                                      -> a -- | Labor output elasticity
                                      -> a -- | Capital output elasticity
                                      -> a -- | Capital input
                                      -> a -- | Labor input
                                      -> a -- | Total production (Monetary
                                           -- value of all goods produced in
                                           -- one time unit)
cobbDouglasProduction productivity a b k l = productivity * k ** a * l ** b

-- | Total costs of production of /quantity/ units of output for given 
-- production factor prices and output elasticities.
cobbDouglasCost :: (Floating a) => a -- | Output elasticity of labor
                                -> a -- | Output elasticity of capital
                                -> a -- | Price of one unit of labor
                                -> a -- | Price of one unit of capital
                                -> a -- | Total factor productivity
                                -> a -- | Quantity to produce
                                -> a -- | Cost for production
cobbDouglasCost a b wage rental productivity quantity = 
  (wage   ** (b / (a + b))) * 
  (rental ** (a / (a + b))) * 
  ((a / b) ** (b    / (a + b)) + 
   (a / b) ** ((-a) / (a + b))) * 
  (quantity / productivity) ** (1 / (a + b))

