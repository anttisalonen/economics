module ProductionTree(requiredInput, requiredInputs,
    productionInputDemands, addDemand)
where

import Data.Maybe

import qualified Data.Edison.Assoc.StandardMap as E

import qualified Production as P
import Cost
import FactorMarket
import Curve
import MarketTypes

-- Given a production info map and the market prices, the demand curves for
-- all inputs are resolved. A demand curve of input x of one market with
-- output z is of form Px=MRPx (marginal revenue product of x), where Px is 
-- the price of input x and MRPx = MPx * Pz. MPx is marginal product of x
-- (partial derivative of the production function w.r.t. input x).
-- Pz is the output good unit price.
-- In a fully competitive market MRPx = Px.
productionInputDemands :: ProductionMap -> MarketQuantityMap -> MarketPriceMap -> MarketDemandMap
productionInputDemands productions quantities prices = E.foldrWithKey' (addDemand quantities prices) E.empty productions

addDemand :: MarketQuantityMap -> MarketPriceMap -> ProductName -> ProductionInfo -> MarketDemandMap -> MarketDemandMap
addDemand quantities prices prodname prodinfo acc = fromMaybe acc $ do
  let in1 = input1 prodinfo
  let in2 = input2 prodinfo
  let iq1 = E.lookupWithDefault maxCurveValue in1 quantities
  let iq2 = E.lookupWithDefault maxCurveValue in2 quantities
  op <- E.lookupM prodname prices
  let prodfunc = productionfunction prodinfo
  let (c1, c2) = marginalRevenue prodfunc iq1 iq2 
  return $ 
    (if null in1 then id else (E.insertWith (+) in1 (factorDemand op c1))) 
   ((if null in2 then id else (E.insertWith (+) in2 (factorDemand op c2))) acc)

requiredInputs :: ProductionMap -> MarketPriceMap -> MarketQuantityMap
requiredInputs productions prices = E.foldrWithKey' go E.empty productions
  where go prodname prodinfo acc = 
          case requiredInput productions prodname prices of
            Nothing -> acc
            Just m  -> E.unionWith (+) m acc

-- Given a product to produce, market prices and a production map, return a 
-- list of quantities of input required for production.
requiredInput :: ProductionMap -> ProductName -> MarketPriceMap -> Maybe MarketQuantityMap
requiredInput = f' E.empty

f' :: MarketQuantityMap -> ProductionMap -> ProductName -> MarketPriceMap -> Maybe MarketQuantityMap
f' acc _ "" _ = Just acc
f' acc productions name prices = do
  p <- E.lookupM name productions
  let getprice x = if null x then 0 else E.lookupWithDefault maxCurveValue x prices
  let price      = getprice name
  let i1       = input1 p
  let i2       = input2 p
  let ip1      = getprice i1
  let ip2      = getprice i2
  let pf       = productionfunction p
  let curve    = marginalCosts' pf ip1 ip2
  let (f1, f2) = P.factors pf ip1 ip2 (productionQuantity curve price)
  t1 <- if null i1 
          then return acc 
          else f' (E.insertWith (+) i1 f1 acc) productions i1 prices
  if null i2 
    then return t1 
    else f' (E.insertWith (+) i2 f2 t1) productions i2 prices

