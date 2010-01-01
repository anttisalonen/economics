module Market
where

import Text.Printf
import Data.List (intercalate)
import Data.Maybe (fromMaybe)

import qualified Data.Edison.Assoc.StandardMap as E
import qualified Data.Edison.Seq.SimpleQueue as S

import Libaddutil.Misc (clamp, swap)

import qualified Production as P
import qualified Utility as U
import Types
import Cost
import Curve
import MarketHelpers
import MarketTypes
import MarketTransaction
import ProductionTree
import UtilityTree

showEconomy :: Economy -> String
showEconomy e = 
  showMarketInfo (marketquantity e, marketprice e)

showMarketInfo :: (MarketQuantityMap, MarketPriceMap) -> String
showMarketInfo (qs, ps) = intercalate "\n" . S.toList . S.rcons "\n" $ E.foldWithKey' go (S.singleton (printf "%-21s %-12s %s" "Name" "Quantity" "Price")) qs
  where go :: ProductName -> Quantity -> S.Seq String -> S.Seq String
        go name q prev = let p = E.lookupWithDefault maxCurveValue name ps
                         in S.rcons (printf "%-20s %9.2f %9.2f" name q p) prev

-- One small step for economy.
stepEconomy :: Economy -> Economy
stepEconomy = (stepPrices . uncurry stepProd . swap . stepDemand . regenerate)

{-
stepEconomy e0 = 
  let e = regenerate e0 
      (e', prod')    = stepDemand e -- demand consumes goods, production retrieves inputs
      e'' = stepProd prod' e'  -- supply sets input prices and supplies products
  in stepPrices e'' -- demand sets prices
-}

-- Generates regenerative resources.
regenerate :: Economy -> Economy
regenerate e = e{marketquantity = deposit (regenerative e) (marketquantity e)}

-- Updates economy by consuming all consumer goods and production factors.
-- Available production factors are returned as well.
stepDemand :: Economy -> (Economy, MarketQuantityMap)
stepDemand e = 
  let (mq, prodinputs) = stepDemand' (utilityinfo e) (productioninfo e) (marketprice e) (marketquantity e) (rootutility e) (budget e)
  in (e{marketquantity = mq}, prodinputs)

-- Removes the production factors from market needed for production.
-- Then removes the consumed goods from market.
-- Returns new market status and the list of production factors retrieved
-- from the market.
stepDemand' :: UtilityMap -> ProductionMap -> MarketPriceMap -> MarketQuantityMap -> ProductName -> Price -> (MarketQuantityMap, MarketQuantityMap)
stepDemand' utilities prods mprices mquantities rootname i = 
  let (mq', prodinputs) = withdraw (requiredInputs prods mprices) mquantities -- deduct inputs
      mq'' = consume (quantityAllocation mprices                    -- consumption
                (budgetAllocation i                                     -- (actually bought quantities ignored)
                   (buildUtilityTree 
                       rootname utilities mprices)))
                mq'
  in (mq'', prodinputs)

-- stepProd produces goods as given by inputs and market prices.
stepProd :: MarketQuantityMap -> Economy -> Economy
stepProd q e = e{marketquantity = stepProd' (productioninfo e) (marketprice e) q (marketquantity e)}

-- Production functions and market prices are used for defining the supply
-- curves. (Market prices are used for finding the production factor prices.)
-- Using the market price for the output and the supply curve, the quantity 
-- of good to produce is determined.
-- The quantity is then produced using the inputs. If not enough inputs are
-- available, a quantity smaller than optimal will be produced.
-- The result is the quantities of all produced goods. The unused inputs
-- are returned to the market.
stepProd' :: ProductionMap -> MarketPriceMap -> MarketQuantityMap -> MarketQuantityMap -> MarketQuantityMap
stepProd' prods prices inputs mquantities = 
  let (rest, mq') = E.foldrWithKey' (produce prods prices supplies) (inputs, mquantities) prods
      supplies = buildProductMap $ concatMap (uncurry (mkSupply prices)) (E.toSeq prods)
  in E.unionWith (+) rest mq'

produce :: ProductionMap 
        -> MarketPriceMap 
        -> MarketSupplyMap 
        -> ProductName 
        -> ProductionInfo 
        -> (MarketQuantityMap, MarketQuantityMap) 
        -> (MarketQuantityMap, MarketQuantityMap)
produce prods prices supplies pname prod (inp, acc) = fromMaybe (inp, acc) $ do
  scurve <- E.lookupM pname supplies
  prodinfo <- E.lookupM pname prods
  let prodfunc = productionfunction prodinfo
  let in1 = input1 prodinfo
  let in2 = input2 prodinfo
  let oprice = E.lookupWithDefault maxCurveValue pname prices
  let ip1 = E.lookupWithDefault maxCurveValue in1 prices
  let ip2 = E.lookupWithDefault maxCurveValue in2 prices
  let avail_iq1 = E.lookupWithDefault 0 in1 inp
  let avail_iq2 = E.lookupWithDefault 0 in2 inp
  let wishq = clamp 0 maxCurveValue (productionQuantity scurve oprice)
  let (req_iq1, req_iq2) = P.factors prodfunc ip1 ip2 wishq
  let (real_iq1, real_iq2) = (min req_iq1 avail_iq1, min req_iq2 avail_iq2)
  let prodq = P.production prodfunc real_iq1 real_iq2
  let acc' = E.insertWith (+) pname prodq acc
  let restinputs = E.adjust (\x -> x - real_iq1) in1 (E.adjust (\x -> x - real_iq2) in2 inp)
  return (restinputs, acc')

stepPrices :: Economy -> Economy
stepPrices e = e{marketprice = stepPrices' (utilityinfo e) (marketquantity e) (productioninfo e) (marketprice e) (budget e)}

stepPrices' :: UtilityMap -> MarketQuantityMap -> ProductionMap -> MarketPriceMap -> Price -> MarketPriceMap
stepPrices' utilities quantities productions prices i =
  E.foldrWithKey' (setPrice demands) prices quantities
   where demands = E.unionWith (+) consdemands inpdemands
         consdemands = buildProductMap $ concatMap (mkDemand prices i) (E.elements utilities)
         inpdemands = productionInputDemands productions quantities prices

setPrice :: MarketDemandMap -> ProductName -> Quantity -> MarketPriceMap -> MarketPriceMap
setPrice demands pname q prcs = fromMaybe prcs $ do
  dcurve <- E.lookupM pname demands
  let p = clamp 0 maxCurveValue (U.demandPrice dcurve q)
  return $ E.insert pname p prcs

