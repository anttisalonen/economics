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

nthEconomy :: Int -> Economy -> Economy
nthEconomy n e = nthEconomy' n stepEconomy e

nthEconomy' :: Int -> (Economy -> Economy) -> Economy -> Economy
nthEconomy' n f e = head . drop n $ iterate f e

showRunningEconomy n m e = mapM_ putStrLn . map showEconomyWithData . take m . drop n $ runEconomyData e

showLatestEconomy :: [Economy] -> String
showLatestEconomy = showEconomy . last

runEconomyData :: Economy -> [(Economy, MarketQuantityMap, MarketQuantityMap, MarketQuantityMap)]
runEconomyData e = 
  let t@(e', consumed, produced, used) = stepEconomy' e
  in t : runEconomyData e'

runEconomy :: Economy -> [Economy]
runEconomy e = take 30 $ iterate stepEconomy e

runEconomy' :: Int -> Int -> Economy -> [Economy]
runEconomy' n m e = take m . drop n $ iterate stepEconomy e

showEconomy :: Economy -> String
showEconomy e = 
  showMarketInfo (marketquantity e) (marketprice e)

showEconomyData :: MarketQuantityMap -> MarketQuantityMap -> MarketQuantityMap -> String
showEconomyData consumed produced usedinputs = showData (printf "%-25s %8s %9s  %s" "Name" "Produced" "Consumed" "Used as input") go produced
  where go :: ProductName -> Quantity -> S.Seq String -> S.Seq String
        go name q prev = let p = E.lookupWithDefault 0 name consumed
                             u = E.lookupWithDefault 0 name usedinputs
                         in S.rcons (printf "%-20s %9.2f %9.2f %9.2f" name q p u) prev

showEconomyWithData :: (Economy, MarketQuantityMap, MarketQuantityMap, MarketQuantityMap) -> String
showEconomyWithData (e, c, p, u) = showEconomy e ++ showEconomyData c p u

showData
  :: (Ord k) =>
     String 
  -> (k -> a -> S.Seq String -> S.Seq String)
  -> E.FM k a
  -> String
showData initline go xs = intercalate "\n" . S.toList . S.rcons "\n" $ E.foldWithKey' go (S.singleton (initline)) xs

showMarketInfo :: MarketQuantityMap -> MarketPriceMap -> String
showMarketInfo qs ps = showData (printf "%-21s %-12s %s" "Name" "Quantity" "Price") go qs
  where go :: ProductName -> Quantity -> S.Seq String -> S.Seq String
        go name q prev = let p = E.lookupWithDefault maxCurveValue name ps
                         in S.rcons (printf "%-20s %9.2f %9.2f" name q p) prev

dropThird :: (a, b, c) -> (a, b)
dropThird (a, b, c) = (a, b)

-- One small step for economy.
stepEconomy :: Economy -> Economy
stepEconomy e = let (e', _, _, _) = stepEconomy' e in e'

stepEconomy' :: Economy -> (Economy, MarketQuantityMap, MarketQuantityMap, MarketQuantityMap)
stepEconomy' e = 
  let (e', inputs, consumed) = stepDemand (stepPrices $ regenerate e)
      (e'', produced, usedinputs) = stepProd inputs e'
  in (stepPrices e'', consumed, produced, usedinputs)

-- Generates regenerative resources.
regenerate :: Economy -> Economy
regenerate e = e{marketquantity = E.union (regenerative e) (marketquantity e)}

regProd :: MarketQuantityMap -> ProductionMap
regProd = fmap (\q -> ProductionInfo (P.Constant q) "" "" 0 0)

-- Updates economy by consuming all consumer goods and production factors.
-- Available production factors are returned as well.
stepDemand :: Economy -> (Economy, MarketQuantityMap, MarketQuantityMap)
stepDemand e = 
  let (mq, prodinputs, consumed) = stepDemand' (utilityinfo e) (productioninfo e) (marketprice e) (marketquantity e) (rootutility e) (budget e)
  in (e{marketquantity = mq}, prodinputs, consumed)

-- Removes the production factors from market needed for production.
-- Then removes the consumed goods from market.
-- Returns new market status and the list of production factors retrieved
-- from the market.
stepDemand' :: UtilityMap -> ProductionMap -> MarketPriceMap -> MarketQuantityMap -> ProductName -> Price -> (MarketQuantityMap, MarketQuantityMap, MarketQuantityMap)
stepDemand' utilities prods mprices mquantities rootname i = 
  let (mq', prodinputs) = withdraw (requiredInputs prods mprices) mquantities -- deduct inputs
      (mq'', consumed) = withdraw (quantityAllocation mprices                    -- consumption
                (budgetAllocation i
                   (buildUtilityTree 
                       rootname utilities mprices)))
                mq'
  in (mq'', prodinputs, consumed)

-- stepProd produces goods as given by inputs and market prices.
stepProd :: MarketQuantityMap -> Economy -> (Economy, MarketQuantityMap, MarketQuantityMap)
stepProd q e = let (rest, produced, usedinputs) = stepProd' (productioninfo e) (marketprice e) q
 in (e{marketquantity = E.unionWith (+) rest (E.unionWith (+) produced (marketquantity e))}, produced, usedinputs)

-- Production functions and market prices are used for defining the supply
-- curves. (Market prices are used for finding the production factor prices.)
-- Using the market price for the output and the supply curve, the quantity 
-- of good to produce is determined.
-- The quantity is then produced using the inputs. If not enough inputs are
-- available, a quantity smaller than optimal will be produced.
-- Returns (unused inputs, produced goods).
stepProd' :: ProductionMap -> MarketPriceMap -> MarketQuantityMap -> (MarketQuantityMap, MarketQuantityMap, MarketQuantityMap)
stepProd' prods prices inputs = 
  E.foldrWithKey' (produce prices supplies) (inputs, E.empty, E.empty) prods
      where supplies = buildProductMap $ concatMap (uncurry (mkSupply prices)) (E.toSeq prods)

marketSupplies :: Economy -> MarketSupplyMap
marketSupplies e = buildProductMap $ concatMap (uncurry (mkSupply (marketprice e))) (E.toSeq (productioninfo e))

produce :: MarketPriceMap 
        -> MarketSupplyMap 
        -> ProductName 
        -> ProductionInfo 
        -> (MarketQuantityMap, MarketQuantityMap, MarketQuantityMap) 
        -> (MarketQuantityMap, MarketQuantityMap, MarketQuantityMap)
produce prices supplies pname prodinfo (inp, acc, usedinp) = fromMaybe (inp, acc, usedinp) $ do
  scurve <- E.lookupM pname supplies
  let prodfunc = productionfunction prodinfo
  let in1 = input1 prodinfo
  let in2 = input2 prodinfo
  let oprice = E.lookupWithDefault maxCurveValue pname prices
  let ip1 = if null in1 then 0 else E.lookupWithDefault maxCurveValue in1 prices
  let ip2 = if null in2 then 0 else E.lookupWithDefault maxCurveValue in2 prices
  let avail_iq1 = if null in1 then maxCurveValue else E.lookupWithDefault 0 in1 inp
  let avail_iq2 = if null in2 then maxCurveValue else E.lookupWithDefault 0 in2 inp
  let wishq = clamp 0 maxCurveValue (productionQuantity scurve oprice)
  let (req_iq1, req_iq2) = P.factors prodfunc ip1 ip2 wishq
  let (real_iq1, real_iq2) = (min req_iq1 avail_iq1, min req_iq2 avail_iq2)
  let prodq = P.production prodfunc real_iq1 real_iq2
  if prodq == 0 || null in1 && null in2
    then Nothing
    else do
      let acc' = E.insertWith (+) pname prodq acc
      let restinputs = E.adjust (subtract real_iq1) in1 (E.adjust (subtract real_iq2) in2 inp)
      let usedinputs = E.insertWith (+) in1 real_iq1 (E.insertWith (+) in2 real_iq2 usedinp)
      return (restinputs, acc', usedinputs)

stepPrices :: Economy -> Economy
stepPrices e = e{marketprice = stepPrices' (utilityinfo e) (marketquantity e) (productioninfo e) (marketprice e) (budget e)}

stepPrices' :: UtilityMap -> MarketQuantityMap -> ProductionMap -> MarketPriceMap -> Price -> MarketPriceMap
stepPrices' utilities quantities productions prices i =
  E.foldrWithKey' (setPrice (marketDemands' utilities quantities productions prices i)) prices quantities

setSpecificPrice :: ProductName -> Economy -> Economy
setSpecificPrice pn e = e{marketprice = 
            setPrice (marketDemands e)
                     pn (E.lookupWithDefault 0 pn (marketquantity e)) (marketprice e)}

marketDemands :: Economy -> MarketDemandMap
marketDemands e = marketDemands' (utilityinfo e) (marketquantity e) (productioninfo e) (marketprice e) (budget e)

marketDemands' :: UtilityMap -> MarketQuantityMap -> ProductionMap -> MarketPriceMap -> Price -> MarketDemandMap
marketDemands' utilities quantities productions prices i = E.unionWith (+) consdemands inpdemands
   where consdemands = buildProductMap $ concatMap (mkDemand prices i) (E.elements utilities)
         inpdemands = productionInputDemands productions quantities prices

setPrice :: MarketDemandMap -> ProductName -> Quantity -> MarketPriceMap -> MarketPriceMap
setPrice demands pname q prcs = fromMaybe prcs $ do
  dcurve <- E.lookupM pname demands
  let p = clamp 0 maxCurveValue (U.demandPrice dcurve q)
  return $ E.insert pname p prcs

