module TestMarket
where

import Test.HUnit

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.BinTree

import qualified Production as P
import qualified Utility as U
import Cost
import Curve
import MarketHelpers
import MarketTypes
import ProductionTree
import UtilityTree
import Market

pfRice    = P.Complement  1 0
pfWheat   = P.Complement  1 0
pfPig     = P.CobbDouglas 1 0.4  0.1
pfCow     = P.CobbDouglas 1 0.2  0.3
pfSheep   = P.CobbDouglas 1 0.3  0.2
pfPork    = P.CobbDouglas 1 0.25 0.25
pfBeef    = P.CobbDouglas 1 0.25 0.25
pfMutton  = P.CobbDouglas 1 0.25 0.25
pfLeather = P.CobbDouglas 1 0.25 0.25
pfWool    = P.CobbDouglas 1 0.25 0.25
pfLabor   = P.CobbDouglas 1 0.25 0.25

{-
pfRice = P.Complement 1 0
pfWheat = P.Complement 1 0
pfPig = P.Complement 1 0.4
pfCow = P.Complement 1 0.2
pfSheep = P.Complement 1 0.3
pfPork = P.Complement 1 3.0
pfBeef = P.CobbDouglas 1 0.25 0.25
pfMutton = P.Complement 1 3.0
pfLeather = P.Substitute 1 1
pfWool = P.CobbDouglas 1 0.25 0.25
-}

productionmap = E.fromSeq 
 [
   ("Rice",     ProductionInfo pfRice    "" "Labor" 9 0.1)
  ,("Wheat",    ProductionInfo pfWheat   "" "Labor" 9 0.1)
  ,("Pig",      ProductionInfo pfPig     "Labor" "Wheat"   1 0.1)
  ,("Cow",      ProductionInfo pfCow     "Labor" "Wheat"   1 0.1)
  ,("Sheep",    ProductionInfo pfSheep   "Labor" "Wheat"   1 0.1)
  ,("Pork",     ProductionInfo pfPork    "Labor" "Pig"     1 0.1)
  ,("Beef",     ProductionInfo pfBeef    "Labor" "Cow"     1 0.1)
  ,("Mutton",   ProductionInfo pfMutton  "Labor" "Sheep"   1 0.1)
  ,("Leather",  ProductionInfo pfLeather "Labor" "Cow"     1 0.1)
  ,("Wool",     ProductionInfo pfWool    "Labor" "Sheep"   1 0.1)
  ,("Labor",    ProductionInfo pfLabor   ""      ""        1 0.1)
 ]

ufWelfare = U.CobbDouglas 0.5
ufClothing = U.CobbDouglas 0.75
ufFood = U.CobbDouglas 0.25
ufVegetables = U.CobbDouglas 0.5
ufMeat = U.CobbDouglas 0.4
ufOtherMeat = U.CobbDouglas 0.7

utilitymap = E.fromSeq 
 [
   ("Welfare",    UtilityInfo ufWelfare    "Food"       "Clothing")
  ,("Clothing",   UtilityInfo ufClothing   "Wool"       "Leather")
  ,("Food",       UtilityInfo ufFood       "Vegetables" "Meat")
  ,("Vegetables", UtilityInfo ufVegetables "Wheat"      "Rice")
  ,("Meat",       UtilityInfo ufMeat       "Pork"       "OtherMeat")
  ,("OtherMeat",  UtilityInfo ufOtherMeat  "Beef"       "Mutton")
 ]

initialEconomy :: Economy
initialEconomy = mkEconomy 10 productionmap utilitymap "Welfare" (E.fromSeq [("Labor", 100)])

utree = 
  NodeR ("Welfare", ufWelfare)
    (NodeR ("Food", ufFood)
      (NodeR ("Vegetables", ufVegetables)
        (LeafR ("Wheat", 3.0))
        (LeafR ("Rice", 5.0)))
      (NodeR ("Meat", ufMeat)
        (LeafR ("Pork", 10.0))
        (NodeR ("OtherMeat", ufOtherMeat)
          (LeafR ("Beef", 15.0))
          (LeafR ("Mutton", 18.0)))))
    (NodeR ("Clothing", ufClothing)
      (LeafR ("Wool", 8.0))
      (LeafR ("Leather", 10.0)))

testprices = E.fromSeq [("Wheat", 3.0), ("Rice", 5.0), ("Pork", 10.0), ("Beef", 15.0), ("Mutton", 18.0), ("Wool", 8.0), ("Leather", 10.0),
         ("Pig", 8), ("Cow", 12), ("Sheep", 14)]

prodprices = E.insert "Labor" 30.0 testprices

nthEconomy :: Int -> Economy
nthEconomy n = head . drop n $ iterate stepEconomy initialEconomy

showRunningEconomy n m = mapM_ putStrLn . map showEconomyWithData . take m . drop n $ runEconomyData initialEconomy

showLatestEconomy :: [Economy] -> String
showLatestEconomy = showEconomy . last

runEconomyData :: Economy -> [(Economy, MarketQuantityMap, MarketQuantityMap, MarketQuantityMap)]
runEconomyData e = 
  let t@(e', consumed, produced, used) = stepEconomy' e
  in t : runEconomyData e'

runEconomy :: [Economy]
runEconomy = take 30 $ iterate stepEconomy initialEconomy

runEconomy' :: Int -> Int -> [Economy]
runEconomy' n m = take m . drop n $ iterate stepEconomy initialEconomy

testUtree = do
  let w = weightAllocation utree
  let b = budgetAllocation 100 utree
  assertBool ("Weight allocation: " ++ show w) (show w == "Node (\"Welfare\",1.0) (Node (\"Food\",0.53125) (Node (\"Vegetables\",0.47058823529411764) (Node (\"Wheat\",0.625) Empty Empty) (Node (\"Rice\",0.375) Empty Empty)) (Node (\"Meat\",0.5294117647058824) (Node (\"Pork\",1.0) Empty Empty) (Node (\"OtherMeat\",0.0) (Node (\"Beef\",1.0) Empty Empty) (Node (\"Mutton\",0.0) Empty Empty)))) (Node (\"Clothing\",0.46875) (Node (\"Wool\",1.0) Empty Empty) (Node (\"Leather\",0.0) Empty Empty))")
  assertBool ("Budget allocation: " ++ show b) (show b == "fromList [(\"Beef\",0.0),(\"Leather\",0.0),(\"Mutton\",0.0),(\"Pork\",28.125),(\"Rice\",9.375),(\"Wheat\",15.625),(\"Wool\",46.875)]")
  assertBool ("Building utility tree: " ++ show utree) (utree == buildUtilityTree "Welfare" utilitymap testprices)

e = nthEconomy 10
prices = marketprice e
productions = productioninfo e
prods = productions
quantities = marketquantity e
p = E.lookup "Labor" prods
curve = marginalCosts' (productionfunction p) (E.lookup (input1 p) prices) (E.lookup (input2 p) prices)
pf = productionfunction p
ip1 = E.lookup (input1 p) prices
ip2 = E.lookup (input2 p) prices
pq = productionQuantity curve 0
supplies = buildProductMap $ concatMap (uncurry (mkSupply prices)) (E.toSeq prods)
utilities = utilityinfo e
i = budget e
consdemands = buildProductMap $ concatMap (mkDemand prices i) (E.elements utilities)
inpdemands = productionInputDemands productions quantities prices
demands = E.unionWith (+) consdemands inpdemands
s1 = E.lookup "Labor" supplies
d1 = E.lookup "Labor" demands
sup n = E.lookup n supplies
dem n = E.lookup n demands
balanceGood n = balance (E.lookup n supplies) (E.lookup n demands)
prodkeys = E.keys productions :: [String]
