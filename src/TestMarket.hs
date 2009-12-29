module TestMarket
where

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.BinTree

import qualified Production as P
import qualified Utility as U
import MarketHelpers
import MarketTypes
import Market

productionmap = E.fromSeq 
 [
   ("Rice",     ProductionInfo (P.CobbDouglas 1 0.45 0.05)      "Labor" "Capital" 9 0.1)
  ,("Wheat",    ProductionInfo (P.CobbDouglas 3 0.25 0.25)      "Labor" "Capital" 9 0.1)
  -- ,("Pig",      ProductionInfo (P.Complement  1 0.4)         "Labor" "Wheat"   1 0.1)
  -- ,("Cow",      ProductionInfo (P.Complement  1 0.2)         "Labor" "Wheat"   1 0.1)
  -- ,("Sheep",    ProductionInfo (P.Complement  1 0.3)         "Labor" "Wheat"   1 0.1)
  -- ,("Pork",     ProductionInfo (P.Complement  1 3.0)         "Labor" "Pig"     1 0.1)
  -- ,("Beef",     ProductionInfo (P.Complement  1 2.0)         "Labor" "Cow"     1 0.1)
  -- ,("Mutton",   ProductionInfo (P.Complement  1 3.0)         "Labor" "Sheep"   1 0.1)
  -- ,("Leather",  ProductionInfo (P.Complement  1 0.5)         "Labor" "Cow"     1 0.1)
  -- ,("Wool",     ProductionInfo (P.Complement  1 0.5)         "Labor" "Sheep"   1 0.1)
 ]

ufWelfare = U.CobbDouglas 0.5
ufClothing = U.Substitute 2
ufFood = U.CobbDouglas 0.25
ufVegetables = U.CobbDouglas 0.5
ufMeat = U.Substitute 1.2
ufOtherMeat = U.Substitute 1.2

utilitymap = E.fromSeq 
 [
   ("Welfare",    UtilityInfo ufWelfare    "Food"       "Clothing")
  ,("Clothing",   UtilityInfo ufClothing   "Wool"       "Leather")
  ,("Food",       UtilityInfo ufFood       "Vegetables" "Meat")
  ,("Vegetables", UtilityInfo ufVegetables "Wheat"      "Rice")
  ,("Meat",       UtilityInfo ufMeat       "Pork"       "OtherMeat")
  ,("OtherMeat",  UtilityInfo ufOtherMeat  "Beef"       "Mutton")
 ]

initialEconomy = mkEconomy 100 productionmap utilitymap 

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

showMarket = putStrLn . concatMap showEconomy $ runEconomy

showLatestEconomy :: [Economy] -> String
showLatestEconomy = showEconomy . last

runEconomy :: [Economy]
runEconomy = take 30 . drop 2 $ iterate stepEconomy initialEconomy

