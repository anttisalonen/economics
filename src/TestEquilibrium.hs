module TestEquilibrium
where

import qualified Data.Edison.Assoc.StandardMap as E

import qualified Production as P
import qualified Utility as U
import MarketTypes
import MarketHelpers

pfRice    = P.CobbDouglas 10 0.02 0.48
pfWheat   = P.CobbDouglas 10 0.01 0.49
pfGrain   = P.CobbDouglas 10 0.79 0.01
pfPig     = P.CobbDouglas 10 0.4  0.1
pfCow     = P.CobbDouglas 10 0.2  0.3
pfSheep   = P.CobbDouglas 10 0.3  0.2
pfPork    = P.CobbDouglas 10 0.25 0.25
pfBeef    = P.CobbDouglas 10 0.25 0.25
pfMutton  = P.CobbDouglas 10 0.25 0.25
pfLeather = P.CobbDouglas 10 0.25 0.25
pfWool    = P.CobbDouglas 10 0.25 0.25
pfBread   = P.CobbDouglas 10 0.25 0.25
pfLabor   = P.Constant 100
pfWater   = P.Constant 100000
pfMachine = P.CobbDouglas 10 0.4 0.1
pfIron    = P.CobbDouglas 10 0.4 0.1
pfIronore = P.Constant 1

productionmap = E.fromSeq 
 [
   ("Rice",     ProductionInfo pfRice    "Labor" "Machinery" 9 0.1)
  ,("Wheat",    ProductionInfo pfWheat   "Labor" "Machinery" 9 0.1)
  ,("Grain",    ProductionInfo pfGrain   "Machinery" "Wheat" 1 0.1)
  ,("Pig",      ProductionInfo pfPig     "Water" "Wheat"   1 0.1)
  ,("Cow",      ProductionInfo pfCow     "Water" "Wheat"   1 0.1)
  ,("Sheep",    ProductionInfo pfSheep   "Water" "Wheat"   1 0.1)
  ,("Pork",     ProductionInfo pfPork    "Labor" "Pig"     1 0.1)
  ,("Beef",     ProductionInfo pfBeef    "Labor" "Cow"     1 0.1)
  ,("Mutton",   ProductionInfo pfMutton  "Labor" "Sheep"   1 0.1)
  ,("Leather",  ProductionInfo pfLeather "Labor" "Cow"     1 0.1)
  ,("Wool",     ProductionInfo pfWool    "Labor" "Sheep"   1 0.1)
  ,("Bread",    ProductionInfo pfBread   "Water" "Grain"   1 0.1)
  ,("Labor",    ProductionInfo pfLabor   ""      ""        1 0.1)
  ,("Water",    ProductionInfo pfWater   ""      ""        1 0.1)
  ,("Machinery",ProductionInfo pfMachine "Labor" "Iron"    1 0.1)
  ,("Iron",     ProductionInfo pfIron    "Labor" "Iron ore" 1 0.1)
  ,("Iron ore", ProductionInfo pfIronore ""      ""        1 0.1)
 ]

ufWelfare = U.CobbDouglas 0.5
ufClothing = U.CobbDouglas 0.75
ufFood = U.CobbDouglas 0.25
ufVegetables = U.CobbDouglas 0.5
ufMeat = U.CobbDouglas 0.4
ufOtherMeat = U.CobbDouglas 0.3

utilitymap = E.fromSeq 
 [
   ("Welfare",    UtilityInfo ufWelfare    "Food"       "Clothing")
  ,("Clothing",   UtilityInfo ufClothing   "Wool"       "Leather")
  ,("Food",       UtilityInfo ufFood       "Vegetables" "Meat")
  ,("Vegetables", UtilityInfo ufVegetables "Bread"      "Rice")
  ,("Meat",       UtilityInfo ufMeat       "Pork"       "OtherMeat")
  ,("OtherMeat",  UtilityInfo ufOtherMeat  "Beef"       "Mutton")
 ]

initialEconomy :: Economy
initialEconomy = mkEconomy 1000 productionmap utilitymap "Welfare" E.empty

