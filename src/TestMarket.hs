module TestMarket
where

import Test.HUnit

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.BinTree

import qualified Production as P
import qualified Utility as U
import MarketHelpers
import MarketTypes
import UtilityTree
import Market

pfRice = P.Complement 1 0
pfWheat = P.Complement 1 0
pfPig = P.Complement 1 0.4
pfCow = P.Complement 1 0.2
pfSheep = P.Complement 1 0.3
pfPork = P.Complement 1 3.0
pfBeef = P.Complement 1 2.0
pfMutton = P.Complement 1 3.0
pfLeather = P.Complement 1 0.5
pfWool = P.Complement 1 0.5

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
  ,("Labor",    ProductionInfo pfWool    ""      ""        1 0.1)
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

testprices = E.fromSeq [("Wheat", 3.0), ("Rice", 5.0), ("Pork", 10.0), ("Beef", 15.0), ("Mutton", 18.0), ("Wool", 8.0), ("Leather", 10.0),
         ("Pig", 8), ("Cow", 12), ("Sheep", 14)]

prodprices = E.insert "Labor" 30.0 testprices

showMarket = putStrLn . concatMap showEconomy $ runEconomy

showLatestEconomy :: [Economy] -> String
showLatestEconomy = showEconomy . last

runEconomy :: [Economy]
runEconomy = take 30 . drop 2 $ iterate stepEconomy initialEconomy

testUtree = do
  let w = weightAllocation utree
  let b = budgetAllocation 100 utree
  assertBool ("Weight allocation: " ++ show w) (show w == "Node (\"Welfare\",1.0) (Node (\"Food\",0.53125) (Node (\"Vegetables\",0.47058823529411764) (Node (\"Wheat\",0.625) Empty Empty) (Node (\"Rice\",0.375) Empty Empty)) (Node (\"Meat\",0.5294117647058824) (Node (\"Pork\",1.0) Empty Empty) (Node (\"OtherMeat\",0.0) (Node (\"Beef\",1.0) Empty Empty) (Node (\"Mutton\",0.0) Empty Empty)))) (Node (\"Clothing\",0.46875) (Node (\"Wool\",1.0) Empty Empty) (Node (\"Leather\",0.0) Empty Empty))")
  assertBool ("Budget allocation: " ++ show b) (show b == "fromList [(\"Beef\",0.0),(\"Leather\",0.0),(\"Mutton\",0.0),(\"Pork\",28.125),(\"Rice\",9.375),(\"Wheat\",15.625),(\"Wool\",46.875)]")
  assertBool ("Building utility tree: " ++ show utree) (utree == buildUtilityTree "Welfare" utilitymap testprices)

