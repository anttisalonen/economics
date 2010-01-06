module EqMarket2
where

import qualified Data.Edison.Assoc.StandardMap as E

import qualified Production as P
import qualified Utility as U
import MarketTypes
import MarketHelpers

newProd result prodcoeff (Left (coeff1, coeff2)) needed1 needed2 = (result, ProductionInfo (P.CobbDouglas prodcoeff coeff1 coeff2) needed1 needed2 1 0.1)
newProd result prodcoeff (Right conscoeff)       needed1 needed2 = (result, ProductionInfo (P.Constant    conscoeff)               needed1 needed2 1 0.1)

productionmap techlevel minerals coal oil iron aluminium labor vegetation water precmetals sulphur uranium nitrogen methane = E.fromSeq
 [
    newProd "Robotics"               (techlevel * 1)  (Left (0.25, 0.25)) "Computers"             "Light plastics"
  , newProd "Computers"              (techlevel * 2)  (Left (0.25, 0.25)) "Electronic components" "Industrial parts"
  , newProd "Electronic components"  (techlevel * 2)  (Left (0.25, 0.25)) "Silicon"               "Light alloys"
  , newProd "Light plastics"         (techlevel * 2)  (Left (0.40, 0.10)) "Coal"                  "Petroleum"
  , newProd "Silicon"                (techlevel * 5)  (Left (0.25, 0.25)) "Silica"                "Sulphur"
  , newProd "Silica"                 (techlevel * 8)  (Left (0.25, 0.25)) "Minerals"              "Labor"
  , newProd "Industrial parts"       (techlevel * 8)  (Left (0.25, 0.25)) "Metal alloys"          "Sulphur"
  , newProd "Metal alloys"           (techlevel * 8)  (Left (0.45, 0.05)) "Iron"                  "Coal"
  , newProd "Light alloys"           (techlevel * 8)  (Left (0.25, 0.25)) "Aluminium"             "Silicon"

  , newProd "Textiles"               (techlevel * 10) (Left (0.15, 0.35)) "Livestock"             "Vegetation"
  , newProd "Animal meat"            (techlevel * 12) (Left (0.49, 0.01)) "Livestock"             "Labor"
  , newProd "Livestock"              (techlevel * 12) (Left (0.25, 0.25)) "Grain"                 "Water"
  , newProd "Fruit"                  (techlevel * 5)  (Left (0.35, 0.15)) "Vegetation"            "Water"
  , newProd "Grain"                  (techlevel * 10) (Left (0.49, 0.01)) "Vegetation"            "Labor"

  , newProd "Hand weapons"           (techlevel * 5)  (Left (0.25, 0.25)) "Industrial parts"      "Light alloys"
  , newProd "Battle weapons"         (techlevel * 1)  (Left (0.25, 0.25)) "Robotics"              "Explosives"
  , newProd "Nuclear weapons"        (techlevel * 1)  (Left (0.25, 0.25)) "Uranium"               "Computers"
  , newProd "Chemical weapons"       (techlevel * 1)  (Left (0.25, 0.25)) "Robotics"              "Petroleum"
  , newProd "Explosives"             (techlevel * 5)  (Left (0.45, 0.05)) "Nitrogen"              "Petroleum"
  , newProd "Synthetic narcotics"    (techlevel * 1)  (Left (0.25, 0.25)) "Sulphur"               "Nitrogen" 
  , newProd "Medicines"              (techlevel * 5)  (Left (0.25, 0.25)) "Sulphur"               "Petroleum"
  , newProd "Fertilizer"             (techlevel * 5)  (Left (0.25, 0.25)) "Sulphur"               "Methane"
  , newProd "Synthetic meat"         (techlevel * 1)  (Left (0.15, 0.35)) "Petroleum"             "Methane"
  , newProd "Mineral fuel"           (techlevel * 12) (Left (0.25, 0.25)) "Light alloys"          "Petroleum"
  , newProd "Hydrogen fuel"          (techlevel * 1)  (Left (0.05, 0.45)) "Robotics"              "Methane"
  , newProd "Ammonia"                (techlevel * 5)  (Left (0.25, 0.25)) "Methane"               "Industrial parts"

  , newProd "Liquor"                 (techlevel * 5)  (Left (0.25, 0.25)) "Vegetation"            "Labor"
  , newProd "Jewellery"              (techlevel * 5)  (Left (0.49, 0.01)) "Precious metals"       "Labor"
  , newProd "Minerals"               (techlevel * 1)  (Right minerals)    ""                      ""
  , newProd "Coal"                   (techlevel * 1)  (Right coal)        ""                      ""
  , newProd "Petroleum"              (techlevel * 1)  (Right oil)         ""                      ""
  , newProd "Iron"                   (techlevel * 1)  (Right iron)        ""                      ""
  , newProd "Aluminium"              (techlevel * 1)  (Right aluminium)   ""                      ""
  , newProd "Labor"                  (techlevel * 1)  (Right labor)       ""                      ""
  , newProd "Vegetation"             (techlevel * 1)  (Right vegetation)  ""                      ""
  , newProd "Water"                  (techlevel * 1)  (Right water)       ""                      ""
  , newProd "Precious metals"        (techlevel * 1)  (Right precmetals)  ""                      ""
  , newProd "Sulphur"                (techlevel * 1)  (Right sulphur)     ""                      ""
  , newProd "Uranium"                (techlevel * 1)  (Right uranium)     ""                      ""
  , newProd "Nitrogen"               (techlevel * 1)  (Right nitrogen)    ""                      ""
  , newProd "Methane"                (techlevel * 1)  (Right methane)     ""                      ""
 ]

utilitymap = E.fromSeq
 [
   ("Society",            UtilityInfo (U.CobbDouglas 0.9)  "Welfare"          "Military")
  ,("Welfare",            UtilityInfo (U.CobbDouglas 0.9)  "Survival"         "Luxury")
  ,("Survival",           UtilityInfo (U.CobbDouglas 0.8)  "Health"           "Textiles")
  ,("Health",             UtilityInfo (U.CobbDouglas 0.6)  "Food"             "Medicines")
  ,("Food",               UtilityInfo (U.CobbDouglas 0.25) "Fruit"            "Meat")
  ,("Meat",               UtilityInfo (U.CobbDouglas 0.7)  "Animal meat"      "Synthetic meat")
  ,("Luxury",             UtilityInfo (U.CobbDouglas 0.7)  "Luxury goods"     "Liquor")
  ,("Luxury goods",       UtilityInfo (U.CobbDouglas 0.8)  "Jewellery"        "Other luxuries")
  ,("Other luxuries",     UtilityInfo (U.CobbDouglas 0.4)  "Robotics"         "Mineral fuel")

  ,("Military",           UtilityInfo (U.CobbDouglas 0.3)  "Hydrogen fuel"    "Weaponry")
  ,("Weaponry",           UtilityInfo (U.CobbDouglas 0.5)  "Battle weapons"   "Other weapons")
  ,("Other weapons",      UtilityInfo (U.CobbDouglas 0.7)  "Hand weapons"     "WMD")
  ,("WMD",                UtilityInfo (U.CobbDouglas 0.8)  "Chemical weapons" "Nuclear weapons")
 ]

initialEconomy b tech mineral coal oil iron alum labor veg water precmetals sulphur uranium nitrogen methane = 
  mkEconomy b (productionmap tech mineral coal oil iron alum labor veg water precmetals sulphur uranium nitrogen methane) utilitymap "Welfare" E.empty

stdEconomy1 = initialEconomy 100000 1 1000 100 100 100 100 10000 10000 10000000 1 10000 1 100000 1000
