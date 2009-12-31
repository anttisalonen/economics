module MarketTypes
where

import qualified Data.Edison.Assoc.StandardMap as E

import Libaddutil.BinTree

import qualified Production as P
import qualified Utility as U
import Types

-- General
type ProductName = String

-- Production
data ProductionInfo = ProductionInfo { productionfunction :: P.ProductionFunction,
                                       input1             :: ProductName,
                                       input2             :: ProductName,
                                       maxchange          :: Flt,
                                       changecoeff        :: Flt
                                     }
    deriving (Eq, Show, Read)

-- Map types
type ProductMap a = E.FM ProductName a

type MarketQuantityMap = ProductMap Quantity

type MarketPriceMap = ProductMap Price

type MarketSupplyMap = ProductMap SupplyCurve

type MarketDemandMap = ProductMap DemandCurve

type ProductionMap = ProductMap ProductionInfo

type UtilityMap = ProductMap UtilityInfo

-- Utility
data UtilityInfo = UtilityInfo { utilityfunction :: U.UtilityFunction
                               , output1         :: ProductName
                               , output2         :: ProductName
                               }
    deriving (Eq, Show, Read)

type UtilityTree = BinTreeR (ProductName, U.UtilityFunction) (ProductName, Price)

type MiddleTree = BinTreeR (ProductName, U.UtilityFunction, Price) (ProductName, Price)

type MultiplicatorTree = BinTree (ProductName, Flt)

-- Economy
data Economy = Economy { budget         :: Flt
                       , productioninfo :: ProductionMap
                       , utilityinfo    :: UtilityMap
                       , rootutility    :: ProductName
                       , marketquantity :: MarketQuantityMap
                       , marketprice    :: MarketPriceMap
                       , regenerative   :: MarketQuantityMap
                       }
    deriving (Eq, Show, Read)

