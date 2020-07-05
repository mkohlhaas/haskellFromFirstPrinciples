{-# LANGUAGE FlexibleInstances #-}

module Algebraic where
import Data.Int

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show
-- FarmerType is a Sum
data FarmerType = DairyFarmer | WheatFarmer | SoybeanFarmer deriving Show
-- Farmer is a plain ole product of
-- Name, Acres, and FarmerType
data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec = FarmerRec { name :: Name , acres :: Acres , farmerType :: FarmerType } deriving Show

-- isDairyFarmerRec :: FarmerRec -> Bool
-- isDairyFarmerRec farmer =
--   case farmerType farmer of
--     DairyFarmer -> True
--     _ -> False

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec (FarmerRec _ _ DairyFarmer) = True
isDairyFarmerRec  _ = False

11.14 Function type is exponential

