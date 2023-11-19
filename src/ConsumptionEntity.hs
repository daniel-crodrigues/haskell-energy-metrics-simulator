{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE OverloadedStrings #-}

module ConsumptionEntity
  ( ConsumptionEntry (..),
    getId,
    getDate,
    getTotalConsumption,
    getActiveProduction,
    getSolarProduction,
    getTotalConsumptionMeter,
    getKitchenConsumption,
    getPoolMotorConsumption,
    getPoolLigthConsumption,
    getWaterConsumption,
    getRoomsConsumption,
    ToRecord (..),
    FromNamedRecord (..),
  )
where

import Data.Csv
import Prelude hiding (id)

data ConsumptionEntry = ConsumptionEntry
  { id :: Int,
    date :: String,
    totalConsumption :: Maybe Float,
    activeProduction :: Maybe Float,
    solarProduction :: Maybe Float,
    totalConsumptionMeter :: Maybe Float,
    kitchenConsumption :: Maybe Float,
    poolMotorConsumption :: Maybe Float,
    poolLigthConsumption :: Maybe Float,
    waterConsumption :: Maybe Float,
    roomsConsumption :: Maybe Float
  }
  deriving (Show, Eq)

-- Define how to get a ConsumptionEntry from a record (CSV row)
instance FromNamedRecord ConsumptionEntry where
  parseNamedRecord dataRecord =
    ConsumptionEntry
      <$> dataRecord Data.Csv..: "Id"
      <*> dataRecord Data.Csv..: "Date"
      <*> dataRecord Data.Csv..: "TotalConsumption"
      <*> dataRecord Data.Csv..: "ActiveProduction"
      <*> dataRecord Data.Csv..: "SolarProduction"
      <*> dataRecord Data.Csv..: "TotalConsumptionMeter"
      <*> dataRecord Data.Csv..: "KitchenConsumption"
      <*> dataRecord Data.Csv..: "PoolMotorConsumption"
      <*> dataRecord Data.Csv..: "PoolLigthConsumption"
      <*> dataRecord Data.Csv..: "WaterConsumption"
      <*> dataRecord Data.Csv..: "RoomsConsumption"

instance ToRecord ConsumptionEntry where
  toRecord :: ConsumptionEntry -> Record
  toRecord entry =
    record
      [ toField (getId entry),
        toField (getDate entry),
        toField (getTotalConsumption entry),
        toField (getActiveProduction entry),
        toField (getSolarProduction entry),
        toField (getTotalConsumptionMeter entry),
        toField (getKitchenConsumption entry),
        toField (getPoolMotorConsumption entry),
        toField (getPoolLigthConsumption entry),
        toField (getWaterConsumption entry),
        toField (getRoomsConsumption entry)
      ]

getId :: ConsumptionEntry -> Int
getId = id

getDate :: ConsumptionEntry -> String
getDate = date

gettotalConsumption :: ConsumptionEntry -> Maybe Float
gettotalConsumption = totalConsumption

getTotalConsumption :: ConsumptionEntry -> Maybe Float
getTotalConsumption = totalConsumption

getActiveProduction :: ConsumptionEntry -> Maybe Float
getActiveProduction = activeProduction

getSolarProduction :: ConsumptionEntry -> Maybe Float
getSolarProduction = solarProduction

getTotalConsumptionMeter :: ConsumptionEntry -> Maybe Float
getTotalConsumptionMeter = totalConsumptionMeter

getKitchenConsumption :: ConsumptionEntry -> Maybe Float
getKitchenConsumption = kitchenConsumption

getPoolMotorConsumption :: ConsumptionEntry -> Maybe Float
getPoolMotorConsumption = poolMotorConsumption

getPoolLigthConsumption :: ConsumptionEntry -> Maybe Float
getPoolLigthConsumption = poolLigthConsumption

getWaterConsumption :: ConsumptionEntry -> Maybe Float
getWaterConsumption = waterConsumption

getRoomsConsumption :: ConsumptionEntry -> Maybe Float
getRoomsConsumption = roomsConsumption
