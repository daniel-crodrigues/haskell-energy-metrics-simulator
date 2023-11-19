module CsvGenerator
  ( integerToNominalDiffTime,
    randomDate,
    randomComsumptionEntry,
    generateEntries,
    writeConsumptionEntitiesToCsv,
  )
where

import ConsumptionEntity
import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Text as T hiding (split)
import Data.Text.Encoding (encodeUtf8)
import Data.Time
import System.Random
import Prelude hiding (id)

-- Convert Integer to NominalDiffTime
integerToNominalDiffTime :: Integer -> NominalDiffTime
integerToNominalDiffTime x = fromInteger x :: NominalDiffTime

randomDate :: UTCTime -> Integer -> StdGen -> String
randomDate timestampDate maxValue seed = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q%z" (addUTCTime (integerToNominalDiffTime $ fst $ randomR (0, maxValue) seed) timestampDate)

randomComsumptionEntry :: Maybe ConsumptionEntry -> Int -> UTCTime -> StdGen -> ConsumptionEntry
randomComsumptionEntry _ identification timestampDate seed =
  let activeProduction = fst $ randomR (0.0, 2500) seed
      solarProduction = fst $ randomR (0.0, 2500) seed
      kitchenConsumption = fst $ randomR (0.0, 3000) seed
      poolMotorConsumption = fst $ randomR (0.0, 600) seed
      poolLigthConsumption = fst $ randomR (0.0, 300) seed
      waterConsumption = fst $ randomR (0.0, 1400) seed
      roomsConsumption = fst $ randomR (0.0, 1000) seed
      totalConsumption =
        ( kitchenConsumption
            + poolMotorConsumption
            + poolLigthConsumption
            + waterConsumption
            + roomsConsumption) - activeProduction

   in ConsumptionEntry
        identification
        (randomDate timestampDate 999999999 seed)
        (Just totalConsumption)
        (Just activeProduction)
        (Just solarProduction)
        (Just totalConsumption)
        (Just kitchenConsumption)
        (Just poolMotorConsumption)
        (Just poolLigthConsumption)
        (Just waterConsumption)
        (Just roomsConsumption)

generateEntriesRecursive :: Int -> UTCTime -> StdGen -> ConsumptionEntry -> [ConsumptionEntry]
generateEntriesRecursive 0 _ _ _ = []
generateEntriesRecursive counter timestampDate seed prevEntity =
  let newEntity = randomComsumptionEntry (Just prevEntity) counter timestampDate seed
      restEntities = generateEntriesRecursive (counter - 1) timestampDate (snd (split seed)) newEntity
   in newEntity : restEntities

generateEntries :: Int -> IO [ConsumptionEntry]
generateEntries counter = do
  currentTime <- getCurrentTime
  gen <- getStdGen
  let initialEntity = randomComsumptionEntry Nothing 1 currentTime gen
  return $ generateEntriesRecursive counter currentTime (snd (split gen)) initialEntity

writeConsumptionEntitiesToCsv :: [ConsumptionEntry] -> FilePath -> IO ()
writeConsumptionEntitiesToCsv csvData filePath = do
  let csvHeader = "Id,Date,TotalConsumption,ActiveProduction,SolarProduction,TotalConsumptionMeter,KitchenConsumption,PoolMotorConsumption,PoolLigthConsumption,WaterConsumption,RoomsConsumption\n"
  BL.writeFile filePath (BL.fromStrict (encodeUtf8 (pack csvHeader)) <> encode csvData)