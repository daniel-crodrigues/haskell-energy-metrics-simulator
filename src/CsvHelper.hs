{-# LANGUAGE OverloadedStrings #-}

module CsvHelper
  ( removeHeaders,
    parseCsvFile,
    vectorToList,
  )
where

-- this is from the Cassava library

import ConsumptionEntity
import qualified Data.ByteString.Lazy as BL
import Data.Csv hiding (record)
import qualified Data.Vector as V
import System.Directory (doesFileExist)
import Text.Printf
import Prelude hiding (filter)

type ErrorMsg = String

-- type synonym to handle CSV contents
type CsvData = (Header, V.Vector ConsumptionEntry)

-- Discard headers from CsvData
removeHeaders :: CsvData -> V.Vector ConsumptionEntry
removeHeaders = snd

-- Function to read the CSV
parseCsvFile :: FilePath -> IO (Either ErrorMsg CsvData)
parseCsvFile filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then decodeByName <$> BL.readFile filePath
    else return . Left $ printf "The file %s does not exist"

vectorToList :: V.Vector ConsumptionEntry -> [ConsumptionEntry]
vectorToList = V.toList
