module Utilities
  ( parseDate,
    toDate,
    millisecondsDiff,
    waitUntilTimestamp,
    readFileIfExists
  )
where

import Control.Monad (unless)
import Data.Time
import GHC.Conc.POSIX (threadDelay)
import System.IO
import System.Directory (doesFileExist)
import Text.Printf

-- Function used to convert between String to UTCTime
parseDate :: String -> UTCTime
parseDate s = parseTimeOrError True defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q" s :: UTCTime

-- Function used to convert between UTCTime to String
toDate :: UTCTime -> String
toDate = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S%Q"

-- Function used to calculate the time difference in milliseconds
millisecondsDiff :: UTCTime -> UTCTime -> Integer
millisecondsDiff t t' = round (diffUTCTime t t' * 1000)

-- Function used to wait until a specific UTCTime
waitUntilTimestamp :: UTCTime -> IO ()
waitUntilTimestamp timestamp = do
  currentTime <- getCurrentTime
  let coincides = timestamp < currentTime
  unless coincides $ do
    let waitingTime = 1000 * millisecondsDiff timestamp currentTime
    threadDelay (fromIntegral waitingTime)

-- Function to read the CSV
readFileIfExists :: String -> IO (Either String String)
readFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  if fileExists
    then do
      content <- readFile filePath
      return (Right content)
    else return (Left (printf "The file %s does not exist" filePath))