import ConsumptionEntity (ConsumptionEntry (..), getDate)
import CsvHelper
import Data.ByteString.Lazy.Char8 as B
import Data.Text as T
import Data.Time (UTCTime, addUTCTime, diffUTCTime, getCurrentTime)
import Data.Time.Clock (NominalDiffTime, nominalDiffTimeToSeconds, secondsToNominalDiffTime)
import Network.Connection ()
import Network.MQTT.Client
import Network.MQTT.Topic
import Network.URI (URI, parseURI)
import System.Environment (lookupEnv)
import Utilities (parseDate, toDate, waitUntilTimestamp, readFileIfExists)
import Prelude hiding (filter)
import System.Environment (getArgs)

------------------------------------------------------------------------------------
------------------------------------ SIMULATION ------------------------------------
------------------------------------------------------------------------------------

-- Function to divide NominalDiffTime by a Double factor
divideDiffTime :: NominalDiffTime -> Double -> NominalDiffTime
divideDiffTime diffTime factor =
  secondsToNominalDiffTime (nominalDiffTimeToSeconds diffTime / realToFrac factor)

-- Function to update timestamps based on the time difference
updateTimestamps :: UTCTime -> Double -> [ConsumptionEntry] -> [ConsumptionEntry]
updateTimestamps currentTime scale entries = go currentTime (parseDate $ date $ Prelude.head entries) entries
  where
    go _ _ [] = []
    go base prevTime (entry : rest) =
      let entryDateTime = parseDate (date entry)
          timeDiff = diffUTCTime entryDateTime prevTime
          divTime = divideDiffTime timeDiff scale
          newTime = addUTCTime divTime base
          updatedEntry = entry {date = toDate newTime}
       in updatedEntry : go newTime entryDateTime rest

-- Function used to run the consumption / production simulation
execSimulation :: MQTTClient -> String -> [ConsumptionEntry] -> IO ()
execSimulation mqttClient topic entries = do
  mapM_ (publishOnTime mqttClient topic) entries

-- Function used to publish the simulated consumption / production metrics on time
publishOnTime :: MQTTClient -> String -> ConsumptionEntry -> IO ()
publishOnTime mqttClient topic entry = do
  let timestamp = parseDate $ getDate entry
  Prelude.putStrLn $ getDate entry
  print entry
  waitUntilTimestamp timestamp
  publishToTopic mqttClient topic (show entry)

------------------------------------------------------------------------------------
----------------------------------- MQTT Client ------------------------------------
------------------------------------------------------------------------------------

-- Function used to connec to a specific MQTT brocker
connect :: URI -> IO MQTTClient
connect uri = do
  Prelude.putStrLn $ "Connecting to URI: " ++ show uri
  connectURI mqttConfig uri

-- Function used to pusblish a message into a specific topic
publishToTopic :: MQTTClient -> String -> String -> IO ()
publishToTopic client topic message = do
  case mkTopic $ T.pack topic of
    Just topic -> do
      let payload = B.pack message
      publish client topic payload False
      Prelude.putStrLn "Pulished!"
    Nothing -> Prelude.putStrLn "Error: mkTopic returned Nothing."

-- MQTT filter
handleFilter :: Maybe Filter -> (Filter -> IO ()) -> IO ()
handleFilter (Just filter) action = action filter
handleFilter Nothing _ = Prelude.putStrLn "Error: mkFilter returned Nothing"

-- Function used to subscribe to a specific topic
subscribeToTopic :: URI -> IO ()
subscribeToTopic uri = do
  mc <- connectURI mqttConfig {_msgCB = SimpleCallback msgReceived} uri

  handleFilter (mkFilter $ T.pack "Yhsiahfiajisj/topic1") $ \filter1 -> do
    handleFilter (mkFilter $ T.pack "Yhsiahfiajisj/topic2") $ \filter2 -> do
      print =<< subscribe mc [(filter1, subOptions), (filter2, subOptions)] []
      waitForClient mc
  where
    msgReceived _ t m p = print (t, m, p)

------------------------------------------------------------------------------------
-------------------------------------- Main ----------------------------------------
------------------------------------------------------------------------------------

main :: IO ()
main = do
  Prelude.putStrLn "Start!"
  args <- getArgs
  case args of
    [csvFilePath] -> do
      -- Read Consumption entries from CSV
      result <- parseCsvFile csvFilePath
      case result of
        Left _ -> Prelude.putStrLn "Error: error while reading the csv file."
        Right (_, csvData) -> do      
          -- Retrieve MQTT URI from file
          mqttCredentials <- readFileIfExists ".credentials.txt"
          case mqttCredentials of
            Left _ -> Prelude.putStrLn "Error: error while reading the mqtt credentials file."
            Right mqttUri -> do
              let uri = parseURI mqttUri
              case uri of
                Just parsedUri -> do
                  mqttClient <- connect parsedUri
                  -- Read the csvData with the consumptionEntries
                  let entities = vectorToList csvData
                  -- Create a new event list based on the current timestamp
                  currentTime <- getCurrentTime
                  let events = updateTimestamps currentTime 30.0 entities
                  -- Start the simulation
                  execSimulation mqttClient "Simulation/Consumption" events
                Nothing -> Prelude.putStrLn "Error: Invalid MQTT URI."
