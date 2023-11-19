import CsvHelper
import Test.Hspec
import CsvGenerator (generateEntries, writeConsumptionEntitiesToCsv)
import System.Directory (removeFile, doesFileExist)
import Control.Monad

deleteFileIfExists :: String -> IO ()
deleteFileIfExists filePath = do
  fileExists <- doesFileExist filePath
  Control.Monad.when fileExists $ removeFile filePath

csvParser :: IO ()
csvParser = hspec $ do
  describe "CsvHelper" $ do
    context "when generating and writing CSV" $ do
      it "generates the correct number of entries" $ do
        generatedEntities <- generateEntries 1440
        length generatedEntities `shouldBe` 1440

      it "writes to CSV without errors" $ do
        generatedEntities <- generateEntries 1440
        let csvFilePath = "test/generated.csv"
        deleteFileIfExists csvFilePath
        writeConsumptionEntitiesToCsv generatedEntities csvFilePath
        fileExists <- doesFileExist csvFilePath
        fileExists `shouldBe` True

    context "when parsing CSV" $ do
      it "parses a generated CSV file correctly" $ do
        generatedEntities <- generateEntries 1440
        let csvFilePath = "test/generated.csv"
        writeConsumptionEntitiesToCsv generatedEntities csvFilePath
        parsedCsv <- parseCsvFile csvFilePath
        case parsedCsv of
          Left _ -> Prelude.putStrLn "Error: error while reading the csv file."
          Right (_, csvData) -> do
            length csvData `shouldBe` 1440

main :: IO ()
main = do
  csvParser