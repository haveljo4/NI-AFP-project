module Backend.ChessComDownloadManager where
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Codec.Archive.Zip
import Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.ByteString as BS
import qualified Backend.Downloader as Downloader 
import qualified Backend.PGNFileConcatenator as PGNFileConcatenator 
import Backend.ChessComJsonHelper as ChessComJsonHelper
import Backend.HTTPHelper as HTTPHelper
import Backend.CommonHelper (performTaskWithTimeout)
import Network.Wreq
import Control.Lens
import Data.Foldable
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import System.FilePath
import Data.List (intercalate, (\\))
import Data.Char (isAscii, isAlphaNum)

downloadAndGroup :: Integer -> Integer -> Integer -> Integer -> String -> FilePath -> (String -> String -> IO ()) -> IO ()
downloadAndGroup yearFrom monthFrom yearTo monthTo userName outputFolder logFunction = do
  let tmpFolderPath = outputFolder </> "chess-tool-chesscom-tmp"
  logFunction ("Creating output directories: " <> outputFolder <> " and " <> tmpFolderPath) "INFO"
  createTmpFolders outputFolder tmpFolderPath
  let url = "https://api.chess.com/pub/player/" ++ userName ++ "/games/archives"
  logFunction ("Sending GET request on url: " <> url <> " to receive available archives") "INFO"
  response <- HTTPHelper.getRequest url
  logFunction ("Getting GET response with code: " ++ show (response ^. responseStatus . statusCode) ) "INFO"
  let lazyResBody = response ^. responseBody
  logFunction "Applying filter " "INFO"
  let filteredArchivesURLsRes = ChessComJsonHelper.filterArchives yearFrom monthFrom yearTo monthTo (unpack lazyResBody)
      outputFileName = "chesscom-" ++ userName ++ "-" ++ show yearFrom ++ "-" ++ show monthFrom ++ "-" ++ show yearTo ++ "-" ++ show monthTo ++ ".pgn"
  processedFilteredArchives filteredArchivesURLsRes tmpFolderPath userName logFunction outputFolder outputFileName
  where
    createTmpFolders outputFolder tmpFolderPath = do
      createDirectoryIfMissing True outputFolder
      createDirectoryIfMissing True tmpFolderPath

processedFilteredArchives :: Either String ([String], [String]) ->  FilePath -> String-> (String -> String -> IO ()) -> FilePath -> FilePath ->  IO ()
processedFilteredArchives (Left err) tmpFolderPath userName logFunction outputFolder outputFileName = logFunction  ("Unable filter archives due to: "++show err) "ERROR"
processedFilteredArchives (Right (validUrls, invalidUrls)) tmpFolderPath userName logFunction outputFolder outputFileName = do
  mapM_ (formatInvalidUrl logFunction ) invalidUrls
  processFilterResult validUrls tmpFolderPath userName logFunction outputFolder outputFileName
  where
    formatInvalidUrl :: (String -> String -> IO ()) -> String -> IO ()
    formatInvalidUrl logFunction invalidUrl =
      logFunction ("Invalid URL: " ++ invalidUrl) "ERROR"

processFilterResult :: [String] -> FilePath -> String ->  (String -> String -> IO ()) -> FilePath -> FilePath ->  IO ()
processFilterResult filteredArchivesURLs tmpFolderPath userName logFunction outputFolder outputFileName  = do
    mapM_ (processUrl tmpFolderPath userName logFunction) filteredArchivesURLs
    logFunction ("Joining pgns into single file: " <> (outputFolder </> outputFileName)) "INFO"
    PGNFileConcatenator.processFolderWithPGNs tmpFolderPath (outputFolder </> outputFileName)
    logFunction ("Joined successfully, file: " <> (outputFolder </> outputFileName)) "INFO"
    logFunction ("Removing tmp folder: " <> tmpFolderPath)  "INFO"
    removeDirectoryRecursive tmpFolderPath
  where
    processUrl :: FilePath -> String -> (String -> String -> IO ()) -> String -> IO ()
    processUrl tmpFolderPath userName logFunction url = do
      let yearMonthRes = ChessComJsonHelper.extractYearAndMonthFromURL url
      processExtractedYearAndMonth tmpFolderPath userName logFunction url yearMonthRes
    processExtractedYearAndMonth :: FilePath -> String -> (String -> String -> IO ()) -> String -> Either String (Integer,Integer) ->  IO ()
    processExtractedYearAndMonth tmpFolderPath userName logFunction url  (Right (year, month))  = do
      let fileName = ((sanitizeUsername userName) ++ "-" ++ show year ++ "-" ++ show month ++ ".pgn")
          filePath = tmpFolderPath </> fileName
      logFunction ("Downloading: " <> fileName) "INFO"
      Downloader.download (url ++ "/pgn") filePath
    processExtractedYearAndMonth tmpFolderPath userName logFunction url  (Left err) = do
      logFunction ("Error when parsing url: " <> url <> " due to: " <> err ) "ERROR"


sanitizeUsername :: String -> String
sanitizeUsername = map replaceInvalidChars
  where
    replaceInvalidChars :: Char -> Char
    replaceInvalidChars c
      | isAscii c && isAlphaNum c = c
      | otherwise = '_'

  
