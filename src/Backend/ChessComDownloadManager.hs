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
import Network.Wreq
import Control.Lens
import Data.Foldable
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import System.FilePath
import Data.List (intercalate)
import Data.Char (isAscii, isAlphaNum)

downloadAndGroup :: Integer -> Integer -> Integer -> Integer -> String -> FilePath -> (String -> String -> IO ()) -> IO ()
downloadAndGroup yearFrom monthFrom yearTo monthTo userName outputFolder logFction = do
  let tmpFolderPath = outputFolder </> "chess-tool-chesscom-tmp"
  logFction ("Creating output directories: " <> outputFolder <> " and " <> tmpFolderPath) "INFO"
  createTmpFolders outputFolder tmpFolderPath
  let url = "https://api.chess.com/pub/player/" ++ userName ++ "/games/archives"
  let timeout = 5000 -- Timeout in milliseconds
  logFction ("Sending GET request on url: " <> url <> " to receive available archives") "INFO"
  response <- HTTPHelper.getRequest url 
  logFction ("Getting GET response with code: " ++ show (response ^. responseStatus . statusCode) ) "INFO"
  let lazyResBody = response ^. responseBody
  let filteredArchivesURLs = ChessComJsonHelper.filterArchives yearFrom monthFrom yearTo monthTo (unpack lazyResBody)
  logFction "Applying filter " "INFO"
  mapM_ (processUrl tmpFolderPath userName logFction) filteredArchivesURLs
  let outputFileName = "chesscom-" ++ userName ++ "-" ++ show yearFrom ++ "-" ++ show monthFrom ++ "-" ++ show yearTo ++ "-" ++ show monthTo ++ ".pgn"
  logFction ("Joining pgns into single file: " <> (outputFolder </> outputFileName)) "INFO"
  PGNFileConcatenator.processFolderWithPGNs tmpFolderPath (outputFolder </> outputFileName) "pgn"
  removeDirectoryRecursive tmpFolderPath
  where
    processUrl tmpFolderPath userName logFction url  = do
      let (year, month) = ChessComJsonHelper.extractYearAndMonthFromURL url
          fileName = ((sanitizeUsername userName )++ "-" ++ show year ++ "-" ++ show month ++ ".pgn")
          filePath = tmpFolderPath </> fileName
      logFction ("Downloading: " <> fileName) "INFO"
      Downloader.download (url ++ "/pgn") filePath

    createTmpFolders outputFolder tmpFolderPath = do
      createDirectoryIfMissing True outputFolder
      createDirectoryIfMissing True tmpFolderPath


sanitizeUsername :: String -> String
sanitizeUsername = map replaceInvalidChars
  where
    replaceInvalidChars :: Char -> Char
    replaceInvalidChars c
      | isAscii c && isAlphaNum c = c
      | otherwise = '_'

  
