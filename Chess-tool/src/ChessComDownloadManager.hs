module ChessComDownloadManager where
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Codec.Archive.Zip
import Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.ByteString as BS
import Downloader
import PGNFileConcatenator
import Network.Wreq
import Control.Lens
import Data.Foldable
import qualified Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 (pack, unpack)
import Control.Monad.IO.Class (liftIO)
import System.FilePath
import ChessComJsonHelper
import Data.List (intercalate)

downloadAndGroup :: Integer -> Integer -> Integer -> Integer -> String -> FilePath -> IO ()
downloadAndGroup yearFrom monthFrom yearTo monthTo userName outputFolder = do
  let tmpFolderPath = outputFolder </> "chess-tool-chesscom-tmp"
  createTmpFolders outputFolder tmpFolderPath
  let url = "https://api.chess.com/pub/player/" ++ userName ++ "/games/archives"
  response <- get url
  let lazyResBody = response ^. responseBody
  let filteredArchivesURLs = ChessComJsonHelper.filterArchives yearFrom monthFrom yearTo monthTo (unpack lazyResBody)
  mapM_ (processUrl tmpFolderPath userName) filteredArchivesURLs
  let outputFileName = "chesscom-" ++ userName ++ "-" ++ show yearFrom ++ "-" ++ show monthFrom ++ "-" ++ show yearTo ++ "-" ++ show monthTo ++ ".pgn"
  PGNFileConcatenator.processFolderWithPGNs tmpFolderPath (outputFolder </> outputFileName) "pgn"
  removeDirectoryRecursive tmpFolderPath
  where
    processUrl tmpFolderPath userName url = do
      let (year, month) = ChessComJsonHelper.extractYearAndMonthFromURL url
          filePath = tmpFolderPath </> (userName ++ "-" ++ show year ++ "-" ++ show month ++ ".pgn")
      download (url ++ "/pgn") filePath

    createTmpFolders outputFolder tmpFolderPath = do
      createDirectoryIfMissing True outputFolder
      createDirectoryIfMissing True tmpFolderPath


  
