module Backend.TWICDownloadManager (downloadAndGroup) where
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Codec.Archive.Zip
import Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.ByteString as BS
import qualified Backend.Downloader as Downloader
import qualified Backend.PGNFileConcatenator as PGNFileConcatenator
import qualified Data.Text as T


downloadAndGroup :: Integer -> Integer -> FilePath -> (String -> String -> IO ()) -> IO ()
downloadAndGroup iFrom iTo outputFolder logFction = do
    let tmpFolderPath = outputFolder </> "chess-tool-twic-tmp"
    logFction ("Creating output directory: " <> outputFolder) "INFO"
    createDirectoryIfMissing True outputFolder
    logFction ("Creating directory: " <> tmpFolderPath) "INFO"
    createDirectoryIfMissing True tmpFolderPath
--    TODO assert that iFrom < iTo
    forM_ [iFrom..iTo] $ \i -> do
          let fileExtension = ".zip"
              fileName = "twic" ++ show i ++ "g"
              filePath = tmpFolderPath </> (fileName ++ fileExtension)
          logFction ("Downloading: " <> fileName) "INFO"
          Downloader.download ("https://theweekinchess.com/zips/" ++ fileName ++ fileExtension) filePath
          logFction ("Unzipping: " <> fileName) "INFO"
          extractFiles filePath (tmpFolderPath </> fileName ++ ".pgn")
    logFction ("Joining pgns into single file") "INFO"
    PGNFileConcatenator.processFolderWithPGNs  tmpFolderPath (outputFolder </> "twic" ++ show iFrom ++"-" ++ show iTo ++ ".pgn") "pgn"
    logFction ("Removing tmp folder: " <> tmpFolderPath)  "INFO"
    removeDirectoryRecursive tmpFolderPath
    where extractFiles :: FilePath -> FilePath -> IO ()
          extractFiles zipPath outputFilePath  = do
            withArchive zipPath $ do
              names <- entryNames
              forM_ names $ \name -> do
                sourceEntry name (CC.sinkFile outputFilePath)
