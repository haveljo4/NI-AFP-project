module TWICDownloadManager (downloadAndGroup) where
import System.Directory (createDirectoryIfMissing, removeDirectoryRecursive)
import System.FilePath ((</>))
import Control.Monad (forM_)
import Codec.Archive.Zip
import Conduit
import qualified Data.Conduit.Combinators as CC
import qualified Data.ByteString as BS
import Downloader
import PGNFileConcatenator

downloadAndGroup :: Integer -> Integer -> FilePath -> IO ()
downloadAndGroup iFrom iTo outputFolder = do
    let tmpFolderPath = outputFolder </> "chess-tool-twic-tmp"
    createDirectoryIfMissing True outputFolder
    createDirectoryIfMissing True tmpFolderPath
--    TODO assert that iFrom < iTo
    forM_ [iFrom..iTo] $ \i -> do
          let fileExtension = ".zip"
              fileName = "twic" ++ show i ++ "g"
              filePath = tmpFolderPath </> (fileName ++ fileExtension)
          Downloader.download ("https://theweekinchess.com/zips/" ++ fileName ++ fileExtension) filePath
          extractFiles filePath (tmpFolderPath </> fileName ++ ".pgn")
    PGNFileConcatenator.processFolderWithPGNs  tmpFolderPath (outputFolder </> "twic" ++ show iFrom ++"-" ++ show iTo ++ ".pgn") "pgn"
    removeDirectoryRecursive tmpFolderPath
    where extractFiles :: FilePath -> FilePath -> IO ()
          extractFiles zipPath outputFilePath  = do
            withArchive zipPath $ do
              names <- entryNames
              forM_ names $ \name -> do
                sourceEntry name (CC.sinkFile outputFilePath)
