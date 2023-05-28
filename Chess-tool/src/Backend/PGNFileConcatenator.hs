module Backend.PGNFileConcatenator (processFolderWithPGNs) where
  import System.Directory (listDirectory)
  import System.FilePath (takeExtension, (</>))
  import Data.List (isSuffixOf)
  import Conduit
  import qualified Data.ByteString as BS

  concatenateFiles :: [FilePath] -> FilePath -> IO ()
  concatenateFiles inputFiles outputFile = runConduitRes $
    mapM_ sourceFileBS inputFiles .| sinkFileBS outputFile

  filterFilesByExtension :: String -> [FilePath] -> [FilePath]
  filterFilesByExtension extension = filter (isSuffixOf extension)

  processFolderWithPGNs :: FilePath -> FilePath -> String -> IO ()
  processFolderWithPGNs folderPath outputFilePath fileExtension = do
      files <- listDirectory folderPath
      -- Filter files with the extension ".pgn"
      let pgnFiles = filterFilesByExtension fileExtension files
      -- Concatenate the PGN files into "All.pgn"
      concatenateFiles (map (folderPath </>) pgnFiles) outputFilePath
