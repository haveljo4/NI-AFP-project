module Frontend.CommandLine.MainWindow where
import qualified Backend.Downloader as Downloader
import qualified Backend.TWICDownloadManager as TWICDownloadManager
import qualified Backend.ChessComDownloadManager as ChessComDownloadManager
-- TODO implemented command line if desired
main :: IO ()
--main = do Downloader.download "https://theweekinchess.com/zips/twic1485g.zip" ""
main = do TWICDownloadManager.downloadAndGroup 920 930 ".\\SomeOutputFolder" logger
--main = do ChessComDownloadManager.downloadAndGroup 2023 1 2023 5 "" ".\\SomeOutputFolder"

logger:: String -> String -> IO ()
logger a b = do
  putStr (a <> b)