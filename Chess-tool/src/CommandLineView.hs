module CommandLineView where
import Downloader
import TWICDownloadManager
import ChessComDownloadManager
main :: IO ()
--main = do Downloader.download "https://theweekinchess.com/zips/twic1485g.zip" ""
--main = do TWICDownloadManager.downloadAndGroup 920 930 ".\\SomeOutputFolder"
main = do ChessComDownloadManager.downloadAndGroup 2023 1 2023 5 "havlas208" ".\\SomeOutputFolder"
