module Downloader where

class DownloaderA a where
  download :: a -> IO ()

-- Example implementation
data MyDownloader = MyDownloader

instance DownloaderA MyDownloader where
  download _ = putStrLn "Downloading file..."
