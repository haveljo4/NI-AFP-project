module Backend.Downloader (download) where
import Network.Wreq
import Control.Lens
import Data.Foldable
import qualified Data.ByteString.Lazy as B
import Control.Monad.IO.Class (liftIO)
import System.FilePath
import Backend.HTTPHelper as HTTPHelper



download :: String -> FilePath -> IO ()
download url fp  =  do
                   response <- HTTPHelper.getRequest url 
                   -- TODO return warning if couldn't download
                   let lazyResBody =  response ^. responseBody
                   liftIO $ B.writeFile fp lazyResBody

-- TODO implement estimator of size of the downloaded file. 
