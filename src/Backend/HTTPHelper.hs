module Backend.HTTPHelper (getRequest) where

import Network.Wreq
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Network.HTTP.Client (ManagerSettings(..), responseTimeoutMicro)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq.Session as Sess


getRequest :: String -> IO (Response B.ByteString)
getRequest = get 
