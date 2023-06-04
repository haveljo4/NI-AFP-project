module Backend.HTTPHelper (getRequest) where

import Network.Wreq
import qualified Data.ByteString.Lazy as B
import Control.Lens
import Network.HTTP.Client (ManagerSettings(..), responseTimeoutMicro)
import qualified Network.HTTP.Client as HTTP
import qualified Network.Wreq.Session as Sess

oneSecond :: Int
oneSecond = 1000000

getRequest :: String -> IO (Response B.ByteString)
getRequest url = get url 
--getRequest url = do
--    let timeoutMicros = 3 * oneSecond
--        responseTimeout = responseTimeoutMicro timeoutMicros
--        settings = HTTP.defaultManagerSettings { managerResponseTimeout = responseTimeout }
--    Sess.withSessionControl Nothing settings $ \session -> do
--        response <- Sess.getWith defaults session url 
--        return response