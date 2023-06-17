module Backend.CommonHelper(getTmpFolderName) where
import Data.Time.Format (defaultTimeLocale, formatTime)
import Data.Time.LocalTime (getZonedTime)

getTmpFolderName :: String -> IO String
getTmpFolderName prefix = do
    currentTime <- getZonedTime
    let formattedTime = formatTime defaultTimeLocale "%Y%m%d-%H-%M-%S" currentTime
    return (prefix <> "-" <> formattedTime)
   