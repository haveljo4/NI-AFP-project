{-# LANGUAGE OverloadedStrings #-}
module Frontend.Flkhts.CommonHelper  where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Backend.TWICDownloadManager as TWICDownloadManager
import qualified Backend.ChessComDownloadManager as ChessComDownloadManager
import Text.Read (readMaybe)
import System.FilePath ((</>))
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Data.Time.Clock
import Data.Time.Format
import System.Locale hiding (defaultTimeLocale)
import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception (evaluate)
import Control.Monad
import Data.IORef
import System.Environment (getExecutablePath)
import System.FilePath (takeDirectory)


-- | Perform a tick operation on the text buffer and the text channel.
tick :: Ref TextBuffer -> TChan T.Text -> IO ()
tick b c = do
  -- Try to read a message from the text channel
  mx <- atomically $ tryReadTChan c
  case mx of
    -- If no message is available, return
    Nothing -> return ()
    -- If a message is available, append it to the text buffer
    Just x -> void $ appendToBuffer b x
  -- Repeat the tick operation with a timeout of 0.025 seconds
  _ <- FL.repeatTimeout 0.025 (tick b c)
  return ()

-- | Send a new line character to the text channel.
sendNewLine :: TChan T.Text -> IO ()
sendNewLine chan = do
  let newLine = T.singleton '\n'
  -- Write the new line character to the text channel
  atomically $ writeTChan chan newLine

-- | Log a message to the text channel with the specified level.
logFunction :: TChan T.Text -> String -> String -> IO ()
logFunction channel msg lvl = do
  -- Format the log message with the current time and level
  formattedMsg <- formatMessage msg lvl
  -- Write the formatted log message to the text channel
  atomically $ writeTChan channel (T.pack formattedMsg)
  -- Send a new line character to the text channel
  sendNewLine channel
  where
    -- Format the log message with the current time and level
    formatMessage :: String -> String -> IO String
    formatMessage msg lvl = do
      currentTime <- getCurrentTime
      let formattedTime = formatTime defaultTimeLocale "%H:%M:%S" currentTime
      return $ formattedTime <> " [" <> lvl <> "]: " <> msg

-- | Convert a 'Text' value to an 'Integer' within the 'IO' monad.
ioTextToInt :: T.Text -> IO Integer
ioTextToInt ioText = do
  let text = T.unpack ioText
  return (read text)

-- | Convert a 'Text' value to a 'String' within the 'IO' monad.
ioTextToString :: T.Text -> IO String
ioTextToString ioText = do
  let text = T.unpack ioText
  return text

setIconToWindow :: (Ref Window) ->  IO ()
setIconToWindow window = do
  icon <- pngImageNew  "icon.png"
  case icon of
    (Right image) -> do
      setIcon window (Just image)
    Left _ -> putStrLn "Couldn't read the image"
 
getCurrentFolder :: IO FilePath
getCurrentFolder = do 
    executablePath <- getExecutablePath
    return (takeDirectory executablePath)

