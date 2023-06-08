{-# LANGUAGE OverloadedStrings #-}
module Frontend.Flkhts.TWICWindow where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Backend.TWICDownloadManager as TWICDownloadManager
import qualified Backend.ChessComDownloadManager as ChessComDownloadManager
import qualified Frontend.Flkhts.CommonHelper as Helper
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


-- | Perform the TWIC download UI operation.
uiTwicDownload :: IO ()
uiTwicDownload = do
  -- Create the main window
  window <- windowNew
             (Size (Width 600) (Height 400))
             Nothing
             (Just "TWIC Download Window")
  begin window
  Helper.setIconToWindow window

  -- Create the box with name of the window
  imageBox  <- boxNew
                  (toRectangle (10, 10, 600, 50))
                  (Just "TWIC Downloader")
                    
-- Create the image box
  imageBox  <- boxNew
                  (toRectangle (10, 10, 600, 50))
                  (Just "TWIC Downloader")
-- Create the image box
  infoBox  <- boxNew
                  (toRectangle (10, 40, 600, 50))
                  (Just "Download games from https://theweekinchess.com/ website.")

  -- Create the input fields for index from and index to
  indexFromInput <- inputNew
                     (toRectangle (100, 150, 280, 25))
                     (Just "Index from #:")
                     (Just FlIntInput)
  setMaximumSize indexFromInput 8
  indexToInput <- inputNew
                   (toRectangle (100, 190, 280, 25))
                   (Just "Index to #:")
                   (Just FlIntInput)
  setMaximumSize indexToInput 8

  -- Create the text buffer for logs and the text display widget
  buff <- textBufferNew Nothing Nothing
  logswindow <- textDisplayNew
                 (toRectangle (10, 250, 580, 100))
                 Nothing
  setBuffer logswindow (Just buff)

  -- Create the download button and the text channel
  buttonDownload <- buttonNew
              (Rectangle (Position (X 10) (Y 360)) (Size (Width 280) (Height 30)))
              (Just "Download")
  c <- newTChanIO

  -- Set the callback for the download button
  setCallback buttonDownload (\_ ->  do
     indexFrom <- Helper.ioTextToInt =<< getValue indexFromInput
     indexTo <- Helper.ioTextToInt =<< getValue indexToInput
     -- TODO: Add a field for specifying the download path?
     void $ forkIO $ (handleTWICDownload indexFrom indexTo c)
     )

  -- Add the tick operation to update the text buffer
  FL.addTimeout 0.025 (Helper.tick buff c)

  end window
  showWidget window

-- | Handle the TWIC download operation.
handleTWICDownload :: Integer -> Integer -> TChan T.Text -> IO ()
handleTWICDownload iFrom iTo channel = do
  if iFrom > iTo
    then do
      -- If "index from" is greater than "index to", log an error message
      Helper.logFunction channel "\"Index from\" is greater than \"index to\"!" "ERROR"
      Helper.logFunction channel "Downloading didn't start" "ERROR"
    else do
      -- Call the TWIC download manager with the specified range and log function
      TWICDownloadManager.downloadAndGroup iFrom iTo "./TWIC-output" (Helper.logFunction channel)