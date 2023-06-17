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
  let winWidth  = 800
      winHeight = 600
  -- Create the main window
  window <- windowNew
             (Size (Width winWidth) (Height winHeight))
             Nothing
             (Just "TWIC Download Window")
  begin window
  Helper.setIconToWindow window

  -- Create the box with name of the window
  imageBox  <- boxNew
                  (toRectangle (10, 10, winWidth, 50))
                  (Just "TWIC Downloader")
                    
-- Create the image box
  imageBox  <- boxNew
                  (toRectangle (10, 10, winWidth, 50))
                  (Just "TWIC Downloader")
-- Create the image box
  infoBox  <- boxNew
                  (toRectangle (10, 40, winWidth, 50))
                  (Just "Download games from https://theweekinchess.com/ website.")

  -- Create the output folder input field
  outputFolderInput <- inputNew
                     (toRectangle (100, 110, 680, 25))
                     (Just "Output folder:")
                     (Just FlNormalInput)
  setMaximumSize outputFolderInput 255
  currentFolder <- Helper.getCurrentFolder
  _ <- setValue outputFolderInput (T.pack currentFolder)
--                     
  -- Create the input fields for index from and index to
  indexFromInput <- inputNew
                     (toRectangle (100, 150, 120, 25))
                     (Just "Index from #:")
                     (Just FlIntInput)
  setMaximumSize indexFromInput 8
  indexToInput <- inputNew
                   (toRectangle (100, 190, 120, 25))
                   (Just "Index to #:")
                   (Just FlIntInput)
  setMaximumSize indexToInput 8

  -- Create the text buffer for logs and the text display widget
  buff <- textBufferNew Nothing Nothing
  logswindow <- textDisplayNew
                 (toRectangle (10, 250, winWidth - 20, winHeight - 250 - 50))
                 Nothing
  setBuffer logswindow (Just buff)

  -- Create the download button and the text channel
  buttonDownload <- buttonNew
              (Rectangle (Position (X 10) (Y (winHeight - 40 ))) (Size (Width (winWidth - 20)) (Height 30)))
              (Just "Download")
  c <- newTChanIO

  -- Set the callback for the download button
  setCallback buttonDownload (\_ ->  do
     indexFrom <- Helper.ioTextToInt =<< getValue indexFromInput
     indexTo <- Helper.ioTextToInt =<< getValue indexToInput
     outputFolder <- Helper.ioTextToString =<< getValue outputFolderInput
     void $ forkIO $ (handleTWICDownload indexFrom indexTo outputFolder c )
     )

  -- Add the tick operation to update the text buffer
  FL.addTimeout 0.025 (Helper.tick buff c)

  end window
  showWidget window

-- | Handle the TWIC download operation.
handleTWICDownload :: Integer -> Integer -> FilePath -> TChan T.Text ->  IO ()
handleTWICDownload iFrom iTo outputFolder channel  = do
  if iFrom > iTo
    then do
      -- If "index from" is greater than "index to", log an error message
      Helper.logFunction channel "\"Index from\" is greater than \"index to\"!" "ERROR"
      Helper.logFunction channel "Downloading didn't start" "ERROR"
    else do
      -- Call the TWIC download manager with the specified range and log function
      TWICDownloadManager.downloadAndGroup iFrom iTo outputFolder (Helper.logFunction channel)