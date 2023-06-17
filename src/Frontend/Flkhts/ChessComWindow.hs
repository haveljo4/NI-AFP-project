{-# LANGUAGE OverloadedStrings #-}
module Frontend.Flkhts.ChessComWindow where
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


-- | Download Chess.com data.
uiChessComDownload :: IO ()
uiChessComDownload = do
  let winWidth  = 800
      winHeight = 600
  -- Create the Chess.com download window
  window <- windowNew
             (Size (Width winWidth) (Height winHeight))
             Nothing
             (Just "ChessCom Download Window")
  begin window
  Helper.setIconToWindow window

  -- Create the image box
  imageBox  <- boxNew
                  (toRectangle (10, 10, winWidth, 50))
                  (Just "ChessCom Downloader")
                                    
  imageBox  <- boxNew
                  (toRectangle (10, 40, winWidth, 50))
                  (Just "Download games from https://www.chess.com/ website.")

  -- Create the output folder input field
  outputFolderInput <- inputNew
                     (toRectangle (100, 110, 680, 25))
                     (Just "Output folder:")
                     (Just FlNormalInput)
  setMaximumSize outputFolderInput 255
  currentFolder <- Helper.getCurrentFolder
  _ <- setValue outputFolderInput (T.pack currentFolder)
  
  -- Create the user name input field
  userNameInput <- inputNew
                     (toRectangle (100, 150, 120, 25))
                     (Just "User name:")
                     (Just FlNormalInput)
  setMaximumSize userNameInput 255

  -- Create the year range input fields
  yearFromInput <- inputNew
                     (toRectangle (330, 150, 120, 25))
                     (Just "Year from #:")
                     (Just FlIntInput)
  setMaximumSize yearFromInput 4
  yearToInput <- inputNew
                     (toRectangle (560, 150, 120, 25))
                     (Just "Year to #:")
                     (Just FlIntInput)
  setMaximumSize yearToInput 4

  -- Create the month range input fields
  monthFromInput <- inputNew
                   (toRectangle (330, 190, 120, 25))
                   (Just "Month from #:")
                   (Just FlIntInput)
  setMaximumSize monthFromInput 2
  monthToInput <- inputNew
                     (toRectangle (560, 190, 120, 25))
                     (Just "Month to #:")
                     (Just FlIntInput)
  setMaximumSize monthToInput 2

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
  setCallback buttonDownload (\_ ->  do
     yearFrom <- Helper.ioTextToInt =<< getValue yearFromInput
     monthFrom <- Helper.ioTextToInt =<< getValue monthFromInput
     yearTo <- Helper.ioTextToInt =<< getValue yearToInput
     monthTo <- Helper.ioTextToInt =<< getValue monthToInput
     username <- Helper.ioTextToString  =<< getValue userNameInput
     outputFolder <- Helper.ioTextToString  =<< getValue outputFolderInput
     void $ forkIO $ (handleChessComDownload yearFrom monthFrom yearTo monthTo username c outputFolder)
     )
  FL.addTimeout 0.025 (Helper.tick buff c)
  -- Add the widgets to the window
  end window
  showWidget window

-- | Handle Chess.com download.
handleChessComDownload :: Integer -> Integer -> Integer -> Integer -> String -> TChan T.Text -> FilePath -> IO ()
handleChessComDownload yearFrom monthFrom yearTo monthTo username channel outputFolder = do
  case validateDates yearFrom monthFrom yearTo monthTo of
    Left msg -> Helper.logFunction channel msg "ERROR"
    Right _ -> do
      ChessComDownloadManager.downloadAndGroup yearFrom monthFrom yearTo monthTo username outputFolder (Helper.logFunction channel) 
      pure ()
      
-- | Validate the date range.
validateDates :: Integer -> Integer -> Integer -> Integer -> Either String ()
validateDates yearFrom monthFrom yearTo monthTo
  | yearFrom > yearTo = Left "Invalid date range: From year is after To year"
  | yearFrom == yearTo && monthFrom > monthTo = Left "Invalid date range: From month is after To month"
  | not (isValidMonth monthFrom) = Left "Invalid month: From month is not in the range 1-12"
  | not (isValidMonth monthTo) = Left "Invalid month: To month is not in the range 1-12"
  | otherwise = Right ()
  where
    -- Check if a month is valid (between 1 and 12)
    isValidMonth :: Integer -> Bool
    isValidMonth month = month >= 1 && month <= 12