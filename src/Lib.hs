{-# LANGUAGE OverloadedStrings #-}
module Lib (main,replMain) where
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

uiStart :: IO ()
uiStart = do
    window <- windowNew
               (Size (Width 600) (Height 400))
               Nothing
               (Just "Chess tool")

    buttonChessCom <- buttonNew
                (Rectangle (Position (X 10) (Y 30)) (Size (Width 180) (Height 30)))
                (Just "Chesscom")
    setCallback buttonChessCom (\_ -> do uiChessComDownload)
    buttonTwic <- buttonNew
                (Rectangle (Position (X 190) (Y 30)) (Size (Width 180) (Height 30)))
                (Just "TWIC")
    setCallback buttonTwic (\_ -> do uiTwicDownload)
    buttonJoinPGNs <- buttonNew
                (Rectangle (Position (X 370) (Y 30)) (Size (Width 180) (Height 30)))
                (Just "Join pgns")
    end window
    showWidget window


uiTwicDownload :: IO ()
uiTwicDownload = do
  window <- windowNew
             (Size (Width 600) (Height 400))
             Nothing
             (Just "TWIC Download Window")
  begin window

  imageBox  <- boxNew
                  (toRectangle (10, 10, 600, 50))
                  (Just "TWIC Downloader")

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

  buff <- textBufferNew Nothing Nothing
  logswindow <- textDisplayNew
                 (toRectangle (10, 250, 580, 100))
                 Nothing
  setBuffer logswindow (Just buff)

  buttonDownload <- buttonNew
              (Rectangle (Position (X 10) (Y 360)) (Size (Width 280) (Height 30)))
              (Just "Download")
  c <- newTChanIO
  setCallback buttonDownload (\_ ->  do
     indexFrom <- ioTextToInt =<< getValue indexFromInput
     indexTo <- ioTextToInt =<< getValue indexToInput
--     inputValue' <- getValue intInput' >>= return . read . T.unpack
--    TODO field for specifying path?
     void $ forkIO $ (handleTWICDownload indexFrom indexTo c)
--     appendToBuffer buff  ("\nDownloading!!" <>  indexFrom <>  indexTo)
     )
  FL.addTimeout 0.025 (tick buff c)

  end window
  showWidget window

uiChessComDownload :: IO ()
uiChessComDownload = do
  window <- windowNew
             (Size (Width 600) (Height 400))
             Nothing
             (Just "ChessCom Download Window")
  begin window

  imageBox  <- boxNew
                  (toRectangle (10, 10, 600, 50))
                  (Just "ChessCom Downloader")
  userNameInput <- inputNew
                     (toRectangle (100, 110, 120, 25))
                     (Just "User name :")
                     (Just FlNormalInput)
  setMaximumSize userNameInput 255

  yearFromInput <- inputNew
                     (toRectangle (100, 150, 120, 25))
                     (Just "Year from #:")
                     (Just FlIntInput)
  setMaximumSize yearFromInput 4
  yearToInput <- inputNew
                     (toRectangle (330, 150, 120, 25))
                     (Just "Year to #:")
                     (Just FlIntInput)
  setMaximumSize yearToInput 4

  monthFromInput <- inputNew
                   (toRectangle (100, 190, 120, 25))
                   (Just "Month from #:")
                   (Just FlIntInput)
  setMaximumSize monthFromInput 2
  monthToInput <- inputNew
                     (toRectangle (330, 190, 120, 25))
                     (Just "Month to #:")
                     (Just FlIntInput)
  setMaximumSize monthToInput 2

  buff <- textBufferNew Nothing Nothing
  logswindow <- textDisplayNew
                 (toRectangle (10, 250, 580, 100))
                 Nothing
  setBuffer logswindow (Just buff)


  buttonDownload <- buttonNew
              (Rectangle (Position (X 10) (Y 360)) (Size (Width 280) (Height 30)))
              (Just "Download")
  c <- newTChanIO
  setCallback buttonDownload (\_ ->  do
     yearFrom <- ioTextToInt =<< getValue yearFromInput
     monthFrom <- ioTextToInt =<< getValue monthFromInput
     yearTo <- ioTextToInt =<< getValue yearToInput
     monthTo <- ioTextToInt =<< getValue monthToInput
     username <- ioTextToString  =<< getValue userNameInput
     void $ forkIO $ (handleChessComDownload yearFrom monthFrom yearTo monthTo username c)
     )
  FL.addTimeout 0.025 (tick buff c)
  end window
  showWidget window




handleChessComDownload :: Integer -> Integer -> Integer -> Integer -> String -> TChan T.Text -> IO ()
handleChessComDownload yearFrom monthFrom yearTo monthTo username channel = do
  case validateDates yearFrom monthFrom yearTo monthTo of
    Left msg -> logFunction channel msg "ERROR"
    Right _ -> do
      ChessComDownloadManager.downloadAndGroup yearFrom monthFrom yearTo monthTo username "./chess-com-output"  (logFunction channel)
      pure ()

validateDates :: Integer -> Integer -> Integer -> Integer -> Either String ()
validateDates yearFrom monthFrom yearTo monthTo
  | yearFrom > yearTo = Left "Invalid date range: From year is after To year"
  | yearFrom == yearTo && monthFrom > monthTo = Left "Invalid date range: From month is after To month"
  | not (isValidMonth monthFrom) = Left "Invalid month: From month is not in the range 1-12"
  | not (isValidMonth monthTo) = Left "Invalid month: To month is not in the range 1-12"
  | otherwise = Right ()
  where
    isValidMonth :: Integer -> Bool
    isValidMonth month = month >= 1 && month <= 12


handleTWICDownload :: Integer -> Integer ->  TChan T.Text -> IO ()
handleTWICDownload iFrom iTo  channel = do
   if iFrom > iTo
      then  do
        logFunction channel "\"Index from\" is greater then \"index to\"!" "ERROR"
        logFunction channel "Downloading didn't start" "ERROR"
      else do
        TWICDownloadManager.downloadAndGroup iFrom iTo "./TWIC-output" (logFunction channel)

-- Check for a message from our worker thread.  If there is a message,
-- gobble all the messages up and set the label to the contents on the
-- most recent message.
tick :: Ref TextBuffer -> TChan T.Text -> IO ()
tick b c = do
  mx <- atomically $ tryReadTChan c
  case mx of
    Nothing -> return ()
    Just x -> void $ appendToBuffer b x
  _ <- FL.repeatTimeout 0.025 (tick b c)
  return ()

sendNewLine :: TChan T.Text -> IO ()
sendNewLine chan = do
  let newLine = T.singleton '\n'
  atomically $ writeTChan chan newLine



logFunction :: TChan T.Text -> String -> String -> IO ()
logFunction channel msg lvl = do
  msg <- formatMessage msg lvl
  atomically $ writeTChan channel (T.pack msg)
  sendNewLine channel
  where 
    formatMessage :: String -> String -> IO String
    formatMessage msg lvl = do
      currentTime <- getCurrentTime
      let formattedTime = formatTime defaultTimeLocale "%H:%M:%S" currentTime
      return $ formattedTime <> " [" <> lvl <> "]: " <> msg

ioTextToInt :: T.Text -> IO Integer
ioTextToInt ioText = do
  let text = T.unpack ioText
  return (read text)

ioTextToString :: T.Text -> IO String
ioTextToString ioText = do
  let text = T.unpack ioText
  return text

main :: IO ()
main = uiStart >> FL.run >> FL.flush

replMain :: IO ()
replMain = uiStart >> FL.replRun
