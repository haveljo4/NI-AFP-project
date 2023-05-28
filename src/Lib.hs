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
                 (toRectangle (10, 250, 500, 100))
                 Nothing
  setBuffer logswindow (Just buff)

  buttonDownload <- buttonNew
              (Rectangle (Position (X 10) (Y 360)) (Size (Width 280) (Height 30)))
              (Just "Download")
  setCallback buttonDownload (\_ ->  do
     indexFrom <- ioTextToInt =<< getValue indexFromInput
     indexTo <- ioTextToInt =<< getValue indexToInput
--     inputValue' <- getValue intInput' >>= return . read . T.unpack
--    TODO field for specifying path?
     handleDownload indexFrom indexTo buff
--     appendToBuffer buff  ("\nDownloading!!" <>  indexFrom <>  indexTo)
     )
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
  yearFromInput <- inputNew
                     (toRectangle (100, 150, 120, 25))
                     (Just "year from #:")
                     (Just FlIntInput)
  setMaximumSize yearFromInput 4
  yearToInput <- inputNew
                     (toRectangle (260, 150, 120, 25))
                     (Just "year to #:")
                     (Just FlIntInput)
  setMaximumSize yearToInput 4

  monthFromInput <- inputNew
                   (toRectangle (100, 190, 280, 25))
                   (Just "Index to #:")
                   (Just FlIntInput)
  setMaximumSize monthFromInput 2
  monthToInput <- inputNew
                     (toRectangle (100, 190, 280, 25))
                     (Just "Index to #:")
                     (Just FlIntInput)
  setMaximumSize monthToInput 2

  buff <- textBufferNew Nothing Nothing
  logswindow <- textDisplayNew
                 (toRectangle (10, 250, 500, 100))
                 Nothing
  setBuffer logswindow (Just buff)


  buttonDownload <- buttonNew
              (Rectangle (Position (X 10) (Y 360)) (Size (Width 280) (Height 30)))
              (Just "Download")

--  setCallback button (\_ ->  do appendToBuffer buff  "\nHello, FLTKHS!")
--  setCallback buttonDownload (\_ ->  do
--     indexFrom <- ioTextToInt =<< getValue indexFromInput
--     indexTo <- ioTextToInt =<< getValue indexToInput
----     inputValue' <- getValue intInput' >>= return . read . T.unpack
----    TODO field for specifying path?
--     handleDownload indexFrom indexTo buff
----     appendToBuffer buff  ("\nDownloading!!" <>  indexFrom <>  indexTo)
--     )
  end window
  showWidget window



handleDownload :: Integer -> Integer -> Ref TextBuffer -> IO ()
handleDownload iFrom iTo buff = do
   if iFrom > iTo
      then  do
        logFunction buff "\"Index from\" is greater then \"index to\"!" "ERROR"
        logFunction buff "Downloading didn't start" "ERROR"
      else do
        _ <- forkIO $ TWICDownloadManager.downloadAndGroup iFrom iTo "./" (logFunction buff)
        --     TODO this is not nice
        putStr ""

logFunction :: Ref TextBuffer -> String -> String -> IO ()
logFunction buff msg lvl = do
  currentTime <- getCurrentTime
  let formattedTime = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" currentTime
  let logMsg = formattedTime <> " [" <> lvl <> "]: " <> msg <> "\n"
  appendToBuffer buff (T.pack logMsg)

ioTextToInt :: T.Text -> IO Integer
ioTextToInt ioText = do
  let text = T.unpack ioText
  return (read text)

main :: IO ()
main = uiStart >> FL.run >> FL.flush

replMain :: IO ()
replMain = uiStart >> FL.replRun
