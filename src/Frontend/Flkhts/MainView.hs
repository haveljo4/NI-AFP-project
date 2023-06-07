{-# LANGUAGE OverloadedStrings #-}
module Frontend.Flkhts.MainView (main,replMain) where
import qualified Graphics.UI.FLTK.LowLevel.FL as FL
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.FLTKHS
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Backend.TWICDownloadManager as TWICDownloadManager
import qualified Backend.ChessComDownloadManager as ChessComDownloadManager
import qualified Frontend.Flkhts.TWICWindow as TWICWin
import qualified Frontend.Flkhts.ChessComWindow as ChessComWin
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

-- | Start the UI.
uiStart :: IO ()
uiStart = do
  -- Create the main window
  window <- windowNew
             (Size (Width 600) (Height 400))
             Nothing
             (Just "Chess tool")

  -- Create the Chess.com button
  buttonChessCom <- buttonNew
                (Rectangle (Position (X 10) (Y 30)) (Size (Width 180) (Height 30)))
                (Just "Chess.com")
  -- Set the callback for the Chess.com button
  setCallback buttonChessCom (\_ -> do ChessComWin.uiChessComDownload)

  -- Create the TWIC button
  buttonTwic <- buttonNew
                (Rectangle (Position (X 190) (Y 30)) (Size (Width 180) (Height 30)))
                (Just "TWIC")
  -- Set the callback for the TWIC button
  setCallback buttonTwic (\_ -> do TWICWin.uiTwicDownload)

  -- Create the Join pgns button, TODO not implemented yet
  buttonJoinPGNs <- buttonNew
                (Rectangle (Position (X 370) (Y 30)) (Size (Width 180) (Height 30)))
                (Just "Join pgns")
  text <- boxNew
            (Rectangle (Position (X 190) (Y 360)) (Size (Width 180) (Height 30)))
            (Just "Copyright Josef Havelka (c) 2023")

  -- Add the widgets to the window
  end window
  showWidget window

-- | The main entry point.
main :: IO ()
main = uiStart >> FL.run >> FL.flush

-- | The REPL entry point.
replMain :: IO ()
replMain = uiStart >> FL.replRun