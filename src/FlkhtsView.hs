{-# LANGUAGE OverloadedStrings #-}
module FlkhtsView where
import Graphics.UI.FLTK.LowLevel.FL
import Graphics.UI.FLTK.LowLevel.Fl_Enumerations
import Graphics.UI.FLTK.LowLevel.Fl_Types
import Graphics.UI.FLTK.LowLevel.Widget
import Graphics.UI.FLTK.LowLevel.Window
import Graphics.UI.FLTK.LowLevel.Button
import Graphics.UI.FLTK.LowLevel.Input
import qualified Graphics.UI.FLTK.LowLevel.FL as FL


--ui :: IO ()
--ui = do
--  window <- windowNew
--             (Size (Width 300) (Height 200))
--             Nothing
--             (Just "Download Window")
--
--  input1 <- inputNew
--             (Rectangle (Position (X 10) (Y 10)) (Size (Width 280) (Height 25)))
--             (Just "Input 1")
--  input2 <- inputNew
--             (Rectangle (Position (X 10) (Y 40)) (Size (Width 280) (Height 25)))
--             (Just "Input 2")
--  input3 <- inputNew
--             (Rectangle (Position (X 10) (Y 70)) (Size (Width 280) (Height 25)))
--             (Just "Input 3")
--  input4 <- inputNew
--             (Rectangle (Position (X 10) (Y 100)) (Size (Width 280) (Height 25)))
--             (Just "Input 4")
--
--  button <- buttonNew
--              (Rectangle (Position (X 10) (Y 130)) (Size (Width 280) (Height 30)))
--              (Just "Download")
--  setCallback button (\_ -> putStrLn "Download button clicked")
--
--main :: IO ()
--main = ui >> FL.run >> FL.flush



