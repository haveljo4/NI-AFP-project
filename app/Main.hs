{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Frontend.Flkhts.MainView as FlkhtsView

main :: IO ()
main = FlkhtsView.main
--main = CommandLineView.main

replMain :: IO ()
replMain = FlkhtsView.replMain
