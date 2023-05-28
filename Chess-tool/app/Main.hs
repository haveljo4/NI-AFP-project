{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Lib
import qualified CommandLineView

main :: IO ()
--main = Lib.main
main = CommandLineView.main

replMain :: IO ()
replMain = Lib.replMain
