{-# LANGUAGE QuasiQuotes #-}

module Main where

import CmdLine (parseCmdLine)

main :: IO ()
main = putStrLn <$> show =<< parseCmdLine
