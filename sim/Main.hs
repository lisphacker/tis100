{-# LANGUAGE QuasiQuotes #-}

module Main where

import CmdLine
import Options.Applicative

main :: IO ()
main = greet =<< execParser opts
  where
    opts =
      info
        (sample <**> helper)
        ( fullDesc
            <> progDesc "Print a greeting for TARGET"
            <> header "hello - a test for optparse-applicative"
        )
