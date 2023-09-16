module CmdLine where

import Options.Applicative

-- import Data.String.Interpolate (i)
-- import TIS100

-- main :: IO ()
-- main = do
--   let a = 1
--   let b = 2
--   putStrLn [i| Sum of #{a} and #{b} is #{sum' a b}|]

data CmdLineOpts = CmdLineOpts
  { asmFilePath :: String
  }

sample :: Parser CmdLineOpts
sample =
  CmdLineOpts
    <$> strArgument
      ( metavar "ASMFILE"
          <> help "Assembly file name"
      )

greet :: CmdLineOpts -> IO ()
greet (CmdLineOpts asmFilePath) = putStrLn $ "asm: " ++ asmFilePath
greet _ = return ()