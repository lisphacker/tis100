module TIS100.Sim.Run where

import Data.Vector qualified as V
import Foreign qualified as V
import TIS100.Parser.Config qualified as CFG
import TIS100.Sim.CPU qualified as CPU

step :: CFG.Config -> CPU.CPUState -> IO CPU.CPUState
step cfg cpu = do
  mtiles <- V.thaw $ CPU.tiles cpu
  tiles <- V.freeze mtiles
  return $ cpu{CPU.tiles = tiles}