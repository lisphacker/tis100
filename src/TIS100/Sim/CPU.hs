module TIS100.Sim.CPU where

import Control.Monad (zipWithM)
import Control.Monad.RWS (MonadState (get))
import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Vector qualified as V
import Data.Vector.Mutable qualified as MV
import TIS100.Errors (TISError (..), TISErrorCode (TISParseError), TISErrorOr)
import TIS100.Parser.AsmParser qualified as AP
import TIS100.Parser.Config qualified as C
import TIS100.Sim.ConnectedTile (ConnectedTile (..))
import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.Inactive qualified as Inactive
import TIS100.Tiles.T21 qualified as T21
import TIS100.Tiles.T30 qualified as T30
import Text.Read (Lexeme (String))

-- data TileType = Undefined | T21 | T30
--   deriving (Eq, Show)

data Tile = Tile
  { pos :: (Int, Int)
  , -- , typ :: TileType
    connTile :: ConnectedTile
  }
  deriving (Show)

data CPUState = CPUState
  { rows :: Int
  , cols :: Int
  , tiles :: V.Vector Tile
  }
  deriving (Show)

createInitialCPUState :: C.Config -> AP.AsmSource -> TISErrorOr CPUState
createInitialCPUState cfg asm =
  let rows = C.rows cfg
      cols = C.cols cfg
      numTiles = rows * cols
      tileTypes = concat $ C.tiles cfg
   in (CPUState rows cols . V.fromList <$> zipWithM createTile [0 ..] tileTypes)
 where
  createTile :: Int -> C.TileType -> TISErrorOr Tile
  createTile i tileType =
    let pos = i `divMod` C.cols cfg
     in case tileType of
          C.Conpute -> Tile pos . ConnectedTile . T21.createTileState <$> getTileAsm i
          C.Stack -> Right $ Tile pos $ ConnectedTile $ T30.T30 []
          C.Disabled -> Right $ Tile pos $ ConnectedTile $ Inactive.InactiveTile

  getTileAsm :: Int -> TISErrorOr T21.TileProgram
  getTileAsm i = case IM.lookup i asm of
    Nothing -> Left $ TISError TISParseError $ "No tile asm forat index " ++ show i
    Just a -> resolveAsm a

  resolveAsm :: AP.TileAsmSource -> TISErrorOr T21.TileProgram
  resolveAsm asm = resolve (locateLabels asm 0 M.empty) asm []
   where
    locateLabels :: AP.TileAsmSource -> Int -> M.Map String Int -> M.Map String Int
    locateLabels [] _ m = m
    locateLabels (AP.Label l : xs) pc m = locateLabels xs (pc + 1) (M.insert l pc m)
    locateLabels (_ : xs) pc m = locateLabels xs (pc + 1) m

    resolve :: M.Map String Int -> AP.TileAsmSource -> T21.TileProgram -> TISErrorOr T21.TileProgram
    resolve _ [] p = Right p
    resolve m (AP.Label l : xs) p = resolve m xs p
    resolve m (AP.NOP : xs) p = resolve m xs (p ++ [T21.NOP])
    resolve m (AP.MOV (AP.Register src) dst : xs) p = resolve m xs (p ++ [T21.MOV (resolveReg src) (resolveReg dst)])
    resolve m (AP.MOV (AP.Constant srci) dst : xs) p = resolve m xs (p ++ [T21.MOVI (Tiles.Value srci) (resolveReg dst)])
    resolve m (AP.SWP : xs) p = resolve m xs (p ++ [T21.SWP])
    resolve m (AP.SAV : xs) p = resolve m xs (p ++ [T21.SAV])
    resolve m (AP.ADD (AP.Register src) : xs) p = resolve m xs (p ++ [T21.ADD (resolveReg src)])
    resolve m (AP.ADD (AP.Constant srci) : xs) p = resolve m xs (p ++ [T21.ADDI (Tiles.Value srci)])
    resolve m (AP.SUB (AP.Register src) : xs) p = resolve m xs (p ++ [T21.SUB (resolveReg src)])
    resolve m (AP.SUB (AP.Constant srci) : xs) p = resolve m xs (p ++ [T21.SUBI (Tiles.Value srci)])
    resolve m (AP.NEG : xs) p = resolve m xs (p ++ [T21.NEG])
    resolve m (AP.JMP l : xs) p = resolveJump m xs p l $ T21.JMP
    resolve m (AP.JEZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.EZ
    resolve m (AP.JNZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.NZ
    resolve m (AP.JGZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.GZ
    resolve m (AP.JLZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.LZ
    resolve m (AP.JRO (AP.Register src) : xs) p = resolve m xs (p ++ [T21.JRO (resolveReg src)])
    resolve m (AP.JRO (AP.Constant srci) : xs) p = resolve m xs (p ++ [T21.JROI (Tiles.Value srci)])

    resolveJump :: M.Map String Int -> [AP.LabelOrInstruction] -> T21.TileProgram -> String -> (T21.Address -> T21.Instruction) -> TISErrorOr T21.TileProgram
    resolveJump m xs p l ins = case M.lookup l m of
      Nothing -> Left $ TISError TISParseError $ "Unknown label " ++ l
      Just addr -> resolve m xs (p ++ [ins (T21.Address addr)])

    resolveReg :: AP.Register -> T21.RegisterOrPort
    resolveReg AP.ACC = T21.Register T21.ACC
    resolveReg AP.NIL = T21.Register T21.NIL
    resolveReg AP.LEFT = T21.Port T21.LEFT
    resolveReg AP.RIGHT = T21.Port T21.RIGHT
    resolveReg AP.UP = T21.Port T21.UP
    resolveReg AP.DOWN = T21.Port T21.DOWN
    resolveReg AP.ANY = T21.Port T21.ANY
    resolveReg AP.LAST = T21.Port T21.LAST
