module TIS100.Sim.CPU where

import Data.IntMap qualified as IM
import Data.Map qualified as M
import Data.Vector qualified as V
import TIS100.Errors (TISError (..), TISErrorCode (TISParseError), TISErrorOr)
import TIS100.Parser.AsmParser qualified as AP
import TIS100.Parser.Config qualified as C
import TIS100.Tiles.Base qualified as Tiles
import TIS100.Tiles.ConnectedTile (ConnectedTile (..))
import TIS100.Tiles.Inactive qualified as Inactive
import TIS100.Tiles.T21 qualified as T21
import TIS100.Tiles.T30 qualified as T30

data Tile = T21' T21.T21 | T30' T30.T30 | Inactive' Inactive.InactiveTile
  deriving (Eq, Show)

data PositionedTile = PositionedTile
  { position :: (Int, Int)
  , index :: Int
  , tile :: ConnectedTile
  }
  deriving (Eq, Show)

data CPUConfig = CPUConfig
  { rows :: Int
  , cols :: Int
  }
  deriving (Eq, Show)

data CPUState = CPUState
  { config :: CPUConfig
  , tiles :: V.Vector PositionedTile
  }
  deriving (Eq, Show)

createInitialCPUState :: C.Config -> AP.AsmSource -> TISErrorOr CPUState
createInitialCPUState cfg asm =
  let tileTypes = concat $ C.tiles cfg
   in (CPUState (CPUConfig (C.rows cfg) (C.cols cfg)) . V.fromList <$> createTiles 0 0 tileTypes)
 where
  createTiles :: Int -> Int -> [C.TileType] -> TISErrorOr [PositionedTile]
  createTiles _ _ [] = Right []
  createTiles asmIdx tileIdx (t : ts) = do
    tile' <- createTile asmIdx tileIdx t
    tiles' <- case t of
      C.Conpute -> createTiles (asmIdx + 1) (tileIdx + 1) ts
      _ -> createTiles asmIdx (tileIdx + 1) ts
    return $ tile' : tiles'

  createTile :: Int -> Int -> C.TileType -> TISErrorOr PositionedTile
  createTile asmIdx tileIdx tileType =
    let pos = tileIdx `divMod` C.cols cfg
     in case tileType of
          C.Conpute -> PositionedTile pos tileIdx . ConnectedTile . T21.createTileState <$> getTileAsm asmIdx
          C.Stack -> Right $ PositionedTile pos tileIdx $ ConnectedTile $ T30.T30 []
          C.Disabled -> Right $ PositionedTile pos tileIdx $ ConnectedTile $ Inactive.InactiveTile

  getTileAsm :: Int -> TISErrorOr T21.TileProgram
  getTileAsm i = case IM.lookup i asm of
    Nothing -> Right $ V.singleton T21.NOP
    Just a -> resolveAsm a

resolveAsm :: AP.TileAsmSource -> TISErrorOr T21.TileProgram
resolveAsm asm = resolve (locateLabels asm 0 M.empty) asm V.empty
 where
  locateLabels :: AP.TileAsmSource -> Int -> M.Map String Int -> M.Map String Int
  locateLabels [] _ m = m
  locateLabels (AP.Label l : xs) pc m = locateLabels xs (pc + 1) (M.insert l pc m)
  locateLabels (_ : xs) pc m = locateLabels xs (pc + 1) m

  resolve :: M.Map String Int -> AP.TileAsmSource -> T21.TileProgram -> TISErrorOr T21.TileProgram
  resolve _ [] p = Right p
  resolve m (AP.Label _ : xs) p = resolve m xs p
  resolve m (AP.NOP : xs) p = resolve m xs $ V.snoc p T21.NOP
  resolve m (AP.MOV (AP.Register src) dst : xs) p = resolve m xs $ V.snoc p $ T21.MOV (resolveReg src) (resolveReg dst)
  resolve m (AP.MOV (AP.Constant srci) dst : xs) p = resolve m xs $ V.snoc p $ T21.MOVI (Tiles.Value srci) (resolveReg dst)
  resolve m (AP.SWP : xs) p = resolve m xs $ V.snoc p T21.SWP
  resolve m (AP.SAV : xs) p = resolve m xs $ V.snoc p T21.SAV
  resolve m (AP.ADD (AP.Register src) : xs) p = resolve m xs $ V.snoc p $ T21.ADD (resolveReg src)
  resolve m (AP.ADD (AP.Constant srci) : xs) p = resolve m xs $ V.snoc p $ T21.ADDI (Tiles.Value srci)
  resolve m (AP.SUB (AP.Register src) : xs) p = resolve m xs $ V.snoc p $ T21.SUB (resolveReg src)
  resolve m (AP.SUB (AP.Constant srci) : xs) p = resolve m xs $ V.snoc p $ T21.SUBI (Tiles.Value srci)
  resolve m (AP.NEG : xs) p = resolve m xs $ V.snoc p T21.NEG
  resolve m (AP.JMP l : xs) p = resolveJump m xs p l $ T21.JMP
  resolve m (AP.JEZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.EZ
  resolve m (AP.JNZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.NZ
  resolve m (AP.JGZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.GZ
  resolve m (AP.JLZ l : xs) p = resolveJump m xs p l $ T21.JCC T21.LZ
  resolve m (AP.JRO (AP.Register src) : xs) p = resolve m xs $ V.snoc p $ T21.JRO (resolveReg src)
  resolve m (AP.JRO (AP.Constant srci) : xs) p = resolve m xs $ V.snoc p $ T21.JROI (Tiles.Value srci)

  resolveJump :: M.Map String Int -> [AP.LabelOrInstruction] -> T21.TileProgram -> String -> (T21.Address -> T21.Instruction) -> TISErrorOr T21.TileProgram
  resolveJump m xs p l ins = case M.lookup l m of
    Nothing -> Left $ TISError TISParseError $ "Unknown label " ++ l
    Just addr -> resolve m xs $ V.snoc p $ ins (T21.Address addr)

  resolveReg :: AP.Register -> T21.RegisterOrPort
  resolveReg AP.ACC = T21.Register T21.ACC
  resolveReg AP.NIL = T21.Register T21.NIL
  resolveReg AP.LEFT = T21.Port Tiles.LEFT
  resolveReg AP.RIGHT = T21.Port Tiles.RIGHT
  resolveReg AP.UP = T21.Port Tiles.UP
  resolveReg AP.DOWN = T21.Port Tiles.DOWN
  resolveReg AP.ANY = T21.Port Tiles.ANY
  resolveReg AP.LAST = T21.Port Tiles.LAST
