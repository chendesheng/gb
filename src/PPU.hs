module PPU (execute, FIFOPixel(..), PixelFIFO(..), PPU(..), initPPU) where

import Prelude hiding (replicate)
import Bus
import Color
import Data.Vector (Vector, replicate, toList)
import Data.Word

type Pixel = Color

-- One dot = one PPU clock. One scanline = 456 dots. One frame = 154 lines = 70224 dots.

-- 160*144
newtype Display = Display (Vector (Vector Pixel))

initDisplay :: Display
initDisplay = Display (replicate 144 (replicate 160 Blank))

data FIFOPixel = FIFOPixel
  { color :: ColorIndex, -- 0 - 3
    palette :: Int, -- 0 - 7
    spritePriority :: Int,
    backgroundPriority :: Int
  }

data PixelFIFO = PixelFIFO {oam :: [FIFOPixel], background :: [FIFOPixel]}

instance Show Display where
  show (Display pixels) =
    unlines $ toList $ fmap showRow pixels
    where
      showRow :: Vector Pixel -> String
      showRow = toList . fmap showPixel

      showPixel :: Pixel -> Char
      showPixel Blank = 'W'
      showPixel LightGray = 'L'
      showPixel DarkGray = 'D'
      showPixel Black = 'B'

-- color :: Pixel -> Word32
-- color 0 = 0x9BBC0F
-- color 1 = 0x8BAC0F
-- color 2 = 0x306230
-- color _ = 0x0F380F

data PPU = PPU
  { x :: Word16
  , y :: Word8
  , mode :: PPUMode
  , lcdOn :: Bool
  , display :: Display
  , selectedOAMObjects :: [SelectedOAMObject] -- up to 10, reversed
  , bus :: Bus
  }

data SelectedOAMObject = SelectedOAMObject
  { oamIndex :: Word16
  , position :: OAMPosition
  }

initPPU :: Bus -> PPU
initPPU = PPU 0 0 HorizontalBlank False initDisplay []

-- newtype Tile = Tile (Vector Word8) -- length 16

data PPUMode
  = HorizontalBlank
  | VerticalBlank
  | OAMScan (Maybe SelectedOAMObject)
  | DrawingPixels

initOAMScan :: PPUMode
initOAMScan = OAMScan Nothing

toIntMode :: PPUMode -> Word8
toIntMode HorizontalBlank = 0
toIntMode VerticalBlank = 1
toIntMode (OAMScan _) = 2
toIntMode DrawingPixels = 3

step :: PPU -> IO PPU
step ppu = do
  let bus = ppu.bus
  case ppu.mode of
    OAMScan Nothing -> do
      -- even dot
      -- read entry  
      let i = ppu.x `div` 2
      pos <- readOAMPosition i ppu.bus
      let mode = OAMScan (Just $ SelectedOAMObject i pos)
      let selected = if i == 0 then [] else ppu.selectedOAMObjects
      syncPPUToBus $ ppu{x=ppu.x+1, mode=mode, selectedOAMObjects=selected}
    OAMScan (Just pending) -> do
      -- `LY` falls within `[Y - 16, Y - 16 + height)`
      size <- readLcdC 2 bus
      let height = if size then 16 else 8
      let ly = fromIntegral ppu.y :: Int
      let y = pending.position.yPos
      let selected = if y - 16 <= ly && ly < y - 16 + height && length ppu.selectedOAMObjects < 10 then
                        pending : ppu.selectedOAMObjects 
                    else
                        ppu.selectedOAMObjects

      let mode = if ppu.x == 79 then DrawingPixels else OAMScan Nothing
      syncPPUToBus $ ppu{x=ppu.x+1, mode=mode, selectedOAMObjects=selected}
    _ -> do
      -- TOOD
      let ppu' = advanceXY ppu
      syncPPUToBus ppu'

syncPPUToBus :: PPU -> IO PPU
syncPPUToBus ppu = do
  syncPPU ppu.y (toIntMode ppu.mode) ppu.bus
  return ppu

advanceXY :: PPU -> PPU
advanceXY ppu
  | ppu.x == 455 = ppu{x = 0, y = if ppu.y == 153 then 0 else ppu.y + 1}
  | otherwise    = ppu{x = ppu.x + 1}

execute :: Word64 -> PPU -> IO PPU
execute 0 ppu = return ppu
execute duration ppu = do
  let wasOn = ppu.lcdOn
  isOn <- isLcdOn ppu.bus
  case (wasOn, isOn) of
    (False, True) -> execute duration ppu{lcdOn=True, mode=initOAMScan}
    (True,  False) -> do
      syncPPU 0 0 ppu.bus
      return ppu{y=0, x=0, lcdOn=False}
    (False, False) -> return ppu
    (True, True) -> do
      ppu1 <- step ppu
      execute (duration - 1) ppu1


