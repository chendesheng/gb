{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
module PPU (execute, FIFOPixel(..), PPU(..), initPPU) where

import Prelude hiding (replicate)
import Control.Monad (when)
import Bus
import Color
import Data.Vector (Vector, replicate, toList, snoc, modify, (//), (!))
import Data.Vector.Algorithms.Intro (sort)
import Data.Word
import Queue hiding (toList)


-- One dot = one PPU clock. One scanline = 456 dots. One frame = 154 lines = 70224 dots.

-- 160*144
newtype Display = Display (Vector (Vector Color))

initDisplay :: Display
initDisplay = Display (replicate 144 (replicate 160 Blank))

renderPixel :: Word8 -> Word8 -> Color -> Display -> Display
renderPixel y x !color (Display rows) =
  let y' = fromIntegral y
      row = rows ! y'
      x' = fromIntegral x
      !row' = row // [(x', color)]
      !res = Display $ rows // [(y', row')]
  in res

data FIFOPixel = FIFOPixel
  { color :: ColorIndex -- 0 - 3
  , palette :: Int -- 0 - 7
  -- , spritePriority :: Int -- not used for DMG
  , backgroundPriority :: Int
  }

-- data PixelFIFO = PixelFIFO {oam :: [FIFOPixel], background :: [FIFOPixel]}

instance Show Display where
  show (Display pixels) =
    unlines $ toList $ fmap showRow pixels
    where
      showRow :: Vector Color -> String
      showRow = toList . fmap showPixel

      showPixel :: Color -> Char
      -- change to use terminal color rect (unicode rect with terminal color) instead
      showPixel Blank = '\x2588'
      showPixel LightGray = '\x2591'
      showPixel DarkGray = '\x2592'
      showPixel Black = '\x2593'

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
  , selectedOAMObjects :: Vector SelectedOAMObject -- up to 10, reversed
  , bus :: Bus
  }

data SelectedOAMObject = SelectedOAMObject
  { oamIndex :: Word8
  , entry :: OAMEntry
  }

instance Eq SelectedOAMObject where
  (==) a b = a.oamIndex == b.oamIndex

instance Ord SelectedOAMObject where
  compare a b =
    case compare a.entry.xPos b.entry.xPos of
      EQ -> compare a.oamIndex b.oamIndex
      others -> others

initPPU :: Bus -> PPU
initPPU = PPU 0 0 HorizontalBlank False initDisplay mempty

-- newtype Tile = Tile (Vector Word8) -- length 16

data PPUMode
  = HorizontalBlank
  | VerticalBlank
  | OAMScan (Maybe SelectedOAMObject)
  | DrawingPixels
      { fetcherStep :: FIFOPixelFetcherStep
      , fetcherX :: Word8
      , screenX :: Maybe Int
      , oam :: Queue FIFOPixel
      , background :: Queue FIFOPixel
      }

instance Eq PPUMode where
  HorizontalBlank == HorizontalBlank = True
  VerticalBlank == VerticalBlank = True
  OAMScan a == OAMScan b = a == b
  DrawingPixels {} == DrawingPixels {} = True
  _ == _ = False

data FIFOPixelFetcherStep
  = GetTileIndex Int
  | GetTileDataLow Int TileIndex
  | GetTileDataHigh Int TileIndex Word8
  | Sleep Int TileRow
  | Push TileRow

initOAMScan :: PPUMode
initOAMScan = OAMScan Nothing

initDrawingPixels :: PPUMode
initDrawingPixels = DrawingPixels (GetTileIndex 0) 0 Nothing mempty mempty

toIntMode :: PPUMode -> Word8
toIntMode HorizontalBlank = 0
toIntMode VerticalBlank = 1
toIntMode (OAMScan _) = 2
toIntMode (DrawingPixels {}) = 3

step :: PPU -> IO PPU
step ppu = do
  let bus = ppu.bus
  case ppu.mode of
    OAMScan Nothing -> do
      -- even dot
      -- read entry
      let i = fromIntegral ppu.x `div` 2
      pos <- readOAMEntry i ppu.bus
      let mode = OAMScan (Just $ SelectedOAMObject i pos)
      let selected = if i == 0 then mempty else ppu.selectedOAMObjects
      return $ ppu{mode=mode, selectedOAMObjects=selected}
    OAMScan (Just pending) -> do
      -- `LY` falls within `[Y - 16, Y - 16 + height)`
      height <- readLcdCObjSize bus
      let ly = fromIntegral ppu.y :: Int
      let y = pending.entry.yPos
      let selected = if y - 16 <= ly && ly < y - 16 + height && length ppu.selectedOAMObjects < 10 then
                        snoc ppu.selectedOAMObjects pending
                     else
                        modify sort ppu.selectedOAMObjects

      let mode = if ppu.x == 79 then initDrawingPixels else OAMScan Nothing
      return $ ppu{mode=mode, selectedOAMObjects=selected}
    DrawingPixels fetcherStep fetcherX screenX oam bg -> do
      bgEnable <- isLcdCBgEnable bus
      if bgEnable then do
        ppu' <- executeFetcherStep ppu
        render ppu'
      else
        -- TODO
        return ppu
      where
        render ppu =
            case ppu.mode of
              DrawingPixels a b maybeScreenX d bg ->
                case dequeue bg of
                  Just (pixel, bg') -> do
                    screenX <- resolveScreenX maybeScreenX
                    if screenX < 0 then
                      return ppu{mode=DrawingPixels a b (Just $ screenX + 1) d bg'}
                    else do
                      palette <- readBGPalette bus
                      let color = getColor pixel.color palette
                      let !display' = renderPixel ppu.y (fromIntegral screenX) color ppu.display
                      let screenX' = screenX + 1
                      if screenX' == 160 then
                        return ppu{mode=HorizontalBlank, display=display'}
                      else
                        return ppu{mode=DrawingPixels a b (Just screenX') d bg', display=display'}
                  _ ->
                    return ppu
                where
                  resolveScreenX :: Maybe Int -> IO Int
                  resolveScreenX Nothing = do
                    scx <- readSCX bus
                    return $ -(fromIntegral $ scx `mod` 8)
                  resolveScreenX (Just sx) = return sx
              _ -> return ppu

        executeFetcherStep ppu =
            case fetcherStep of
              GetTileIndex 1 -> do
                -- TODO
                tileIndex <- readBgTileIndex ppu.y (fetcherX * 8) bus
                return ppu{mode=DrawingPixels (GetTileDataLow 0 tileIndex) fetcherX screenX oam bg}
              GetTileIndex _ ->
                return ppu{mode=DrawingPixels (GetTileIndex 1) fetcherX screenX oam bg}
              GetTileDataLow 1 tileIndex -> do
                scy <- readSCY bus
                low <- readBgTileRowLow tileIndex (scy + ppu.y) bus
                return ppu{mode=DrawingPixels (GetTileDataHigh 0 tileIndex low) fetcherX screenX oam bg}
              GetTileDataLow _ tileIndex ->
                return ppu{mode=DrawingPixels (GetTileDataLow 1 tileIndex) fetcherX screenX oam bg}
              GetTileDataHigh 1 tileIndex low -> do
                scy <- readSCY bus
                high <- readBgTileRowHigh tileIndex (scy + ppu.y) bus
                return ppu{mode=DrawingPixels (Sleep 0 (low, high)) fetcherX screenX oam bg}
              GetTileDataHigh _ tileIndex low ->
                return ppu{mode=DrawingPixels (GetTileDataHigh 1 tileIndex low) fetcherX screenX oam bg}
              Sleep 1 tileRow ->
                return ppu{mode=DrawingPixels (Push tileRow) fetcherX screenX oam bg}
              Sleep _ tileRow ->
                return ppu{mode=DrawingPixels (Sleep 1 tileRow) fetcherX screenX oam bg}
              Push tileRow ->
                if isEmpty bg then do
                  let bg' = foldl' (\acc colorIndex -> enqueue (FIFOPixel colorIndex 0 0) acc) bg (tileRowColorIndexes tileRow)
                  return ppu{mode=DrawingPixels (GetTileIndex 0) (fetcherX + 1) screenX oam bg'}
                else
                  return ppu
    HorizontalBlank ->
      return ppu

    VerticalBlank ->
      return ppu

advanceXY :: PPU -> PPU
advanceXY ppu
  | ppu.x == 455 =
    let y = if ppu.y == 153 then 0 else ppu.y + 1
        mode = if y < 144 then OAMScan Nothing else VerticalBlank
    in ppu{x=0, y=y, mode=mode}
  | otherwise = ppu{x=ppu.x+1}

readBgTileIndex :: Word8 -> Word8 -> Bus -> IO TileIndex
readBgTileIndex ly x bus = do
  base <- readLcdCBgTileMapArea bus
  scx <- readSCX bus
  scy <- readSCY bus
  let bgY = ly + scy
  let bgX = x + scx
  readTileIndex base bgY bgX bus

syncPPUToBus :: PPU -> PPU -> IO ()
syncPPUToBus oldPPU ppu = do
  syncPPU ppu.y (toIntMode ppu.mode) ppu.bus
  -- use if instead
  when (oldPPU.mode /= ppu.mode) $
    case ppu.mode of
      VerticalBlank -> writeIF VBlank True ppu.bus
      _ -> return ()

execute :: Word8 -> PPU -> IO PPU
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
      let ppu2 = advanceXY ppu1
      syncPPUToBus ppu ppu2
      execute (duration - 1) ppu2
