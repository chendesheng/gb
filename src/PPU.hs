{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
module PPU (execute, FIFOPixel(..), PPU(..), Display(..), initPPU) where

import Prelude hiding (replicate)
import Control.Monad (when)
import Bus
import Color
import Data.Vector (Vector, replicate, toList, snoc, modify, (//), (!))
import Data.Vector.Algorithms.Intro (sort)
import Data.Word
import qualified Deque.Lazy as Dq
import Deque.Lazy (Deque)
import Data.List.Split (chunksOf)


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
    let rows = chunksOf 2 $ fmap toList (toList pixels)
    in
    unlines (fmap showRow rows)
    where
      showRow :: [[Color]] -> String
      showRow [r1, r2] = concat (zipWith showPixel r1 r2) ++ "\ESC[0m"
      showRow _ = ""

      showPixel :: Color -> Color -> String
      showPixel t b = ansiBackground t ++ ansiForeground b ++ "▄"

      ansiForeground :: Color -> String
      ansiForeground Blank = "\ESC[38;5;15m"
      ansiForeground LightGray = "\ESC[38;5;7m"
      ansiForeground DarkGray = "\ESC[38;5;8m"
      ansiForeground Black = "\ESC[38;5;0m"

      ansiBackground :: Color -> String
      ansiBackground Blank = "\ESC[48;5;15m"
      ansiBackground LightGray = "\ESC[48;5;7m"
      ansiBackground DarkGray = "\ESC[48;5;8m"
      ansiBackground Black = "\ESC[48;5;0m"

data PPU = PPU
  { x :: Word16
  , y :: Word8
  , mode :: PPUMode
  , lcdOn :: Bool
  , display :: Display
  , selectedOAMObjects :: !(Vector SelectedOAMObject) -- up to 10, reversed
  , windowLine :: Word8
  , windowYTriggered :: Bool -- the "Y condition"
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
initPPU = PPU 0 0 HorizontalBlank False initDisplay mempty 0 False

-- newtype Tile = Tile (Vector Word8) -- length 16

data PPUMode
  = HorizontalBlank
  | VerticalBlank
  | OAMScan (Maybe SelectedOAMObject)
  | DrawingPixels
      { fetcherStep :: FIFOPixelFetcherStep
      , fetcherX :: Word8
      , screenX :: Int
      , oam :: Deque FIFOPixel
      , background :: Deque FIFOPixel
      , windowXTriggered :: Bool
      , windowLine :: Word8
      , fetcherSource :: FIFOFetcherSource
      }

initFIFOPixelFetcher :: FIFOPixelFetcherStep
initFIFOPixelFetcher = GetTileIndexAddress

instance Eq PPUMode where
  HorizontalBlank == HorizontalBlank = True
  VerticalBlank == VerticalBlank = True
  OAMScan a == OAMScan b = a == b
  DrawingPixels {} == DrawingPixels {} = True
  _ == _ = False

data FIFOFetcherSource = Background | Window deriving (Eq, Show)

data FIFOPixelFetcherStep
  = GetTileIndexAddress
  | GetTileIndex Address
  | GetTileDataLowAddress FIFOFetcherSource TileIndex
  | GetTileDataLow Address FIFOFetcherSource TileIndex
  | GetTileDataHighAddress FIFOFetcherSource TileIndex Word8
  | GetTileDataHigh Address Word8
  | Push TileRow

initOAMScan :: PPUMode
initOAMScan = OAMScan Nothing

initDrawingPixels :: Int -> PPUMode
initDrawingPixels screenX = DrawingPixels initFIFOPixelFetcher 0 screenX mempty mempty False 0 Background

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
      ppu' <- updateYTriggered ppu
      return $ ppu'{mode=mode, selectedOAMObjects=selected}
    OAMScan (Just pending) -> do
      -- `LY` falls within `[Y - 16, Y - 16 + height)`
      height <- readLcdCObjSize bus
      let ly = fromIntegral ppu.y :: Int
      let y = pending.entry.yPos
      let selected = if y - 16 <= ly && ly < y - 16 + height && length ppu.selectedOAMObjects < 10 then
                        snoc ppu.selectedOAMObjects pending
                     else
                        modify sort ppu.selectedOAMObjects

      ppu' <- if ppu.x == 79 then do
                  mode <- initDrawingPixels <$> initScreenX bus
                  wx <- readWXInt bus
                  bgEnable <- isLcdCBgEnable bus
                  windowEnable <- isLcdCWindowEnable bus
                  if wx == 0 && bgEnable && windowEnable && ppu.windowYTriggered then
                    activeWindow wx ppu{mode=mode}
                  else return $ ppu{mode=mode}
              else return $ ppu{mode=OAMScan Nothing}
      return $ ppu'{selectedOAMObjects=selected}
    DrawingPixels {} -> do
      bgEnable <- isLcdCBgEnable bus
      if bgEnable then do
        windowEnable <- isLcdCWindowEnable bus
        ppu1 <- if windowEnable then tryActiveWindow ppu ppu.mode else return ppu
        let windowActive = windowEnable && ppu1.windowYTriggered && ppu1.mode.windowXTriggered
        ppu2 <- render ppu1 ppu1.mode
        executeFetcherStep ppu2 ppu2.mode windowActive
      else
        -- TODO
        return ppu
      where
        tryActiveWindow ppu mode@(DrawingPixels{}) = do
          if ppu.windowYTriggered && not mode.windowXTriggered then do
            wx <- readWXInt bus
            let windowXTriggered = wx > 0 && (wx < 7 && (mode.screenX == 0) || wx - 7 == mode.screenX)
            if windowXTriggered then activeWindow wx ppu
            else return ppu
          else return ppu
        tryActiveWindow ppu _ = return ppu

        render ppu mode@(DrawingPixels{screenX=screenX}) =
          case Dq.uncons mode.background of
            Just (pixel, background) -> do
              if mode.screenX < 0 then do
                return ppu{mode=mode{screenX=screenX + 1, background=background}}
              else do
                palette <- readBGPalette bus
                let color = getColor pixel.color palette
                let !display' = renderPixel ppu.y (fromIntegral screenX) color ppu.display
                let screenX' = screenX + 1
                let mode' = if screenX' == 160 then HorizontalBlank else mode{screenX=screenX', background=background}
                return ppu{mode=mode', display=display'}
            _ -> return ppu
        render ppu _ = return ppu

        executeFetcherStep ppu mode@(DrawingPixels {}) windowActive =
            case mode.fetcherStep of
              GetTileIndexAddress -> do
                let source = if windowActive then Window else Background
                mode' <- if mode.fetcherSource == Window && source == Background then do
                              -- switch from window to background
                              return $ mode{fetcherX=fromIntegral (mode.screenX + length mode.background)
                                           , windowXTriggered=False
                                           }
                            else return mode
                addr <- case source of
                    Window -> readWindowTileIndexAddr mode'.windowLine mode'.fetcherX ppu.bus
                    Background -> readBgTileIndexAddr ppu.y mode'.fetcherX bus
                return ppu{mode=mode'{fetcherStep=GetTileIndex addr, fetcherSource=source}}
              GetTileIndex addr -> do
                tileIndex <- readVRam addr bus
                return ppu{mode=mode{fetcherStep=GetTileDataLowAddress mode.fetcherSource tileIndex}}
              GetTileDataLowAddress source tileIndex -> do
                addr <- case source of
                          Window -> readBgTileRowBaseAddress tileIndex mode.windowLine bus
                          Background -> do
                            scy <- readSCY bus
                            readBgTileRowBaseAddress tileIndex (scy + ppu.y) bus
                return ppu{mode=mode{fetcherStep=GetTileDataLow addr source tileIndex}}
              GetTileDataLow addr source tileIndex -> do
                low <- readVRam addr bus
                return ppu{mode=mode{fetcherStep=GetTileDataHighAddress source tileIndex low}}
              GetTileDataHighAddress source tileIndex low -> do
                addr <- case source of
                          Window -> readBgTileRowBaseAddress tileIndex mode.windowLine bus
                          Background -> do
                            scy <- readSCY bus
                            readBgTileRowBaseAddress tileIndex (scy + ppu.y) bus
                return ppu{mode=mode{fetcherStep=GetTileDataHigh (addr + 1) low}}
              GetTileDataHigh addr low -> do
                high <- readVRam addr bus
                let tileRow = (low, high)
                if Dq.null mode.background then do
                  let mode' = pushBgTileRow tileRow mode
                  return ppu{mode=mode'{fetcherStep=initFIFOPixelFetcher}}
                else
                  return ppu{mode=mode{fetcherStep=Push tileRow}}
              Push tileRow ->
                if Dq.null mode.background then do
                  let mode' = pushBgTileRow tileRow mode
                  return ppu{mode=mode'{fetcherStep=initFIFOPixelFetcher}}
                else
                  return ppu
        executeFetcherStep ppu _ _ = return ppu

        pushBgTileRow tileRow mode@(DrawingPixels {}) =
          let bg = foldl' (\acc colorIndex -> Dq.snoc (FIFOPixel colorIndex 0 0) acc)
                           mode.background (tileRowColorIndexes tileRow)
          in
            mode{background=bg, fetcherX=mode.fetcherX + 8}
        pushBgTileRow _ mode = mode
    HorizontalBlank ->
      return ppu

    VerticalBlank ->
      return ppu{windowYTriggered=False, windowLine=0}

activeWindow :: Int -> PPU -> IO PPU
activeWindow wx ppu =
  case ppu.mode of
    mode@(DrawingPixels {}) -> do
      scx <- readSCX ppu.bus
      let fineScroll = fromIntegral (scx `mod` 8) :: Int
          windowScreenX
            | wx == 0 = case fineScroll of
                0 -> -7
                7 -> -14
                _ -> -(8 + fineScroll)
            | wx < 7 = wx - 7
            | otherwise = mode.screenX
      return $ ppu{ windowLine = ppu.windowLine+1
        , mode = mode
                  {fetcherX=0
                  , fetcherStep=initFIFOPixelFetcher
                  , windowXTriggered=True
                  , windowLine=ppu.windowLine
                  , background=mempty
                  , screenX=windowScreenX
                  , fetcherSource=Window
                  }}
    _ -> return ppu

initScreenX :: Bus -> IO Int
initScreenX bus = do
  scx <- readSCX bus
  return $ -(fromIntegral $ scx `mod` 8)

updateYTriggered :: PPU -> IO PPU
updateYTriggered ppu =
  if ppu.x == 0 then do
    wy <- readWY ppu.bus
    return $ if ppu.y == wy then ppu{windowYTriggered=True} else ppu
  else
    return ppu

advanceXY :: PPU -> PPU
advanceXY ppu
  | ppu.x == 455 =
    let y = if ppu.y == 153 then 0 else ppu.y + 1
        mode = if y < 144 then OAMScan Nothing else VerticalBlank
    in ppu{x=0, y=y, mode=mode}
  | otherwise = ppu{x=ppu.x+1}

tileIndexAddr :: Address -> Word8 -> Word8 -> Address
tileIndexAddr base y x =
  let y16 = fromIntegral y
      x16 = fromIntegral x
  in base + (y16 `div` 8 * 32 + x16 `div` 8)

readBgTileIndexAddr :: Word8 -> Word8 -> Bus -> IO Address
readBgTileIndexAddr ly x bus = do
  base <- readLcdCBgTileMapArea bus
  scx <- readSCX bus
  scy <- readSCY bus
  let bgY = ly + scy
  let bgX = x + scx
  return $ tileIndexAddr base bgY bgX

readWindowTileIndexAddr :: Word8 -> Word8 -> Bus -> IO Address
readWindowTileIndexAddr windowLine x bus = do
  base <- readLcdCWindowTileMapArea bus
  return $ tileIndexAddr base windowLine x

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
      return ppu{y=0, x=0, lcdOn=False, windowYTriggered=False, windowLine=0}
    (False, False) -> return ppu
    (True, True) -> do
      ppu1 <- step ppu
      let ppu2 = advanceXY ppu1
      syncPPUToBus ppu ppu2
      execute (duration - 1) ppu2
