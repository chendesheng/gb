{-# OPTIONS_GHC -Wno-name-shadowing #-}
{-# LANGUAGE BangPatterns #-}
{-# OPTIONS_GHC -Wno-incomplete-record-updates #-}
module PPU (execute, FIFOPixel(..), PPU(..), Display(..), initPPU, isVBlankMode) where

import Control.Monad (when)
import Bus
import Color
import Data.Vector (Vector, (//), (!))
import qualified Data.Vector as Vector
import Data.Vector.Algorithms.Intro (sort)
import Data.Word
import qualified Deque.Lazy as Dq
import Deque.Lazy (Deque)
import Data.List.Split (chunksOf)
import Data.Bits (testBit, (.&.))
import Data.Foldable (toList)
import GHC.Exts (fromList)

-- One dot = one PPU clock. One scanline = 456 dots. One frame = 154 lines = 70224 dots.

-- 160*144
newtype Display = Display (Vector (Vector Color))

initDisplay :: Display
initDisplay = Display (Vector.replicate 144 (Vector.replicate 160 Blank))

renderPixel :: Word8 -> Int -> Color -> Display -> Display
renderPixel y x !color (Display rows) =
  let y' = fromIntegral y
      row = rows ! y'
      x' = fromIntegral x
      !row' = row // [(x', color)]
      !res = Display $ rows // [(y', row')]
  in res

data FIFOPixel = FIFOPixel
  { color :: ColorIndex -- 0 - 3
  , palette :: Bool -- False = OBP0, True = OBP1
  -- , spritePriority :: Int -- not used for DMG
  , behindBg :: Bool
  }

zeroPixel :: FIFOPixel
zeroPixel = FIFOPixel ID0 False False
-- data PixelFIFO = PixelFIFO {oamQueue :: [FIFOPixel], backgroundQueue :: [FIFOPixel]}

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
  , display :: !Display
  , windowLine :: Word8
  , windowYTriggered :: Bool -- the "Y condition"
  , bus :: Bus
  }

data SelectedOAMObject = SelectedOAMObject
  { oamIndex :: Word8
  , position :: OAMObjectPosition
  }

instance Eq SelectedOAMObject where
  (==) a b = a.oamIndex == b.oamIndex

instance Ord SelectedOAMObject where
  compare a b =
    case compare a.position.xPos b.position.xPos of
      EQ -> compare a.oamIndex b.oamIndex
      others -> others

initPPU :: Bus -> PPU
initPPU = PPU 0 0 HorizontalBlank False initDisplay 0 False

-- newtype Tile = Tile (Vector Word8) -- length 16

data PPUMode
  = HorizontalBlank
  | VerticalBlank
  | OAMScan !(Vector SelectedOAMObject) (Maybe SelectedOAMObject)
  | DrawingPixels
      { fetcher :: Fetcher
      , backgroundFetcherX :: Word8
      , screenX :: Int
      , oamQueue :: Deque FIFOPixel
      , backgroundQueue :: Deque FIFOPixel
      , windowXTriggered :: Bool
      , windowLine :: Word8
      , backgroundFetcherSource :: BackgroundFetcherSource
      , selectedOAMObjects :: !(Vector SelectedOAMObject) -- up to 10, reversed
      }
data Fetcher
  = FetchBackground Bool BackgroundFetcher
  | FetchObject BackgroundFetcher ObjectFetcher Int

initFetcher :: Fetcher
initFetcher = FetchBackground False GetTileIndexAddress

instance Eq PPUMode where
  HorizontalBlank == HorizontalBlank = True
  VerticalBlank == VerticalBlank = True
  OAMScan _ a == OAMScan _ b = a == b
  DrawingPixels {} == DrawingPixels {} = True
  _ == _ = False

data BackgroundFetcherSource = Background | Window deriving (Eq, Show)

data BackgroundFetcher
  = GetTileIndexAddress
  | GetTileIndex Address
  | GetTileDataLowAddress BackgroundFetcherSource TileIndex
  | GetTileDataLow Address BackgroundFetcherSource TileIndex
  | GetTileDataHighAddress BackgroundFetcherSource TileIndex Word8
  | GetTileDataHigh Address Word8
  | Push TileRow

data ObjectFetcher
  = GetObjectAttrAddress SelectedOAMObject
  | GetObjectAttr SelectedOAMObject Address
  | GetObjectDataLowAddress SelectedOAMObject OAMObjectAttributes
  | GetObjectDataLow SelectedOAMObject OAMObjectAttributes Address
  | GetObjectDataHighAddress SelectedOAMObject OAMObjectAttributes Word8
  | GetObjectDataHigh OAMObjectAttributes Address Word8

isYFlip :: OAMObjectAttributes -> Bool
isYFlip = (`testBit` 6) . attributes

isXFlip :: OAMObjectAttributes -> Bool
isXFlip = (`testBit` 5) . attributes

isBehindBg :: OAMObjectAttributes -> Bool
isBehindBg = (`testBit` 7) . attributes

dmgPalette :: OAMObjectAttributes -> Bool
dmgPalette = (`testBit` 4) . attributes

readDMGPalette :: OAMObjectAttributes -> Bus -> IO ColorPalette
readDMGPalette attr bus = do
  if dmgPalette attr then
    readOBP1Palette bus
  else
    readOBP0Palette bus

initOAMScan :: PPUMode
initOAMScan = OAMScan mempty Nothing

initDrawingPixels :: Int -> Vector SelectedOAMObject -> PPUMode
initDrawingPixels screenX = DrawingPixels initFetcher 0 screenX mempty mempty False 0 Background

toIntMode :: PPUMode -> Word8
toIntMode HorizontalBlank = 0
toIntMode VerticalBlank = 1
toIntMode (OAMScan _ _) = 2
toIntMode (DrawingPixels {}) = 3

step :: PPU -> IO PPU
step ppu = do
  let bus = ppu.bus
  case ppu.mode of
    OAMScan selectedOAMObjects Nothing -> do
      -- even dot
      -- read entry
      let i = fromIntegral ppu.x `div` 2
      pos <- readOAMPosition i ppu.bus
      let objs = if i == 0 then mempty else selectedOAMObjects
      let mode = OAMScan objs (Just $ SelectedOAMObject i pos)
      ppu' <- updateYTriggered ppu
      return $ ppu'{mode=mode}
    OAMScan selectedOAMObjects (Just pending) -> do
      -- `LY` falls within `[Y - 16, Y - 16 + height)`
      height <- readLcdCObjSize bus
      let ly = fromIntegral ppu.y :: Int
      let y = pending.position.yPos
      let objs = Vector.modify sort $
                    if y - 16 <= ly && ly < y - 16 + height && length selectedOAMObjects < 10 then
                        Vector.snoc selectedOAMObjects pending
                    else
                        selectedOAMObjects
      if ppu.x == 79 then do
          screenX <- initScreenX bus
          let mode = initDrawingPixels screenX objs
          wx <- readWXInt bus
          bgEnable <- isLcdCBgEnable bus
          windowEnable <- isLcdCWindowEnable bus
          if wx == 0 && bgEnable && windowEnable && ppu.windowYTriggered then
            activeWindow wx ppu{mode=mode}
          else return $ ppu{mode=mode}
      else return $ ppu{mode=OAMScan objs Nothing}
    DrawingPixels {} -> do
      bgEnable <- isLcdCBgEnable bus
      windowEnable <- isLcdCWindowEnable bus
      objEnable <- isLcdCObjEnable bus
      ppu1 <- if bgEnable && windowEnable then tryActiveWindow ppu ppu.mode else return ppu
      let windowActive = bgEnable && windowEnable && ppu1.windowYTriggered &&
                            case ppu1.mode of
                              { DrawingPixels { windowXTriggered = w } -> w; _ -> False }
      let ppu2 = dispatchFetcher ppu1 ppu1.mode objEnable
      case ppu2.mode of
        DrawingPixels {fetcher=FetchBackground disableRender fetcher} -> do
          ppu3 <- if disableRender then return ppu2 else render ppu2 ppu2.mode objEnable bgEnable
          if ppu3.mode.screenX == 160 then
            return ppu3{mode=HorizontalBlank}
          else
            executeBackgroundFetcher ppu3 ppu3.mode fetcher windowActive
        DrawingPixels {fetcher=FetchObject bgFetcher fetcher@(GetObjectAttrAddress _) clipCount} -> do
          executeBothFetcher ppu2 bgFetcher fetcher windowActive clipCount
        DrawingPixels {fetcher=FetchObject bgFetcher fetcher@(GetObjectAttr _ _) clipCount} -> do
          executeBothFetcher ppu2 bgFetcher fetcher windowActive clipCount
        DrawingPixels {fetcher=FetchObject bgFetcher fetcher clipCount} -> do
          executeObjectFetcher ppu2 ppu2.mode bgFetcher fetcher clipCount
        _ ->
          return ppu2
      where
        executeBothFetcher :: PPU -> BackgroundFetcher -> ObjectFetcher -> Bool -> Int -> IO PPU
        executeBothFetcher ppu bgFetcher objFetcher windowActive clipCount = do
          ppu' <- executeBackgroundFetcher ppu ppu.mode bgFetcher windowActive
          let bgFetcher' = case ppu'.mode of
                            DrawingPixels {fetcher=FetchBackground _ f} -> f
                            DrawingPixels {fetcher=FetchObject f _ _} -> f
                            _ -> bgFetcher
          executeObjectFetcher ppu' ppu'.mode bgFetcher' objFetcher clipCount

        dispatchFetcher ppu mode@(DrawingPixels {}) True =
          let objs = dropPassedObjects mode.screenX mode.selectedOAMObjects
          in
          case Vector.uncons objs  of
            Just (obj, rest) | (mode.screenX <= 0 && obj.position.xPos - 8 < mode.screenX)
                                || mode.screenX == obj.position.xPos - 8  ->
              case mode.fetcher of
                FetchBackground _ bgFetcher ->
                  if backgroundReady mode.backgroundQueue bgFetcher then
                    ppu{mode=mode{ fetcher=FetchObject
                                             bgFetcher
                                             (GetObjectAttrAddress obj)
                                             (max 0 $ mode.screenX - obj.position.xPos + 8)
                                 , selectedOAMObjects=rest
                                 }}
                  else
                    ppu{mode=mode{fetcher=FetchBackground True bgFetcher}}
                FetchObject {} -> ppu
            _ -> ppu{mode=mode{selectedOAMObjects=objs}}
          where
            dropPassedObjects :: Int -> Vector SelectedOAMObject -> Vector SelectedOAMObject
            dropPassedObjects screenX = Vector.dropWhile (\obj -> screenX > 0 && obj.position.xPos - 8 < screenX)

            backgroundReady backgroundQueue fetcher =
              not (Dq.null backgroundQueue) &&
                case fetcher of
                  (GetTileDataHigh _ _) -> True
                  (Push _) -> True
                  _ -> False
        dispatchFetcher ppu _ _ = ppu

        tryActiveWindow ppu mode@(DrawingPixels{}) = do
          if ppu.windowYTriggered && not mode.windowXTriggered then do
            wx <- readWXInt bus
            let windowXTriggered = wx > 0 && (wx < 7 && (mode.screenX == 0) || wx - 7 == mode.screenX)
            if windowXTriggered then activeWindow wx ppu
            else return ppu
          else return ppu
        tryActiveWindow ppu _ = return ppu

        render ppu mode@(DrawingPixels{screenX=screenX}) objEnable bgEnable =
          if mode.screenX < 0 then do
            case (Dq.uncons mode.backgroundQueue, Dq.uncons mode.oamQueue) of
              (Just (_, backgroundQueue), Just (_, oamQueue)) ->
                return ppu{mode=mode{screenX=screenX + 1, backgroundQueue=backgroundQueue, oamQueue=oamQueue}}
              (Just (_, backgroundQueue), _) ->
                return ppu{mode=mode{screenX=screenX + 1, backgroundQueue=backgroundQueue}}
              (_, Just (_, oamQueue)) ->
                return ppu{mode=mode{screenX=screenX + 1, oamQueue=oamQueue}}
              _ -> return ppu
          else
            case (Dq.uncons mode.backgroundQueue, Dq.uncons mode.oamQueue) of
              (Just (bgPixel, backgroundQueue), Just (objPixel, oamQueue)) -> do
                let bgPixel' = if bgEnable then bgPixel else zeroPixel
                let objPixel' = if objEnable then objPixel else zeroPixel
                color <- pixelColor bgPixel' objPixel'
                return ppu{ mode=mode{screenX=screenX+1, backgroundQueue=backgroundQueue, oamQueue=oamQueue}
                          , display=renderPixel ppu.y screenX color ppu.display
                          }
              (Just (pixel, backgroundQueue), Nothing) -> do
                let pixel' = if bgEnable then pixel else zeroPixel
                color <- bgPixelColor pixel'
                return ppu{ mode=mode{screenX=screenX+1, backgroundQueue=backgroundQueue}
                          , display=renderPixel ppu.y screenX color ppu.display
                          }
              _ -> return ppu
        render ppu _ _ _ = return ppu

        pixelColor :: FIFOPixel -> FIFOPixel -> IO Color
        pixelColor bgPixel objPixel = do
          if objPixel.color == ID0 || (objPixel.behindBg && bgPixel.color /= ID0) then
            bgPixelColor bgPixel
          else objectPixelColor objPixel

        bgPixelColor :: FIFOPixel -> IO Color
        bgPixelColor pixel = do
          palette <- readBGPalette bus
          return $ getColor pixel.color palette

        objectPixelColor :: FIFOPixel -> IO Color
        objectPixelColor pixel = do
          palette <- readObjPalette pixel.palette bus
          return $ getColor pixel.color palette

        executeBackgroundFetcher :: PPU -> PPUMode -> BackgroundFetcher -> Bool -> IO PPU
        executeBackgroundFetcher ppu mode@(DrawingPixels {}) fetcher windowActive =
          case fetcher of
            GetTileIndexAddress -> do
              let source = if windowActive then Window else Background
              mode' <- if mode.backgroundFetcherSource == Window && source == Background then do
                            -- switch from window to backgroundQueue
                            return $ mode{backgroundFetcherX=fromIntegral (mode.screenX + length mode.backgroundQueue)
                                         , windowXTriggered=False
                                         }
                       else return mode
              addr <- case source of
                        Window -> readWindowTileIndexAddr mode'.windowLine mode'.backgroundFetcherX ppu.bus
                        Background -> readBgTileIndexAddr ppu.y mode'.backgroundFetcherX bus
              return ppu{mode=mode'{fetcher=FetchBackground False (GetTileIndex addr), backgroundFetcherSource=source}}
            GetTileIndex addr -> do
              tileIndex <- readVRam addr bus
              return ppu{mode=mode{fetcher=FetchBackground False (GetTileDataLowAddress mode.backgroundFetcherSource tileIndex)}}
            GetTileDataLowAddress source tileIndex -> do
              addr <- readBgTileRowBaseAddressForSource source tileIndex mode.windowLine ppu.y bus
              return ppu{mode=mode{fetcher=FetchBackground False (GetTileDataLow addr source tileIndex)}}
            GetTileDataLow addr source tileIndex -> do
              low <- readVRam addr bus
              return ppu{mode=mode{fetcher=FetchBackground False (GetTileDataHighAddress source tileIndex low)}}
            GetTileDataHighAddress source tileIndex low -> do
              addr <- readBgTileRowBaseAddressForSource source tileIndex mode.windowLine ppu.y bus
              return ppu{mode=mode{fetcher=FetchBackground False (GetTileDataHigh (addr + 1) low)}}
            GetTileDataHigh addr low -> do
              high <- readVRam addr bus
              let tileRow = (low, high)
              if Dq.null mode.backgroundQueue then do
                let mode' = pushBgTileRow tileRow mode
                return ppu{mode=mode'{fetcher=initFetcher}}
              else
                return ppu{mode=mode{fetcher=FetchBackground False (Push tileRow)}}
            Push tileRow ->
              if Dq.null mode.backgroundQueue then do
                let mode' = pushBgTileRow tileRow mode
                return ppu{mode=mode'{fetcher=initFetcher}}
              else
                return ppu
        executeBackgroundFetcher ppu _ _ _ = return ppu

        executeObjectFetcher :: PPU -> PPUMode -> BackgroundFetcher -> ObjectFetcher -> Int -> IO PPU
        executeObjectFetcher ppu mode@(DrawingPixels {}) bgFetcher fetcher clipCount =
          case fetcher of
            GetObjectAttrAddress selected -> do
              let addr = fromIntegral selected.oamIndex * 4 + 2
              return ppu{mode=mode{fetcher=FetchObject bgFetcher (GetObjectAttr selected addr) clipCount}}
            GetObjectAttr selected addr -> do
              attr <- readOAMAttributes addr bus
              return ppu{mode=mode{fetcher=FetchObject bgFetcher (GetObjectDataLowAddress selected attr) clipCount}}
            GetObjectDataLowAddress selected attr -> do
              addr <- objectDataAddr selected attr
              return ppu{mode=mode{fetcher=FetchObject bgFetcher (GetObjectDataLow selected attr addr) clipCount}}
            GetObjectDataLow selected attr addr -> do
              low <- readVRam addr bus
              return ppu{mode=mode{fetcher=FetchObject bgFetcher (GetObjectDataHighAddress selected attr low) clipCount}}
            GetObjectDataHighAddress selected attr low -> do
              addr <- objectDataAddr selected attr
              return ppu{mode=mode{fetcher=FetchObject bgFetcher (GetObjectDataHigh attr (addr + 1) low) clipCount}}
            GetObjectDataHigh attr addr low -> do
              high <- readVRam addr bus
              let tileRow = (low, high)
              let mode' = pushObjectTileRow attr tileRow mode clipCount
              return ppu{mode=mode'{fetcher=FetchBackground False bgFetcher}}
        executeObjectFetcher ppu _ _ _ _ = return ppu

        objectDataAddr :: SelectedOAMObject -> OAMObjectAttributes -> IO Address
        objectDataAddr selected attr = do
          height <- readLcdCObjSize bus
          let row = yFlipRow attr height $ fromIntegral ppu.y - selected.position.yPos + 16
          let tileIndex = if height == 16 then attr.tileIndex .&. 0xFE else attr.tileIndex
          return $ fromIntegral $ 0x8000 + fromIntegral tileIndex * 16 + row * 2
          where
            yFlipRow attr height row =
              if isYFlip attr then
                height - 1 - row
              else
                 row

        pushObjectTileRow attr tileRow mode@(DrawingPixels {}) clipCount =
          let palette = dmgPalette attr
              indexes = if isXFlip attr then reverse else id
              behindBg = isBehindBg attr
              newQueue = foldl' (\acc colorIndex -> Dq.snoc (FIFOPixel colorIndex palette behindBg) acc)
                             mempty (drop clipCount $ indexes $ tileRowColorIndexes tileRow)
              oldQueue = foldl' (\acc _ -> Dq.snoc zeroPixel acc) mode.oamQueue [1..8-length mode.oamQueue]

          in
          mode{oamQueue=fromList $ zipWith mergePixel (toList oldQueue) (toList newQueue)}
        pushObjectTileRow _ _ mode _ = mode

        mergePixel :: FIFOPixel -> FIFOPixel -> FIFOPixel
        mergePixel old new
          | new.color == ID0 = old
          | old.color == ID0 = new
          | otherwise     = old

        pushBgTileRow tileRow mode@(DrawingPixels {}) =
          let bg = foldl' (\acc colorIndex -> Dq.snoc (FIFOPixel colorIndex False False) acc)
                           mode.backgroundQueue (tileRowColorIndexes tileRow)
          in
            mode{backgroundQueue=bg, backgroundFetcherX=mode.backgroundFetcherX + 8}
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
                  {backgroundFetcherX=0
                  , fetcher=initFetcher
                  , windowXTriggered=True
                  , windowLine=ppu.windowLine
                  , backgroundQueue=mempty
                  , screenX=windowScreenX
                  , backgroundFetcherSource=Window
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
        mode = if y < 144 then OAMScan mempty Nothing else VerticalBlank
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

readBgTileRowBaseAddressForSource :: BackgroundFetcherSource -> TileIndex -> Word8 -> Word8 -> Bus -> IO Address
readBgTileRowBaseAddressForSource source tileIndex windowLine y bus =
  case source of
    Window -> readBgTileRowBaseAddress tileIndex windowLine bus
    Background -> do
      scy <- readSCY bus
      readBgTileRowBaseAddress tileIndex (scy + y) bus

syncPPUToBus :: PPU -> PPU -> IO ()
syncPPUToBus oldPPU ppu = do
  -- Synchronizing LY/mode also updates the shared STAT interrupt line.
  syncPPU ppu.y (toIntMode ppu.mode) ppu.bus
  -- use if instead
  when (oldPPU.mode /= ppu.mode) $
    case ppu.mode of
      VerticalBlank -> writeIF VBlank True ppu.bus
      _ -> return ()

isVBlankMode :: PPU -> Bool
isVBlankMode PPU{mode=VerticalBlank} = True
isVBlankMode _ = False

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
