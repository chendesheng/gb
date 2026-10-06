{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}

module Main (main) where

import Control.Exception (SomeException, bracket, displayException, mask_, try)
import Control.Monad (unless, when)
import qualified Color as GB
import qualified Data.ByteString.Lazy as BL
import Data.Char (toLower)
import Data.IORef (IORef, atomicModifyIORef', newIORef, writeIORef)
import Data.Maybe (isJust)
import qualified Data.Vector as V
import Emulator (Emulator, EmulatorWorker, powerOn, runInBackground, stopWorker, nextWorkerError, nextDisplay)
import Foreign.Marshal.Array (withArray)
import Foreign.Ptr (castPtr)
import qualified NativeMenu as Menu
import Paths_gb (getDataFileName)
import PPU (Display (..))
import Raylib.Core
  ( clearBackground, closeWindow, getMouseX, getMouseY, getWindowPosition,
    initWindow, isMouseButtonDown, isMouseButtonPressed, isWindowReady,
    setConfigFlags, setMousePosition, setTargetFPS, setWindowPosition,
    setWindowSize, windowShouldClose )
import Raylib.Core.Shapes (drawRectangle)
import Raylib.Core.Textures
  ( drawTexturePro, genImageColor, genTextureMipmaps, imageAlphaCrop,
    imageResize, imageResizeCanvas, isTextureValid,
    loadImage, loadTexture, loadTextureFromImage, setTextureFilter, updateTexture )
import Raylib.Types
  ( Color (..), ConfigFlag (WindowHighdpi, WindowTransparent, WindowUndecorated),
    MouseButton (MouseButtonLeft), Rectangle (..), Texture (..),
    TextureFilter (TextureFilterBilinear, TextureFilterPoint, TextureFilterTrilinear),
    pattern Vector2, vector2'x, vector2'y )
import Raylib.Util (drawing, managed)
import Raylib.Util.Colors (black, blank, white)
import System.Directory (doesFileExist)
import System.Environment (getExecutablePath)
import System.FilePath ((</>), takeDirectory, takeExtension)
import Data.Foldable (for_)

data UIState = UIState
  { cartridge :: Maybe BL.ByteString
  , emulator :: Maybe Emulator
  , paused :: Bool
  , dragAnchor :: Maybe (Int, Int)
  }

data Assets = Assets
  { device :: Texture
  , batteryOff :: Texture
  , batteryOn :: Texture
  , lcd :: Texture
  , boot :: BL.ByteString
  , worker :: IORef (Maybe EmulatorWorker)
  }

main :: IO ()
main = do
  imagePath <- resourcePath "device.png"
  setConfigFlags [WindowUndecorated, WindowHighdpi, WindowTransparent]
  bracket (initWindow 836 471 "Game Boy") (closeWindow . Just) $ \window -> do
    ready <- isWindowReady
    unless ready $ ioError (userError "Could not open the Game Boy window")
    device <- managed window (loadTexture imagePath)
    let loadLED name = do
          path <- resourcePath name
          -- Resample before the large reduction; four bilinear samples from the
          -- full-size sprite otherwise alias the detailed rim. Transparent
          -- padding keeps the antialiased edge away from the texture boundary.
          image <- loadImage path >>= (`imageAlphaCrop` 0.01)
            >>= (\cropped -> imageResize cropped 72 72)
            >>= (\small -> imageResizeCanvas small 80 80 4 4 blank)
          managed window (loadTextureFromImage image >>= genTextureMipmaps
            >>= (`setTextureFilter` TextureFilterTrilinear))
    batteryOff <- loadLED "battery-off.png"
    batteryOn <- loadLED "battery-on.png"
    mapM_ (\texture -> do
      valid <- isTextureValid texture
      unless valid $ ioError (userError "Could not load device textures")) [device, batteryOff, batteryOn]
    _ <- setTextureFilter device TextureFilterBilinear
    lcdImage <- genImageColor 160 144 white
    lcd <- managed window (loadTextureFromImage lcdImage)
    _ <- setTextureFilter lcd TextureFilterPoint
    boot <- resourcePath "dmg.bin" >>= BL.readFile
    let width = (texture'width device + 1) `div` 2
        height = (texture'height device + 1) `div` 2
    setWindowSize width height
    setTargetFPS 60
    Menu.installMenu
    bracket (newIORef Nothing) stopCurrentWorker $ \worker ->
      loop (Assets device batteryOff batteryOn lcd boot worker) width height
        (UIState Nothing Nothing False Nothing)

-- The app bundle carries its own resources; Cabal supplies paths for cabal run.
resourcePath :: FilePath -> IO FilePath
resourcePath name = do
  executable <- getExecutablePath
  let bundled = takeDirectory executable </> ".." </> "Resources" </> name
  exists <- doesFileExist bundled
  if exists then pure bundled else getDataFileName ("resources" </> name)

loop :: Assets -> Int -> Int -> UIState -> IO ()
loop assets width height state = do
  close <- windowShouldClose
  unless close $ do
    action <- Menu.pollMenuAction
    state1 <- handleAction assets state action
    -- The printed OFF/ON switch also toggles power without dragging the window.
    pressed <- isMouseButtonPressed MouseButtonLeft
    mouseX <- getMouseX
    mouseY <- getMouseY
    let switchClicked = pressed && mouseX >= 60 && mouseX <= 164 && mouseY >= 4 && mouseY <= 38
    state2 <- if switchClicked
      then handleAction assets state1 (Just $ if isJust state1.emulator then Menu.PowerOff else Menu.PowerOn)
      else pure state1
    anchor <- if switchClicked then pure Nothing else dragWindow state2.dragAnchor
    state3 <- checkWorkerError assets state2
    Menu.setPowerState (isJust state3.cartridge) (isJust state3.emulator)
    case state3.emulator of
      Just machine -> do
        maybeDisplay <- nextDisplay machine
        for_ maybeDisplay (updateLCD assets.lcd)
      Nothing -> pure ()
    drawing $ do
      clearBackground blank
      drawRectangle 287 100 263 238 black
      when (isJust state3.emulator) $
        drawTexture assets.lcd (Rectangle 287 100 263 238)
      drawTexture assets.device (Rectangle 0 0 (fromIntegral width) (fromIntegral height))
      -- Both generated sprites cover the original red LED and share this location.
      let indicator = if isJust state3.emulator then assets.batteryOn else assets.batteryOff
      drawTexture indicator (Rectangle 232 177 20 20)
    loop assets width height state3{dragAnchor=anchor}

handleAction :: Assets -> UIState -> Maybe Menu.MenuAction -> IO UIState
handleAction assets state action = case action of
  Nothing -> pure state
  Just Menu.PowerOff -> do
    stopCurrentWorker assets.worker
    pure state{emulator=Nothing, paused=False}
  Just Menu.PowerOn -> case state.cartridge of
    Nothing -> pure state
    Just rom -> start rom
  Just (Menu.OpenCartridge path) -> do
    result <- try $ do
      unless (map toLower (takeExtension path) `elem` [".bin", ".gb"]) $
        ioError (userError "Please choose a .bin or .gb cartridge file.")
      rom <- BL.readFile path
      unless (BL.length rom >= 0x8000) $
        ioError (userError "The cartridge must contain at least 32 KiB.")
      pure rom
    case result of
      Left err -> Menu.showError (displayException (err :: SomeException)) >> pure state
      Right rom -> start rom
  where
    start rom = do
      result <- try (powerOn assets.boot rom)
      case result of
        Left err -> Menu.showError (displayException (err :: SomeException)) >> pure state
        Right machine -> do
          mask_ $ do
            stopCurrentWorker assets.worker
            worker <- runInBackground machine
            writeIORef assets.worker (Just worker)
          pure state{cartridge=Just rom, emulator=Just machine, paused=False}

stopCurrentWorker :: IORef (Maybe EmulatorWorker) -> IO ()
stopCurrentWorker workerRef = mask_ $ do
  worker <- atomicModifyIORef' workerRef (Nothing,)
  mapM_ stopWorker worker

checkWorkerError :: Assets -> UIState -> IO UIState
checkWorkerError assets state = case state.emulator of
  Just machine | not state.paused -> do
    failure <- nextWorkerError machine
    case failure of
      Nothing -> pure state
      Just err -> do
        stopCurrentWorker assets.worker
        Menu.showError ("Emulation stopped:\n" ++ displayException err)
        pure state{paused=True}
  _ -> pure state

updateLCD :: Texture -> Display -> IO ()
updateLCD texture (Display rows) =
  withArray (map lcdColor $ concatMap V.toList $ V.toList rows) $ \pixels ->
    updateTexture texture (castPtr pixels)
  where
    lcdColor GB.Blank = Color 224 236 200 255
    lcdColor GB.LightGray = Color 160 184 120 255
    lcdColor GB.DarkGray = Color 80 112 72 255
    lcdColor GB.Black = Color 24 48 32 255

drawTexture :: Texture -> Rectangle -> IO ()
drawTexture texture destination = drawTexturePro texture
  (Rectangle 0 0 (fromIntegral $ texture'width texture) (fromIntegral $ texture'height texture))
  destination (Vector2 0 0) 0 white

dragWindow :: Maybe (Int, Int) -> IO (Maybe (Int, Int))
dragWindow anchor = do
  pressed <- isMouseButtonPressed MouseButtonLeft
  held <- isMouseButtonDown MouseButtonLeft
  mouseX <- getMouseX
  mouseY <- getMouseY
  if pressed
    then pure (Just (mouseX, mouseY))
    else if held
      then do
        case anchor of
          Just (anchorX, anchorY) -> do
            let dx = mouseX - anchorX
                dy = mouseY - anchorY
            when (dx /= 0 || dy /= 0) $ do
              position <- getWindowPosition
              setWindowPosition (round (vector2'x position) + dx) (round (vector2'y position) + dy)
              setMousePosition anchorX anchorY
          Nothing -> pure ()
        pure anchor
      else pure Nothing
