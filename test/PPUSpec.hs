{-# LANGUAGE BangPatterns #-}

module PPUSpec (spec) where

import Bus (Bus (..), readByte, writeByte)
import CPU (CPU (..))
import qualified CPU
import Codec.Picture
  ( Image, PixelRGB8 (..), convertRGB8, generateImage, imageHeight,
    imageWidth, pixelAt, readImage, writePng )
import Color (Color (..))
import Control.Exception (ErrorCall, catch)
import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (isNothing)
import qualified Data.Vector as Vector
import qualified Data.Vector.Unboxed.Mutable as Mutable
import Data.Word (Word8)
import Numeric (showHex)
import qualified PPU
import Registers (Registers (..))
import System.Directory (createDirectoryIfMissing)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "PPU" $
  it "renders the dmg-acid2 DMG reference image" $ do
    rom <- BL.readFile "test/fixtures/dmg-acid2/dmg-acid2.gb"
    decoded <- readImage "test/fixtures/dmg-acid2/reference-dmg.png"
    reference <- either (ioError . userError) (pure . convertRGB8) decoded
    (imageWidth reference, imageHeight reference) `shouldBe` (160, 144)
    cpu <- initPostBootCPU rom
    result <- timeout (30 * 1000000) $ runAcid2 cpu
    case result of
      Nothing -> expectationFailure "dmg-acid2 exceeded the 30-second timeout"
      Just ppu -> compareFrame reference ppu

-- Start at the cartridge entry point in a deterministic DMG post-boot state.
-- Boot-ROM execution is already covered by CPUSpec.
initPostBootCPU :: BL.ByteString -> IO CPU.CPU
initPostBootCPU rom = do
  cpu <- CPU.initCPU BL.empty rom
  Mutable.set cpu.bus.vram 0
  Mutable.set cpu.bus.oam 0
  mapM_ (\(address, value) -> writeByte address value cpu.bus)
    [ (0xFF50, 1), (0xFF40, 0x91), (0xFF47, 0xFC)
    , (0xFF48, 0xFF), (0xFF49, 0xFF)
    ]
  pure cpu{registers = Registers
    { rA = 0x01, rF = 0xB0, rB = 0x00, rC = 0x13
    , rD = 0x00, rE = 0xD8, rH = 0x01, rL = 0x4D
    , rSP = 0xFFFE, rPC = 0x0100
    }}

runAcid2 :: CPU.CPU -> IO PPU.PPU
runAcid2 cpu = go (20 * 70224) 2000000 cpu (PPU.initPPU cpu.bus)
  where
    go :: Int -> Int -> CPU.CPU -> PPU.PPU -> IO PPU.PPU
    go !remaining !steps !cpu !ppu = do
      -- Upstream emits LD B,B after the tenth rendered frame, when B reaches 0.
      -- Stop before executing that marker, with the complete frame in Display.
      opcode <- readByte cpu.registers.rPC cpu.bus
      if isNothing cpu.currentInstruction && cpu.registers.rB == 0 && opcode == 0x40
        then pure ppu
        else if remaining <= 0 || steps <= 0
          then do
            saveActualFrame ppu
            ioError $ userError $ "dmg-acid2 did not reach its ten-frame marker within "
              ++ "20 frames / 2,000,000 CPU steps; PC=0x"
              ++ showHex cpu.registers.rPC "" ++ "; actual frame: " ++ actualPath
          else do
            (cpu', cycles) <- CPU.execute cpu `catch` executionFailure cpu ppu
            ppu' <- PPU.execute cycles ppu
            go (remaining - fromIntegral cycles) (steps - 1) cpu' ppu'

    executionFailure :: CPU.CPU -> PPU.PPU -> ErrorCall -> IO (CPU.CPU, Word8)
    executionFailure cpu ppu err = do
      saveActualFrame ppu
      ioError $ userError $ "dmg-acid2 CPU failure near PC=0x"
        ++ showHex cpu.registers.rPC "" ++ ": " ++ show err
        ++ "; actual frame: " ++ actualPath

compareFrame :: Image PixelRGB8 -> PPU.PPU -> Expectation
compareFrame reference ppu = do
  let actual = frameImage ppu
      differences =
        [ (x, y, pixelAt reference x y, pixelAt actual x y)
        | y <- [0 .. 143], x <- [0 .. 159]
        , pixelAt reference x y /= pixelAt actual x y
        ]
  unless (null differences) $ do
    saveActualFrame ppu
    expectationFailure $ "dmg-acid2: " ++ show (length differences)
      ++ " pixels differ; first differences (x, y, expected, actual): "
      ++ show (take 8 differences) ++ "; actual frame: " ++ actualPath

actualPath :: FilePath
actualPath = "dist-newstyle/ppu-test/dmg-acid2-actual.png"

saveActualFrame :: PPU.PPU -> IO ()
saveActualFrame ppu = do
  createDirectoryIfMissing True "dist-newstyle/ppu-test"
  writePng actualPath (frameImage ppu)

frameImage :: PPU.PPU -> Image PixelRGB8
frameImage ppu =
  let PPU.Display rows = ppu.display
  in generateImage (\x y -> toRGB (rows Vector.! y Vector.! x)) 160 144
  where
    toRGB color = let value = grayscale color in PixelRGB8 value value value
    grayscale Blank = 0xFF
    grayscale LightGray = 0xAA
    grayscale DarkGray = 0x55
    grayscale Black = 0x00
