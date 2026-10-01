module CPUSpec (spec) where

import Bus (Bus (..), bootRomEnabled)
import CPU (CPU (..), execute1, initCPU)
import qualified PPU
import PPU (PPU, initPPU)
import Data.ByteString.Lazy as BL
import Data.Word
import Registers
import Test.Hspec

{- ORMOLU_DISABLE -}
testCPU :: IO CPU
testCPU = do
  bootRom <- BL.readFile "./test/fixtures/dmg.bin"
  initCPU bootRom dummyCartridge
  where
    zeros :: Int -> [Word8]
    zeros n = Prelude.replicate n 0

    dummyCartridge :: BL.ByteString
    dummyCartridge =
      BL.pack $
        zeros 0x0104
          ++ [ 0xCE, 0xED, 0x66, 0x66, 0xCC, 0x0D, 0x00, 0x0B
             , 0x03, 0x73, 0x00, 0x83, 0x00, 0x0C, 0x00, 0x0D
             , 0x00, 0x08, 0x11, 0x1F, 0x88, 0x89, 0x00, 0x0E
             , 0xDC, 0xCC, 0x6E, 0xE6, 0xDD, 0xDD, 0xD9, 0x99
             , 0xBB, 0xBB, 0x67, 0x63, 0x6E, 0x0E, 0xEC, 0xCC
             , 0xDD, 0xDC, 0x99, 0x9F, 0xBB, 0xB9, 0x33, 0x3E
             ]
          ++ zeros (0x014D - 0x0134)
          ++ [0x00E7]
          ++ zeros (0x8000 - 0x014E)
{- ORMOLU_ENABLE -}

isBooted :: CPU -> IO Bool
isBooted cpu = not <$> bootRomEnabled cpu.bus

execute :: (CPU -> IO Bool) -> CPU -> PPU -> IO (CPU, PPU)
execute endPred cpu ppu = do
  end <- endPred cpu
  if end
    then return (cpu, ppu)
    else do
      (cpu', cycles) <- execute1 cpu
      ppu' <- PPU.execute cycles ppu
      execute endPred cpu' ppu'

spec :: SpecWith ()
spec = describe "CPU" $ do
  it "execute first instruction" $ do
    cpu <- testCPU
    (cpu1, _) <- execute1 cpu
    cpu1.registers.rSP `shouldBe` 0xFFFE
  it "execute boot rom" $ do
    cpu <- testCPU
    let ppu = initPPU cpu.bus
    (cpu1, ppu1) <- execute isBooted cpu ppu
    print ppu1.display
    cpu1.registers
      `shouldBe` Registers
        { rA = 0x01,
          rF = 0xB0,
          rB = 0x00,
          rC = 0x13,
          rD = 0x00,
          rE = 0xD8,
          rH = 0x01,
          rL = 0x4D,
          rSP = 0xFFFE,
          rPC = 0x0100
        }
