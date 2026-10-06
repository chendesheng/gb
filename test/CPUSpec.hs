module CPUSpec (spec) where

import Bus (bootRomEnabled, readByte, writeByte)
import CPU (CPU (..), initCPU)
import qualified PPU
import qualified CPU
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
      (cpu', cycles) <- CPU.execute cpu
      ppu' <- PPU.execute cycles ppu
      execute endPred cpu' ppu'


executeOneCPUInstruction :: CPU -> IO (CPU, Word8)
executeOneCPUInstruction = go 0
  where
      go elapsed cpu = do
          (cpu1, elapsed1) <- CPU.execute cpu
          case cpu1.currentInstruction of
            Nothing -> return (cpu1, elapsed + elapsed1)
            _ -> go (elapsed + elapsed1) cpu1

spec :: SpecWith ()
spec = describe "CPU" $ do
  it "execute first instruction" $ do
    cpu <- testCPU
    (cpu1, _) <- executeOneCPUInstruction cpu
    cpu1.registers.rSP `shouldBe` 0xFFFE
  it "cancels interrupt dispatch when pushing PC high changes IE" $ do
    cpu <- initCPU (BL.pack $ 0xFB : Prelude.replicate 255 0) (BL.replicate 0x8000 0)
    -- Execute EI and NOP so the interrupt enable delay has elapsed.
    (cpu1, _) <- executeOneCPUInstruction cpu
    (cpu2, _) <- executeOneCPUInstruction cpu1
    let start = cpu2 {registers = cpu2.registers {rPC = 0x0200, rSP = 0x0000}}
    writeByte 0xFFFF 0x04 start.bus -- Enable Timer.
    writeByte 0xFF0F 0x04 start.bus -- Request Timer.
    writeByte 0xFFFE 0xAA start.bus
    let runService elapsed state
          | elapsed == 20 = return state
          | otherwise = do
              (state', cycles) <- CPU.execute state
              cycles `shouldSatisfy` (> 0)
              let elapsed' = elapsed + fromIntegral cycles
              elapsed' `shouldSatisfy` (<= 20)
              state'.currentInstruction `shouldBe` Nothing
              runService elapsed' state'
    cpu3 <- runService (0 :: Int) start
    -- The high-byte push wraps SP to IE and writes 0x02, disabling Timer.
    -- Dispatch is cancelled, but both PC bytes must still be pushed.
    readByte 0xFFFF cpu3.bus `shouldReturn` 0x02
    readByte 0xFFFE cpu3.bus `shouldReturn` 0x00
    readByte 0xFF0F cpu3.bus `shouldReturn` 0x04
    cpu3.registers.rPC `shouldBe` 0x0000
    cpu3.registers.rSP `shouldBe` 0xFFFE
    cpu3.ime `shouldBe` cpu.ime
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
