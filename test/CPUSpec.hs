module CPUSpec (spec) where

import Bus (Bus, bootRomEnabled, readByte, writeByte, initBus, increaseTimer)
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
  bus <- initBus bootRom dummyCartridge
  return $ initCPU bus
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
      ppu' <- PPU.execute (cycles * 4) ppu
      execute endPred cpu' ppu'


executeOneCPUInstruction :: CPU -> IO (CPU, Word8)
executeOneCPUInstruction = go 0
  where
      go elapsed cpu = do
          (cpu1, elapsed1) <- CPU.execute cpu
          case cpu1.currentInstruction of
            Nothing -> return (cpu1, elapsed + elapsed1)
            _ -> go (elapsed + elapsed1) cpu1

-- Four M-cycles at TAC=5 overflow TIMA and leave the reload pending.
timerOverflowBus :: IO Bus
timerOverflowBus = do
  bus <- initBus BL.empty BL.empty
  writeByte 0xFF07 5 bus
  writeByte 0xFF05 0xFF bus
  writeByte 0xFF06 0xAB bus
  increaseTimer 4 bus
  return bus

spec :: SpecWith ()
spec = describe "CPU" $ do
  describe "timer overflow" $ do
    it "delays reload by one M-cycle and requests the interrupt only once" $ do
      bus <- timerOverflowBus
      readByte 0xFF05 bus `shouldReturn` 0
      readByte 0xFF0F bus `shouldReturn` 0
      increaseTimer 1 bus
      readByte 0xFF05 bus `shouldReturn` 0xAB
      readByte 0xFF0F bus `shouldReturn` 4
      writeByte 0xFF0F 0 bus
      increaseTimer 1 bus
      readByte 0xFF0F bus `shouldReturn` 0

    it "cancels pending reload and interrupt when TIMA is written during the delay" $ do
      bus <- timerOverflowBus
      writeByte 0xFF05 0x77 bus
      increaseTimer 1 bus
      readByte 0xFF05 bus `shouldReturn` 0x77
      readByte 0xFF0F bus `shouldReturn` 0

    it "ignores TIMA writes throughout reloading and accepts them afterwards" $ do
      bus <- timerOverflowBus
      increaseTimer 1 bus
      writeByte 0xFF05 0x77 bus
      readByte 0xFF05 bus `shouldReturn` 0xAB
      writeByte 0xFF05 0x66 bus
      readByte 0xFF05 bus `shouldReturn` 0xAB
      increaseTimer 1 bus
      writeByte 0xFF05 0x77 bus
      readByte 0xFF05 bus `shouldReturn` 0x77

    it "uses TMA written during the delay without changing TIMA early" $ do
      bus <- timerOverflowBus
      writeByte 0xFF06 0x66 bus
      readByte 0xFF05 bus `shouldReturn` 0
      increaseTimer 1 bus
      readByte 0xFF05 bus `shouldReturn` 0x66
      readByte 0xFF0F bus `shouldReturn` 4

    it "updates both TMA and TIMA when TMA is written during reloading" $ do
      bus <- timerOverflowBus
      increaseTimer 1 bus
      writeByte 0xFF06 0x66 bus
      readByte 0xFF06 bus `shouldReturn` 0x66
      readByte 0xFF05 bus `shouldReturn` 0x66
      writeByte 0xFF05 0x77 bus
      readByte 0xFF05 bus `shouldReturn` 0x66
      increaseTimer 1 bus
      writeByte 0xFF06 0x55 bus
      readByte 0xFF05 bus `shouldReturn` 0x66

    it "handles overflow and reload within a batch of M-cycles" $ do
      bus <- initBus BL.empty BL.empty
      writeByte 0xFF07 5 bus
      writeByte 0xFF05 0xFF bus
      writeByte 0xFF06 0xAB bus
      increaseTimer 6 bus
      readByte 0xFF05 bus `shouldReturn` 0xAB
      readByte 0xFF0F bus `shouldReturn` 4
      writeByte 0xFF05 0x77 bus
      readByte 0xFF05 bus `shouldReturn` 0x77

  it "execute first instruction" $ do
    cpu <- testCPU
    (cpu1, _) <- executeOneCPUInstruction cpu
    cpu1.registers.rSP `shouldBe` 0xFFFE
  it "cancels interrupt dispatch when pushing PC high changes IE" $ do

    bus <- initBus (BL.pack $ 0xFB : Prelude.replicate 255 0) (BL.replicate 0x8000 0)
    let cpu = initCPU bus
    -- Execute EI and NOP so the interrupt enable delay has elapsed.
    (cpu1, _) <- executeOneCPUInstruction cpu
    (cpu2, _) <- executeOneCPUInstruction cpu1
    let start = cpu2 {registers = cpu2.registers {rPC = 0x0200, rSP = 0x0000}}
    writeByte 0xFFFF 0x04 start.bus -- Enable Timer.
    writeByte 0xFF0F 0x04 start.bus -- Request Timer.
    writeByte 0xFFFE 0xAA start.bus
    let runService elapsed state
          | elapsed == 5 = return state
          | otherwise = do
              (state', cycles) <- CPU.execute state
              cycles `shouldSatisfy` (> 0)
              let elapsed' = elapsed + fromIntegral cycles
              elapsed' `shouldSatisfy` (<= 5)
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
    ppu <- initPPU cpu.bus
    (cpu1, ppu1) <- execute isBooted cpu ppu
    PPU.snapshotDisplay ppu1 >>= print
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
