module InstructionSpec (spec) where

import Bus (readByte, readByteHighMemory, readR16, writeR8, writeByte, writeR16, writeByteHighMemory)
import CPU (CPU (..), executeInstruction, initCPU)
import qualified CPU
import Control.Monad (foldM)
import Data.Binary.Get (runGet)
import Data.Maybe (fromJust)
import Data.ByteString.Lazy as BL
import Data.Word
import Instruction
import qualified Instruction as I
import Registers
import Test.Hspec

spec :: SpecWith ()
spec = describe "Instruction" $ do
  it "decode 0x31" $ do
    let bs = BL.pack [0x31, 0xFE, 0xFF]
    runGet instructionDecoder bs `shouldBe` I.Instruction (LD_r16_imm16 SP 0xFFFE) 3
  it "ALU_A_R8 XOR A" $ do
    cpu <- testCPU
    (cpu1, 0) <- executeInstruction cpu (ALU_A_R8 XOR A)
    cpu1.registers.rA `shouldBe` 0x00
    zflag cpu1.registers `shouldBe` True
  it "LD_r8_r8 A B" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 B 0x01 cpu
    (cpu2, 0) <- executeInstruction cpu1 (LD_r8_r8 A B)
    cpu2.registers.rA `shouldBe` 0x01
  it "LD_r16_imm16 BC 0xABCD" $ do
    cpu <- testCPU
    (cpu1, 0) <- executeInstruction cpu (LD_r16_imm16 BC 0xABCD)
    readR16 cpu1.registers BC `shouldBe` 0xABCD
  it "LDH_AtImm8_A 0x01" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 A 0x12 cpu
    (cpu2, 4) <- executeInstruction cpu1 (LDH_AtImm8_A 0x01)
    res <- readByteHighMemory 0x01 cpu2.bus
    res `shouldBe` 0x12
  it "LD_AtR16mem_A HLi" $ do
    cpu <- testCPU
    cpu1 <- cpuInitR8 [(A, 0xCC), (H, 0x80), (L, 0x00)] cpu
    (cpu2, 4) <- executeInstruction cpu1 (LD_AtR16mem_A HLi)
    let hl = readR16 cpu2.registers HL
    hl `shouldBe` 0x8001
    val <- readByte 0x8000 cpu2.bus
    val `shouldBe` 0xCC
  it "JR_imm8 0x12" $ do
    cpu <- testCPU
    (cpu1, 4) <- executeInstruction cpu (JR_imm8 0x12)
    cpu1.registers.rPC `shouldBe` 0x12
  it "JR_cond_imm8 Z 0x12" $ do
    cpu <- testCPU
    let regs = setZflag True cpu.registers
        cpu1 = cpu {registers = regs}
    (cpu2, 4) <- executeInstruction cpu1 (JR_cond_imm8 Z 0x12)
    cpu2.registers.rPC `shouldBe` 0x12
  it "JR_cond_imm8 Z 0x12 (not jump)" $ do
    cpu <- testCPU
    let regs = setZflag False cpu.registers
        cpu1 = cpu {registers = regs}
    (cpu2, 0) <- executeInstruction cpu1 (JR_cond_imm8 Z 0x12)
    cpu2.registers.rPC `shouldBe` 0x00
  it "PREFIX_CB (BIT 0x07 A)" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 A 0x80 cpu
    (cpu2, 0) <- executeInstruction cpu1 (PREFIX_CB $ BIT 0x07 A)
    cpu2.registers.rF `shouldBe` 0x20 -- 10100000b
  it "PREFIX_CB (BIT 0x07 A) <2>" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 A 0x00 cpu
    (cpu2, 0) <- executeInstruction cpu1 (PREFIX_CB $ BIT 0x07 A)
    cpu2.registers.rF `shouldBe` 0xA0 -- 10100000b
  it "LD_r8_imm8 C 7" $ do
    cpu <- testCPU
    (cpu1, 0) <- executeInstruction cpu (LD_r8_imm8 C 7)
    cpu1.registers.rC `shouldBe` 0x07
  it "LDH_AtC_A" $ do
    cpu <- testCPU
    cpu1 <- cpuInitR8 [(C, 0x01), (A, 0xAA)] cpu
    (cpu2, 4) <- executeInstruction cpu1 LDH_AtC_A
    val <- readByteHighMemory 0x01 cpu2.bus
    val `shouldBe` 0xAA
  it "INC_r8 SubOpRead D" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 D 0x0F cpu
    (cpu2, 0) <- executeInstruction cpu1 $ INC_r8 SubOpRead D
    (cpu3, 0) <- executeInstruction cpu1 $ fromJust cpu2.currentInstruction
    cpu3.registers.rD `shouldBe` 0x10
    hflag cpu3.registers `shouldBe` True
  it "INC_r16 BC" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR16 BC 0x0102 cpu
    (cpu2, 4) <- executeInstruction cpu1 $ INC_r16 BC
    cpu2.registers.rB `shouldBe` 0x01
    cpu2.registers.rC `shouldBe` 0x03
  it "DEC_r8 E" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 E 0x01 cpu
    (cpu2, 0) <- executeInstruction cpu1 $ DEC_r8 SubOpRead E
    (cpu3, 0) <- executeInstruction cpu2 $ fromJust cpu2.currentInstruction
    cpu3.registers.rE `shouldBe` 0x00
    zflag cpu3.registers `shouldBe` True
  it "DEC_r8 E <2>" $ do
    cpu <- testCPU
    cpu1 <- cpuWriteR8 E 0x10 cpu
    (cpu2, 0) <- executeInstruction cpu1 $ DEC_r8 SubOpRead E
    (cpu3, 0) <- executeInstruction cpu2 $ fromJust cpu2.currentInstruction
    cpu3.registers.rE `shouldBe` 0x0F
    hflag cpu3.registers `shouldBe` True
  it "LD_A_AtR16mem HLd" $ do
    cpu <- testCPU
    writeByte 0x8001 0xAA cpu.bus
    cpu1 <- cpuInitR8 [(H, 0x80), (L, 0x01)] cpu
    (cpu2, 4) <- executeInstruction cpu1 $ LD_A_AtR16mem HLd
    cpu2.registers.rL `shouldBe` 0x00
    cpu2.registers.rA `shouldBe` 0xAA
  it "CALL_addr16 0x1234" $ do
    cpu <- cpuInitPC 0x0003 <$> testCPU
    (cpu1, 0) <- executeInstruction cpu $ LD_r16_imm16 SP 0xFFFE
    (cpu2, 12) <- executeInstructionSteps cpu1 $ CALL_addr16 SubOpCallWait 0x1234
    cpu2.registers.rSP `shouldBe` 0xFFFC
    cpu2.registers.rPC `shouldBe` 0x1234
    l <- readByte 0xFFFC cpu.bus
    l `shouldBe` 0x03
    h <- readByte 0xFFFD cpu.bus
    h `shouldBe` 0x00
  it "RET" $ do
    cpu <- cpuInitPC 0x0003 <$> testCPU
    (cpu1, 0) <- executeInstruction cpu $ LD_r16_imm16 SP 0xFFFE
    (cpu2, 12) <- executeInstructionSteps cpu1 $ CALL_addr16 SubOpCallWait 0x1234
    cpu2.registers.rSP `shouldBe` 0xFFFC
    cpu2.registers.rPC `shouldBe` 0x1234
    (cpu3, 12) <- executeInstructionSteps cpu2 $ RET SubOpPopLow
    cpu3.registers.rPC `shouldBe` 0x0003
    cpu3.registers.rSP `shouldBe` 0xFFFE
  it "PUSH BCstk" $ do
    cpu <- testCPU
    cpu2 <- cpuInitR8 [(B, 0x12), (C, 0xA0)] cpu
    (cpu3, 0) <- executeInstruction cpu2 $ LD_r16_imm16 SP 0xFFFE
    (cpu4, 12) <- executeInstructionSteps cpu3 $ PUSH SubOpPushWait BCstk
    cpu4.registers.rSP `shouldBe` 0xFFFC
    l <- readByte 0xFFFC cpu.bus
    l `shouldBe` 0xA0
    h <- readByte 0xFFFD cpu.bus
    h `shouldBe` 0x12
  it "PREFIX_CB (RL C)" $ do
    cpu <- cpuSetFlags 0x00 <$> (testCPU >>= cpuWriteR8 C 0x80)
    (cpu1, 0) <- executeInstruction cpu $ PREFIX_CB $ RL C
    zflag cpu1.registers `shouldBe` True
    nflag cpu1.registers `shouldBe` False
    hflag cpu1.registers `shouldBe` False
    cflag cpu1.registers `shouldBe` True
  it "RLA" $ do
    cpu <- cpuSetFlags 0x00 <$> (testCPU >>= cpuWriteR8 A 0x80)
    (cpu1, 0) <- executeInstruction cpu RLA
    zflag cpu1.registers `shouldBe` False
    nflag cpu1.registers `shouldBe` False
    hflag cpu1.registers `shouldBe` False
    cflag cpu1.registers `shouldBe` True
  it "POP AFstk" $ do
    cpu <-  cpuSetFlags 0x80 <$> (testCPU >>= cpuWriteR8 A 0x80)
    cpu1 <- cpuWriteR16 SP 0xFFF0 cpu
    writeByte 0xFFF0 0xBF cpu1.bus
    writeByte 0xFFF1 0x12 cpu1.bus
    (cpu2, 8) <- executeInstructionSteps cpu1 $ POP SubOpPopLow AFstk
    cpu2.registers.rSP `shouldBe` 0xFFF2
    cpu2.registers.rA `shouldBe` 0x12
    cpu2.registers.rF `shouldBe` 0xB0
  it "POP BCstk" $ do
    cpu <- testCPU >>= cpuWriteR16 SP 0xFFF0
    writeByte 0xFFF0 0x34 cpu.bus
    writeByte 0xFFF1 0x12 cpu.bus
    (cpu1, 8) <- executeInstructionSteps cpu $ POP SubOpPopLow BCstk
    cpu1.registers.rSP `shouldBe` 0xFFF2
    getBC cpu1.registers `shouldBe` 0x1234
  it "ALU_A_imm8 CP 0x34" $ do
    cpu <- testCPU >>= cpuWriteR8 A 0x30
    (cpu1, 0) <- executeInstruction cpu $ ALU_A_imm8 CP 0x34
    cpu1.registers.rF `shouldBe` 0x70
  it "LD_Addr16_A 0x8000" $ do
    cpu <- testCPU >>= cpuWriteR8 A 0xAA
    (_, 4) <- executeInstruction cpu $ LD_Addr16_A 0x8000
    val <- readByte 0x8000 cpu.bus
    val `shouldBe` 0xAA
  it "LDH_A_AtImm8 0x10" $ do
    cpu <- testCPU
    writeByteHighMemory 0x10 0xBB cpu.bus
    (cpu1, 4) <- executeInstruction cpu $ LDH_A_AtImm8 0x10
    cpu1.registers.rA `shouldBe` 0xBB

-- Run execution steps only; these tests start after instruction fetching.
executeInstructionSteps :: CPU -> OpCode -> IO (CPU, Word8)
executeInstructionSteps cpu op = go 0 cpu{currentInstruction = Just op}
  where
    go elapsed state = do
      (state', cycles) <- CPU.execute state
      let elapsed' = elapsed + cycles
      case state'.currentInstruction of
        Nothing -> return (state', elapsed')
        Just _ -> go elapsed' state'

cpuSetFlags :: Word8 -> CPU -> CPU
cpuSetFlags f cpu =
  cpu{registers = cpu.registers{rF = f}}

cpuInitPC :: Word16 -> CPU -> CPU
cpuInitPC pc cpu =
  let regs = cpu.registers
      regs' = regs{rPC = pc}
  in
    cpu{registers=regs'}

cpuInitR8 :: [(R8, Word8)] -> CPU -> IO CPU
cpuInitR8 vals cpu =
  foldM (\cpu' (r8, val) -> cpuWriteR8 r8 val cpu') cpu vals

cpuWriteR8 :: R8 -> Word8 -> CPU -> IO CPU
cpuWriteR8 r8 val cpu = do
  regs <- writeR8 cpu.registers cpu.bus r8 val
  return cpu {registers = regs}

cpuWriteR16 :: R16 -> Word16 -> CPU -> IO CPU
cpuWriteR16 r16 val cpu = do
  let regs = writeR16 cpu.registers r16 val
  return cpu {registers = regs}

testCPU :: IO CPU
testCPU = do
  bootRom <- BL.readFile "./test/fixtures/dmg.bin"
  initCPU bootRom (BL.pack [])
