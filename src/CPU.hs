module CPU (CPU (..), execute1, execute, initCPU, executeInstruction) where

import Bus
  ( Address,
    Bus,
    fetchInstruction,
    initBus,
    readByte,
    readByteHighMemory,
    readR16,
    readR16Mem,
    readR8,
    writeByte,
    writeByteHighMemory,
    writeR16,
    writeR8,
  )
import Data.Bits (shiftL, xor, (.&.), (.|.))
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Int (Int8)
import Data.Word
import Dbg
import Instruction (ALUOp (..), CBOp (..), Cycles (..), Instruction (..), OpCode (..))
import Registers

data CPU = CPU {registers :: Registers, bus :: Bus, clock :: Word64}

initCPU :: BL.ByteString -> BL.ByteString -> IO CPU
initCPU boot cartridge = do
  bus <- initBus boot cartridge
  return $ CPU {registers = initialRegisters, bus = bus, clock = 0}

advanceAddr :: Address -> Int8 -> Word16
advanceAddr pc imm8 =
  let offset = fromIntegral imm8 :: Int
   in fromIntegral (fromIntegral pc + offset)

advancePC :: Int8 -> Registers -> Registers
advancePC imm8 regs =
  regs {rPC = advanceAddr regs.rPC imm8}

execute :: (CPU -> IO Bool) -> CPU -> IO CPU
execute endPred cpu = do
  end <- endPred cpu
  if end
    then return cpu
    else do
      cpu1 <- execute1 cpu
      execute endPred cpu1

execute1 :: CPU -> IO CPU
execute1 cpu = do
  ins <- fetchInstruction cpu.registers.rPC cpu.bus
  let cpu1 = cpu {registers = advancePC (fromIntegral ins.len) cpu.registers}
  (cpu2, branched) <- executeInstruction cpu1 ins.op
  return $
    cpu2
      { clock =
          cpu1.clock
            + fromIntegral
              ( case ins.cycles of
                  Fixed n -> n
                  -- n < m
                  Branch n m -> if branched then m else n
              )
      }

condSatisfied :: Cond -> Registers -> Bool
condSatisfied cond regs = case cond of
  NZ -> not $ zflag regs
  Z -> zflag regs
  _ -> todo $ "cond " ++ show cond

push16 :: Word16 -> CPU -> IO CPU
push16 val cpu = do
  let regs = cpu.registers
      bus = cpu.bus
      sp = regs.rSP - 1
      (h, l) = toWord8s val
  _ <- writeByte sp h bus
  _ <- writeByte (sp - 1) l bus
  return cpu {registers = regs {rSP = sp - 1}}

pop16 :: CPU -> IO (CPU, Word16)
pop16 cpu = do
  let regs = cpu.registers
      bus = cpu.bus
      sp = regs.rSP
  l <- readByte sp bus
  h <- readByte (sp + 1) bus
  return (cpu {registers = regs {rSP = sp + 2}}, fromWord8s h l)

executeInstruction :: CPU -> OpCode -> IO (CPU, Bool)
executeInstruction cpu op = do
  let bus = cpu.bus
  let regs = cpu.registers
  case echo "execute op: " op of
    NOP -> return (cpu, False)
    HALT -> error "HALT"
    ALU_A_R8 XOR src -> do
      srcVal <- readR8 regs bus src
      dstVal <- readR8 regs bus A
      let a = srcVal `xor` dstVal
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag False
                  & setCflag False
                  & setHflag False
            },
          False
        )
    LD_r8_r8 dst src -> do
      val <- readR8 regs bus src
      regs1 <- writeR8 regs bus dst val
      return (cpu {registers = regs1}, False)
    LD_r16_imm16 r16 val -> do
      return (cpu {registers = writeR16 regs r16 val}, False)
    LDH_AtImm8_A offset -> do
      _ <- writeByteHighMemory offset regs.rA bus
      return (cpu, False)
    LD_AtR16mem_A dst ->
      let srcAddr = readR16Mem regs dst
       in do
            val <- readR8 regs bus A
            _ <- writeByte srcAddr val bus
            return (cpu {registers = updateR16MemHL dst regs}, False)
    LD_Addr16_A addr -> do
      a <- readR8 regs bus A
      _ <- writeByte addr a bus
      return (cpu, False)
    LDH_A_AtImm8 addr8 -> do
      val <- readByteHighMemory addr8 bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = regs'}, False)
    JR_imm8 offset ->
      return (cpu {registers = advancePC offset cpu.registers}, False)
    JR_cond_imm8 cond offset ->
      return $
        if condSatisfied cond regs
          then
            (cpu {registers = advancePC offset regs}, True)
          else (cpu, False)
    PREFIX_CB (BIT b3 r8) -> do
      val <- readR8 regs bus r8
      return
        ( cpu
            { registers =
                regs
                  & setZflag (not $ getB3 b3 val)
                  & setNflag False
                  & setHflag True
            },
          False
        )
    PREFIX_CB (RL r8) -> do
      val <- readR8 regs bus r8
      let c = cflag regs
          newc = (val .&. 0x80) /= 0
          val' = val `shiftL` 1 .|. (if c then 1 else 0)
      regs' <-
        writeR8
          ( regs
              & setZflag (val' == 0)
              & setNflag False
              & setHflag False
              & setCflag newc
          )
          bus
          r8
          val'
      return (cpu {registers = regs'}, False)
    LD_r8_imm8 r8 val -> do
      regs1 <- writeR8 regs bus r8 val
      return (cpu {registers = regs1}, False)
    LDH_AtC_A -> do
      _ <- writeByteHighMemory regs.rC regs.rA bus
      return (cpu, False)
    INC_r8 r8 -> do
      val <- readR8 regs bus r8
      let val' = val + 1
      regs' <- writeR8 regs bus r8 val'
      return
        ( cpu
            { registers =
                regs'
                  & setZflag (val' == 0)
                  & setNflag False
                  & setHflag ((val .&. 0x0F) == 0x0F)
            },
          False
        )
    INC_r16 r16 -> do
      let val = readR16 regs r16
          regs' = writeR16 regs r16 $ val + 1
      return (cpu {registers = regs'}, False)
    DEC_r8 r8 -> do
      val <- readR8 regs bus r8
      let val' = val - 1
      regs' <- writeR8 regs bus r8 $ val'
      return
        ( cpu
            { registers =
                regs'
                  & setZflag (val' == 0)
                  & setNflag True
                  & setHflag ((val .&. 0x0F) == 0x00)
            },
          False
        )
    LD_A_AtR16mem src -> do
      let addr = readR16Mem regs src
      val <- readByte addr bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = updateR16MemHL src regs'}, False)
    CALL_addr16 addr -> do
      cpu' <- push16 regs.rPC cpu
      return (cpu' {registers = cpu'.registers {rPC = addr}}, False)
    RET -> do
      (cpu', pc) <- pop16 cpu
      return (cpu {registers = echo "registers: " $ cpu'.registers {rPC = pc}}, False)
    PUSH stk -> do
      let val = getR16Stk stk regs
      cpu' <- push16 val cpu
      return (cpu', False)
    POP stk -> do
      (cpu', val) <- pop16 cpu
      return (cpu {registers = setR16Stk stk val cpu'.registers}, False)
    RLA -> do
      val <- readR8 regs bus A
      let c = cflag regs
          newc = (val .&. 0x80) /= 0
          val' = val `shiftL` 1 .|. (if c then 1 else 0)
      regs' <-
        writeR8
          ( regs
              & setZflag False
              & setNflag False
              & setHflag False
              & setCflag newc
          )
          bus
          A
          val'
      return (cpu {registers = regs'}, False)
    ALU_A_imm8 CP n -> do
      a <- readR8 regs bus A
      let regs' =
            regs
              & setZflag (a == n)
              & setNflag True
              & setHflag ((a .&. 0x0F) < (n .&. 0x0F))
              & setCflag (a < n)
      return (cpu {registers = regs'}, False)
    _ -> todo $ "execute instruction op " ++ show op
