{-# OPTIONS_GHC -Wno-name-shadowing #-}
module CPU (CPU (..), execute, initCPU, executeInstruction) where

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
    writeIF,
    isInterruptRequested,
    Interrupt(..),
    interruptAddress,
  )
import Data.Bits (shiftL, xor, (.&.), (.|.))
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Int (Int8)
import Data.Word
import Dbg
import Instruction (ALUOp (..), CBOp (..), Cycles (..), Instruction (..), OpCode (..))
import Registers

data CPU = CPU {registers :: Registers, bus :: Bus, ime :: InterruptStep, currentInstruction :: Maybe Instruction }

data InterruptStep = Disabled | Enabled | PendingEnable deriving (Eq, Show)

initCPU :: BL.ByteString -> BL.ByteString -> IO CPU
initCPU boot cartridge = do
  bus <- initBus boot cartridge
  return $ CPU {registers = initialRegisters, bus = bus, ime = Disabled, currentInstruction = Nothing}

advanceAddr :: Address -> Int8 -> Word16
advanceAddr pc imm8 =
  let offset = fromIntegral imm8 :: Int
   in fromIntegral (fromIntegral pc + offset)

advancePC :: Int8 -> Registers -> Registers
advancePC imm8 regs =
  regs {rPC = advanceAddr regs.rPC imm8}

addWithCarry :: Word8 -> Word8 -> Bool -> (Word8, Bool, Bool)
addWithCarry a b carry =
  let carryIn = if carry then 1 else 0
      total = fromIntegral a + fromIntegral b + carryIn :: Int
      result = fromIntegral total :: Word8
      carryOut = total > 0xFF
      halfCarry = ((a .&. 0x0F) + (b .&. 0x0F) + fromIntegral carryIn) > 0x0F
   in (result, carryOut, halfCarry)

execute :: CPU -> IO (CPU, Word8)
execute cpu = do
  case cpu.currentInstruction of
      Nothing -> do
        (cpu, elapsed) <- executeInterruption cpu
        if elapsed > 0 then
          return (cpu, elapsed)
        else do
            ins <- fetchInstruction cpu.registers.rPC cpu.bus
            return (cpu { currentInstruction = Just ins
                        , registers = advancePC (fromIntegral ins.len) cpu.registers
                        }
                  , ins.len * 4
                  )
      Just ins -> do
        (cpu', branched) <- executeInstruction cpu ins.op
        return (cpu'{currentInstruction=Nothing}, (case ins.cycles of
                        Fixed n -> n
                        -- n < m
                        Branch n m -> if branched then m else n) - ins.len * 4)

executeInterruption :: CPU -> IO (CPU, Word8)
executeInterruption cpu = do
  case cpu.ime of
    PendingEnable -> return (cpu{ime=Enabled}, 0)
    Enabled -> do
      maybeInt <- findM (\int -> isInterruptRequested int cpu.bus) [VBlank .. Joypad]
      case maybeInt of
        Just int -> do
          writeIF int False cpu.bus
          (cpu, _) <- callAddr16 (interruptAddress int) cpu
          return (cpu{ime=Disabled}, 20)
        _ ->
          return (cpu, 0)
    _ -> return (cpu, 0)

findM :: Monad m => (a -> m Bool) -> [a] -> m (Maybe a)
findM _ [] = return Nothing
findM p (x:xs) = do
  b <- p x
  if b then return (Just x) else findM p xs

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

callAddr16 :: Word16 -> CPU -> IO (CPU, Bool)
callAddr16 addr cpu = do
  let regs = cpu.registers
  cpu' <- push16 regs.rPC cpu
  return (cpu' {registers = cpu'.registers {rPC = addr}}, False)

executeInstruction :: CPU -> OpCode -> IO (CPU, Bool)
executeInstruction cpu op = do
  let bus = cpu.bus
  let regs = cpu.registers
  case op of
    NOP -> return (cpu, False)
    HALT -> error "HALT"
    ALU_A_R8 ADD src -> do
      dstVal <- readR8 regs bus A
      srcVal <- readR8 regs bus src
      let (a, carry, half) = addWithCarry dstVal srcVal False
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag False
                  & setCflag carry
                  & setHflag half
            },
          False
        )
    ALU_A_R8 SUB src -> do
      dstVal <- readR8 regs bus A
      srcVal <- readR8 regs bus src
      let a = dstVal - srcVal
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag True
                  & setCflag (dstVal < srcVal)
                  & setHflag ((dstVal .&. 0x0F) < (srcVal .&. 0x0F))
            },
          False
        )
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
    ALU_A_R8 CP src -> do
      a <- readR8 regs bus A
      srcVal <- readR8 regs bus src
      let regs' =
            regs
              & setZflag (a == srcVal)
              & setNflag True
              & setHflag ((a .&. 0x0F) < (srcVal .&. 0x0F))
              & setCflag (a < srcVal)
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
      regs' <- writeR8 regs bus r8 val'
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
    CALL_addr16 addr -> callAddr16 addr cpu
    RET -> do
      (cpu', pc) <- pop16 cpu
      return (cpu {registers = cpu'.registers {rPC = pc}}, False)
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
    EI -> return (cpu {ime = if cpu.ime == Enabled then Enabled else PendingEnable}, False)
    DI -> return (cpu {ime = Disabled}, False)
    RETI -> do
      -- RET + EI
      (cpu', pc) <- pop16 cpu
      return (cpu {registers = cpu'.registers {rPC = pc}, ime = Enabled}, False)
    _ -> todo $ "execute instruction op " ++ show op
