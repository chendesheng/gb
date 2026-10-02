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
    readIF,
    readIE,
    writeIF,
    Interrupts,
    interruptAddress,
  )
import Data.Bits ((.<<.), xor, (.>>.), (.&.), (.|.))
import qualified Data.ByteString.Lazy as BL
import Data.Function ((&))
import Data.Int (Int8)
import Data.Word
import Data.List (intersect)
import Dbg
import Instruction
import Registers

data CPU = CPU {registers :: Registers, bus :: Bus, ime :: InterruptStep, currentInstruction :: Maybe OpCode }

data InterruptStep = Disabled | PendingEnable | GetIntRequest | Enabled InterruptServiceStep  deriving (Eq, Show)
data InterruptServiceStep = IntSrvWriteSPHigh | IntSrvWriteSPLow | IntSrvJmp Interrupts deriving (Eq, Show)

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
            return (cpu { currentInstruction = Just ins.op
                        , registers = advancePC (fromIntegral ins.len) cpu.registers
                        }
                  , ins.len * 4
                  )
      Just op -> do
        (cpu', elapsed) <- executeInstruction cpu{currentInstruction=Nothing} op
        return (cpu', elapsed)

executeInterruption :: CPU -> IO (CPU, Word8)
executeInterruption cpu = do
  case cpu.ime of
    PendingEnable -> return (cpu{ime=GetIntRequest}, 0)
    GetIntRequest -> do
      ie <- readIE cpu.bus
      if_ <- readIF cpu.bus
      return $ if null $ ie `intersect` if_ then
                 (cpu, 0)
               else
                (cpu{ime=Enabled IntSrvWriteSPHigh}, 8)
    Enabled IntSrvWriteSPHigh -> do
      let addr = cpu.registers.rPC
      cpu' <- push8 (addr .>>. 8 .&. 0xFF & fromIntegral) cpu
      return (cpu'{ime=Enabled IntSrvWriteSPLow}, 4)
    Enabled IntSrvWriteSPLow -> do
      ie <- readIE cpu.bus
      cpu' <- push8 (fromIntegral cpu.registers.rPC) cpu
      return (cpu'{ime=Enabled $ IntSrvJmp ie }, 4)
    Enabled (IntSrvJmp ie) -> do
      if_ <- readIF cpu.bus
      case ie `intersect` if_ of
        (int:_) -> do
          writeIF int False cpu.bus
          return (cpu{ime=Disabled, registers=cpu.registers{rPC=interruptAddress int}}, 4)
        _ -> return (cpu{ime=Disabled, registers=cpu.registers{rPC=0}}, 4)
    Disabled -> return (cpu, 0)

condSatisfied :: Cond -> Registers -> Bool
condSatisfied cond regs = case cond of
  NZ -> not $ zflag regs
  Z -> zflag regs
  _ -> todo $ "cond " ++ show cond

push8 :: Word8 -> CPU -> IO CPU
push8 val cpu = do
  let regs = cpu.registers
      bus = cpu.bus
      sp = regs.rSP - 1
  _ <- writeByte sp val bus
  return cpu {registers = regs {rSP = sp}}

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

callAddr16 :: Word16 -> CPU -> IO CPU
callAddr16 addr cpu = do
  let regs = cpu.registers
  cpu' <- push16 regs.rPC cpu
  return cpu' {registers = cpu'.registers {rPC = addr}}

memoryCycles :: R8 -> Word8
memoryCycles AtHL = 4
memoryCycles _ = 0

-- Return execution T-cycles only; instruction fetching is timed by execute.
executeInstruction :: CPU -> OpCode -> IO (CPU, Word8)
executeInstruction cpu op = do
  let bus = cpu.bus
  let regs = cpu.registers
  case op of
    NOP -> return (cpu, 0)
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
          memoryCycles src
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
          memoryCycles src
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
          memoryCycles src
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
      return (cpu {registers = regs'}, memoryCycles src)
    ALU_A_imm8 CP n -> do
      a <- readR8 regs bus A
      let regs' =
            regs
              & setZflag (a == n)
              & setNflag True
              & setHflag ((a .&. 0x0F) < (n .&. 0x0F))
              & setCflag (a < n)
      return (cpu {registers = regs'}, 0)
    LD_r8_r8 dst src -> do
      val <- readR8 regs bus src
      regs1 <- writeR8 regs bus dst val
      return (cpu {registers = regs1}, memoryCycles src + memoryCycles dst)
    LD_r16_imm16 r16 val -> do
      return (cpu {registers = writeR16 regs r16 val}, 0)
    LDH_AtImm8_A offset -> do
      _ <- writeByteHighMemory offset regs.rA bus
      return (cpu, 4)
    LD_AtR16mem_A dst ->
      let srcAddr = readR16Mem regs dst
       in do
            val <- readR8 regs bus A
            _ <- writeByte srcAddr val bus
            return (cpu {registers = updateR16MemHL dst regs}, 4)
    LD_Addr16_A addr -> do
      a <- readR8 regs bus A
      _ <- writeByte addr a bus
      return (cpu, 4)
    LDH_A_AtImm8 addr8 -> do
      val <- readByteHighMemory addr8 bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = regs'}, 4)
    JR_imm8 offset ->
      return (cpu {registers = advancePC offset cpu.registers}, 4)
    JR_cond_imm8 cond offset ->
      return $
        if condSatisfied cond regs
          then
            (cpu {registers = advancePC offset regs}, 4)
          else (cpu, 0)
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
          memoryCycles r8
        )
    PREFIX_CB (RL r8) -> do
      val <- readR8 regs bus r8
      let c = cflag regs
          newc = (val .&. 0x80) /= 0
          val' = val .<<. 1 .|. (if c then 1 else 0)
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
      return (cpu {registers = regs'}, 2 * memoryCycles r8)
    LD_r8_imm8 r8 val -> do
      regs1 <- writeR8 regs bus r8 val
      return (cpu {registers = regs1}, memoryCycles r8)
    LDH_AtC_A -> do
      _ <- writeByteHighMemory regs.rC regs.rA bus
      return (cpu, 4)
    INC_r8 SubOpRead r8 -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (INC_r8 (SubOpWrite val) r8)}, memoryCycles r8)
    INC_r8 (SubOpWrite val) r8 -> do
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
          memoryCycles r8
        )
    INC_r16 r16 -> do
      let val = readR16 regs r16
          regs' = writeR16 regs r16 $ val + 1
      return (cpu {registers = regs'}, 4)
    DEC_r8 SubOpRead r8 -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (DEC_r8 (SubOpWrite val) r8)}, memoryCycles r8)
    DEC_r8 (SubOpWrite val) r8 -> do
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
          memoryCycles r8
        )
    LD_A_AtR16mem src -> do
      let addr = readR16Mem regs src
      val <- readByte addr bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = updateR16MemHL src regs'}, 4)
    CALL_addr16 addr -> do
      cpu' <- callAddr16 addr cpu
      return (cpu', 12)
    RET -> do
      (cpu', pc) <- pop16 cpu
      return (cpu {registers = cpu'.registers {rPC = pc}}, 12)
    PUSH stk -> do
      let val = getR16Stk stk regs
      cpu' <- push16 val cpu
      return (cpu', 12)
    POP stk -> do
      (cpu', val) <- pop16 cpu
      return (cpu {registers = setR16Stk stk val cpu'.registers}, 8)
    RLA -> do
      val <- readR8 regs bus A
      let c = cflag regs
          newc = (val .&. 0x80) /= 0
          val' = val .<<. 1 .|. (if c then 1 else 0)
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
      return (cpu {registers = regs'}, 0)
    EI -> return (cpu {ime = case cpu.ime of
                                Disabled -> PendingEnable
                                PendingEnable -> PendingEnable
                                _ ->  cpu.ime
                                }, 0)
    DI -> return (cpu {ime = Disabled}, 0)
    RETI -> do
      -- RET + EI
      (cpu', pc) <- pop16 cpu
      return (cpu {registers = cpu'.registers {rPC = pc}, ime = GetIntRequest}, 12)
    _ -> todo $ "execute instruction op " ++ show op
