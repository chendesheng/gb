{-# OPTIONS_GHC -Wno-name-shadowing #-}
module CPU (CPU (..), execute, initCPU, executeInstruction) where

import Bus
  ( Address,
    Bus,
    fetchInstruction,
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
    increaseTimer,
    executeDMACopy,
  )
import Data.Bits ((.<<.), xor, (.>>.), (.&.), (.|.), complement)
import Data.Function ((&))
import Data.Int (Int8)
import Data.Word
import Data.List (intersect)
import Instruction
import Registers

data CPU = CPU
  { registers :: Registers
  , bus :: Bus
  , ime :: InterruptStep
  , currentInstruction :: Maybe OpCode
  }

data InterruptStep
  = Disabled
  | EnableAfterNextInstruction
  | GetIntRequest
  | Enabled InterruptServiceStep  deriving (Eq, Show)
data InterruptServiceStep = IntSrvWriteSPHigh | IntSrvWriteSPLow | IntSrvJmp Interrupts deriving (Eq, Show)

initCPU :: Bus -> CPU
initCPU bus = do
  CPU {registers=initialRegisters, bus=bus, ime=Disabled, currentInstruction=Nothing}

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

addSignedToSP :: Word16 -> Int8 -> (Word16, Bool, Bool)
addSignedToSP sp offset =
  let unsignedOffset = fromIntegral (fromIntegral offset :: Word8) :: Word16
      result = fromIntegral ((fromIntegral sp :: Int) + fromIntegral offset)
      halfCarry = (sp .&. 0x0F) + (unsignedOffset .&. 0x0F) > 0x0F
      carry = (sp .&. 0xFF) + unsignedOffset > 0xFF
   in (result, carry, halfCarry)

-- TODO: make timer accurate, need split to per M-cycle
execute :: CPU -> IO (CPU, Word8)
execute cpu = do
  (cpu1, elapsed) <- go cpu
  if elapsed > 0 then do
    executeDMACopy elapsed cpu.bus
    increaseTimer elapsed cpu1.bus
    return (cpu1, elapsed)
  else
    return (cpu1, elapsed)
  where
    go cpu =
      case cpu.currentInstruction of
          Nothing -> do
            (cpu, elapsed) <- executeInterruption cpu
            if elapsed > 0 then do
              return (cpu, elapsed)
            else do
                ins <- fetchInstruction cpu.registers.rPC cpu.bus
                return (cpu { currentInstruction = Just ins.op
                            , registers = advancePC (fromIntegral ins.len) cpu.registers
                            }
                      , ins.len
                      )
          Just HALT -> do
            ie <- readIE cpu.bus
            if_ <- readIF cpu.bus
            if null (ie `intersect` if_)
              then return (cpu, 1)
              else go cpu{currentInstruction = Nothing}
          Just (INVALID _) -> return (cpu, 1)
          Just op -> do
            (cpu', elapsed) <- executeInstruction cpu{currentInstruction=Nothing} op
            return (cpu', elapsed)

executeInterruption :: CPU -> IO (CPU, Word8)
executeInterruption cpu = do
  case cpu.ime of
    EnableAfterNextInstruction -> return (cpu{ime=GetIntRequest}, 0)
    GetIntRequest -> do
      ie <- readIE cpu.bus
      if_ <- readIF cpu.bus
      return $ if null $ ie `intersect` if_ then (cpu, 0)
               else (cpu{ime=Enabled IntSrvWriteSPHigh, currentInstruction=Nothing}, 2)
    Enabled IntSrvWriteSPHigh -> do
      let addr = cpu.registers.rPC
      cpu' <- push8High addr cpu
      return (cpu'{ime=Enabled IntSrvWriteSPLow}, 1)
    Enabled IntSrvWriteSPLow -> do
      ie <- readIE cpu.bus
      cpu' <- push8Low cpu.registers.rPC cpu
      return (cpu'{ime=Enabled $ IntSrvJmp ie }, 1)
    Enabled (IntSrvJmp ie) -> do
      if_ <- readIF cpu.bus
      case ie `intersect` if_ of
        (int:_) -> do
          writeIF int False cpu.bus
          return (cpu{ime=Disabled, registers=cpu.registers{rPC=interruptAddress int}}, 1)
        _ -> return (cpu{ime=Disabled, registers=cpu.registers{rPC=0}}, 1)
    Disabled -> return (cpu, 0)

condSatisfied :: Cond -> Registers -> Bool
condSatisfied cond regs = case cond of
  NZ -> not $ zflag regs
  Z -> zflag regs
  NC -> not $ cflag regs
  Cc -> cflag regs

push8 :: Word8 -> CPU -> IO CPU
push8 val cpu = do
  let regs = cpu.registers
      bus = cpu.bus
      sp = regs.rSP - 1
  writeByte sp val bus
  return cpu {registers = regs {rSP = sp}}

push8High :: Word16 -> CPU -> IO CPU
push8High = push8 . highByte

push8Low :: Word16 -> CPU -> IO CPU
push8Low = push8 . lowByte

toWord16 :: Word8 -> Word8 -> Word16
toWord16 l h = (fromIntegral h .<<. 8) .|. fromIntegral l

pop8 :: CPU -> IO (CPU, Word8)
pop8 cpu = do
  let regs = cpu.registers
      bus = cpu.bus
      sp = regs.rSP
  val <- readByte sp bus
  return (cpu {registers = regs {rSP = sp + 1}}, val)

memoryCycles :: R8 -> Word8
memoryCycles AtHL = 1
memoryCycles _ = 0

highByte :: Word16 -> Word8
highByte addr = addr .>>. 8 .&. 0xFF & fromIntegral

lowByte :: Word16 -> Word8
lowByte = fromIntegral

-- Return execution M-cycles only; instruction fetching is timed by execute.
executeInstruction :: CPU -> OpCode -> IO (CPU, Word8)
executeInstruction cpu op = do
  let bus = cpu.bus
  let regs = cpu.registers
  case op of
    NOP -> return (cpu, 0)
    HALT -> return (cpu{currentInstruction=Just HALT}, 0)
    ALU_A_imm8 ADC val -> do
      dstVal <- readR8 regs bus A
      let (a, carry, half) = addWithCarry dstVal val (cflag regs)
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
          0
        )
    ALU_A_imm8 SBC val -> do
      dstVal <- readR8 regs bus A
      let borrow = if cflag regs then 1 else 0 :: Int
          a = dstVal - val - fromIntegral borrow
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag True
                  & setCflag (fromIntegral dstVal < (fromIntegral val + borrow :: Int))
                  & setHflag ((dstVal .&. 0x0F) < ((val .&. 0x0F) + fromIntegral borrow))
            },
          0
        )
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
    ALU_A_R8 OR src -> do
      srcVal <- readR8 regs bus src
      dstVal <- readR8 regs bus A
      let a = srcVal .|. dstVal
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
    ALU_A_R8 AND src -> do
      srcVal <- readR8 regs bus src
      dstVal <- readR8 regs bus A
      let a = srcVal .&. dstVal
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag False
                  & setCflag False
                  & setHflag True
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
    ALU_A_R8 ADC src -> do
      dstVal <- readR8 regs bus A
      srcVal <- readR8 regs bus src
      let (a, carry, half) = addWithCarry dstVal srcVal (cflag regs)
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
    ALU_A_R8 SBC src -> do
      dstVal <- readR8 regs bus A
      srcVal <- readR8 regs bus src
      let borrow = if cflag regs then 1 else 0 :: Int
          a = dstVal - srcVal - fromIntegral borrow
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag True
                  & setCflag (fromIntegral dstVal < (fromIntegral srcVal + borrow :: Int))
                  & setHflag ((dstVal .&. 0x0F) < ((srcVal .&. 0x0F) + fromIntegral borrow))
            },
          memoryCycles src
        )
    ALU_A_imm8 CP n -> do
      a <- readR8 regs bus A
      let regs' =
            regs
              & setZflag (a == n)
              & setNflag True
              & setHflag ((a .&. 0x0F) < (n .&. 0x0F))
              & setCflag (a < n)
      return (cpu {registers = regs'}, 0)
    ALU_A_imm8 AND n -> do
      dstVal <- readR8 regs bus A
      let a = dstVal .&. n
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag False
                  & setCflag False
                  & setHflag True
            },
          0
        )
    ALU_A_imm8 ADD n -> do
      dstVal <- readR8 regs bus A
      let (a, carry, half) = addWithCarry dstVal n False
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
          0
        )
    ALU_A_imm8 SUB n -> do
      dstVal <- readR8 regs bus A
      let a = dstVal - n
      regs1 <- writeR8 regs bus A a
      return
        ( cpu
            { registers =
                regs1
                  & setZflag (a == 0)
                  & setNflag True
                  & setCflag (dstVal < n)
                  & setHflag ((dstVal .&. 0x0F) < (n .&. 0x0F))
            },
          0
        )
    ALU_A_imm8 XOR n -> do
      dstVal <- readR8 regs bus A
      let a = dstVal `xor` n
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
          0
        )
    ALU_A_imm8 OR n -> do
      dstVal <- readR8 regs bus A
      let a = dstVal .|. n
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
          0
        )
    LD_r8_r8 dst src -> do
      val <- readR8 regs bus src
      regs1 <- writeR8 regs bus dst val
      return (cpu {registers = regs1}, memoryCycles src + memoryCycles dst)
    LD_r16_imm16 r16 val -> do
      return (cpu {registers = writeR16 regs r16 val}, 0)
    LDH_AtImm8_A offset -> do
      writeByteHighMemory offset regs.rA bus
      return (cpu, 1)
    LD_AtR16mem_A dst ->
      let srcAddr = readR16Mem regs dst
       in do
            val <- readR8 regs bus A
            writeByte srcAddr val bus
            return (cpu {registers = updateR16MemHL dst regs}, 1)
    LD_Addr16_A addr -> do
      a <- readR8 regs bus A
      writeByte addr a bus
      return (cpu, 1)
    LD_imm16_SP SubOpWriteLow addr -> do
      let sp = readR16 regs SP
      writeByte addr (lowByte sp) bus
      return (cpu {currentInstruction=Just (LD_imm16_SP (SubOpWriteHigh (highByte sp)) addr)}, 1)
    LD_imm16_SP (SubOpWriteHigh value) addr -> do
      writeByte (addr + 1) value bus
      return (cpu {currentInstruction=Nothing}, 1)
    LDH_A_C -> do
      val <- readByteHighMemory regs.rC bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = regs'}, 1)
    LDH_A_AtImm8 addr8 -> do
      val <- readByteHighMemory addr8 bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = regs'}, 1)
    LD_A_imm16 val -> do
      val' <- readByte val bus
      regs' <- writeR8 regs bus A val'
      return (cpu {registers = regs'}, 1)
    JR_imm8 offset ->
      return (cpu {registers = advancePC offset cpu.registers}, 1)
    JR_cond_imm8 cond offset ->
      return $
        if condSatisfied cond regs
          then
            (cpu {registers = advancePC offset regs}, 1)
          else (cpu, 0)
    PREFIX_CB (BIT  b3 r8) -> do
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
    PREFIX_CB (SRL SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (SRL (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (SRL (SubOpWrite val) r8) -> do
      let newc = (val .&. 0x01) /= 0
          val' = val .>>. 1
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
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (SRA SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (SRA (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (SRA (SubOpWrite val) r8) -> do
      let newc = (val .&. 0x01) /= 0
          val' = val .>>. 1 .|. (val .&. 0x80)
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
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (RLC SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (RLC (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (RLC (SubOpWrite val) r8) -> do
      let newc = (val .&. 0x80) /= 0
          val' = val .<<. 1 .|. (val .>>. 7)
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
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (RL SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (RL (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (RL (SubOpWrite val) r8) -> do
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
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (RR SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (RR (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (RR (SubOpWrite val) r8) -> do
      let c = cflag regs
          newc = (val .&. 0x01) /= 0
          val' = val .>>. 1 .|. (if c then 0x80 else 0)
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
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (RES SubOpRead b3 r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (RES (SubOpWrite val) b3 r8))}, memoryCycles r8)
    PREFIX_CB (RES (SubOpWrite val) b3 r8) -> do
      let val' = val .&. complement (1 .<<. fromIntegral b3)
      regs' <- writeR8 regs bus r8 val'
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (SET SubOpRead b3 r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (SET (SubOpWrite val) b3 r8))}, memoryCycles r8)
    PREFIX_CB (SET (SubOpWrite val) b3 r8) -> do
      let val' = val .|. (1 .<<. fromIntegral b3)
      regs' <- writeR8 regs bus r8 val'
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (SWAP SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (SWAP (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (SWAP (SubOpWrite val) r8) -> do
      let val' = (val .<<. 4) .|. (val .>>. 4)
      regs' <-
        writeR8
          ( regs
              & setZflag (val' == 0)
              & setNflag False
              & setHflag False
              & setCflag False
          )
          bus
          r8
          val'
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (SLA SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (SLA (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (SLA (SubOpWrite val) r8) -> do
      let newc = (val .&. 0x80) /= 0
          val' = val .<<. 1
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
      return (cpu {registers = regs'}, memoryCycles r8)
    PREFIX_CB (RRC SubOpRead r8) -> do
      val <- readR8 regs bus r8
      return (cpu{currentInstruction=Just (PREFIX_CB (RRC (SubOpWrite val) r8))}, memoryCycles r8)
    PREFIX_CB (RRC (SubOpWrite val) r8) -> do
      let newc = (val .&. 0x01) /= 0
          val' = val .>>. 1 .|. (val .<<. 7)
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
      return (cpu {registers = regs'}, memoryCycles r8)
    LD_r8_imm8 r8 val -> do
      regs1 <- writeR8 regs bus r8 val
      return (cpu {registers = regs1}, memoryCycles r8)
    LDH_AtC_A -> do
      writeByteHighMemory regs.rC regs.rA bus
      return (cpu, 1)
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
      return (cpu {registers = regs'}, 1)
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
    DEC_r16 BC -> do
      let val = readR16 regs BC
          regs' = writeR16 regs BC $ val - 1
      return (cpu {registers = regs'}, 1)
    DEC_r16 DE -> do
      let val = readR16 regs DE
          regs' = writeR16 regs DE $ val - 1
      return (cpu {registers = regs'}, 1)
    DEC_r16 HL -> do
      let val = readR16 regs HL
          regs' = writeR16 regs HL $ val - 1
      return (cpu {registers = regs'}, 1)
    DEC_r16 SP -> do
      let val = readR16 regs SP
          regs' = writeR16 regs SP $ val - 1
      return (cpu {registers = regs'}, 1)
    LD_A_AtR16mem src -> do
      let addr = readR16Mem regs src
      val <- readByte addr bus
      regs' <- writeR8 regs bus A val
      return (cpu {registers = updateR16MemHL src regs'}, 1)
    LD_HL_SP_plus_imm8 i -> do
      let (res, c, h) = addSignedToSP (readR16 regs SP) i
          regs' =
            writeR16 regs HL res
              & setZflag False
              & setNflag False
              & setHflag h
              & setCflag c
      return (cpu {registers = regs'}, 1)
    LD_SP_HL -> return (cpu {registers = writeR16 regs SP (readR16 regs HL)}, 1)
    CALL_addr16 SubOpCallWait addr ->
      return (cpu {currentInstruction=Just (CALL_addr16 SubOpCallPushHigh addr)}, 1)
    CALL_addr16 SubOpCallPushHigh addr -> do
      cpu' <- push8High cpu.registers.rPC cpu
      return (cpu'{currentInstruction=Just (CALL_addr16 SubOpCallJmp addr)}, 1)
    CALL_addr16 SubOpCallJmp addr -> do
      cpu' <- push8Low cpu.registers.rPC cpu
      return (cpu'{registers=cpu'.registers{rPC=addr}}, 1)
    CALL_cond_imm16 cond addr ->
      if condSatisfied cond regs
        then return (cpu {currentInstruction=Just (CALL_addr16 SubOpCallPushHigh addr)}, 1)
        else return (cpu, 0)
    RET SubOpPopLow -> do
      (cpu', low) <- pop8 cpu
      return (cpu'{currentInstruction=Just (RET $ SubOpPopHigh low)}, 1)
    RET (SubOpPopHigh low) -> do
      (cpu', high) <- pop8 cpu
      let pc = toWord16 low high
      return (cpu{registers = cpu'.registers {rPC = pc}}, 2)
    RET_cond cond -> do
      if condSatisfied cond regs
      then return (cpu{currentInstruction=Just (RET SubOpPopLow)}, 1)
      else return (cpu, 1)
    PUSH SubOpPushWait stk -> do
      return (cpu{currentInstruction=Just (PUSH SubOpPushHigh stk)}, 1)
    PUSH SubOpPushHigh stk -> do
      let val = getR16Stk stk regs
      cpu' <- push8High val cpu
      return (cpu'{currentInstruction=Just (PUSH SubOpPushLow stk)}, 1)
    PUSH SubOpPushLow stk -> do
      let val = getR16Stk stk regs
      cpu' <- push8Low val cpu
      return (cpu', 1)
    POP SubOpPopLow stk -> do
      (cpu', low) <- pop8 cpu
      return (cpu'{currentInstruction=Just (POP (SubOpPopHigh low) stk)}, 1)
    POP (SubOpPopHigh low) AFstk -> do
      (cpu', high) <- pop8 cpu
      return (cpu {registers = setR16Stk AFstk (toWord16 (low .&. 0xF0) high) cpu'.registers}, 1)
    POP (SubOpPopHigh low) stk -> do
      (cpu', high) <- pop8 cpu
      return (cpu {registers = setR16Stk stk (toWord16 low high) cpu'.registers}, 1)
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
    RLCA -> do
      val <- readR8 regs bus A
      let newc = (val .&. 0x80) /= 0
          val' = val .<<. 1 .|. (val .>>. 7)
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
                                Disabled -> EnableAfterNextInstruction
                                EnableAfterNextInstruction -> EnableAfterNextInstruction
                                _ ->  cpu.ime
                                }, 0)
    DI -> return (cpu {ime = Disabled}, 0)
    RETI SubOpPopLow -> do
      (cpu', low) <- pop8 cpu
      return (cpu'{currentInstruction=Just (RETI $ SubOpPopHigh low)}, 1)
    RETI (SubOpPopHigh low) -> do
      -- RET + enable IME
      (cpu', high) <- pop8 cpu
      let pc = toWord16 low high
      return (cpu'{registers = cpu'.registers {rPC = pc}, ime = GetIntRequest}, 2)
    JP_imm16 addr ->
      return (cpu{registers = cpu.registers {rPC = addr}}, 1)
    JP_HL -> return (cpu {registers = cpu.registers {rPC = readR16 regs HL}}, 0)
    JP_cond_imm16 cond addr ->
      if condSatisfied cond regs
        then return (cpu{registers = cpu.registers {rPC = addr}}, 1)
        else return (cpu, 0)
    STOP -> return (cpu, 0)
    CPL -> return
        ( cpu
            { registers =
                regs
                  & modifyR8 A complement
                  & setNflag True
                  & setHflag True
            },
          0
        )
    RST SubOpPushWait val -> return (cpu {currentInstruction=Just (RST SubOpPushHigh val)}, 1)
    RST SubOpPushHigh val -> do
      cpu' <- push8High cpu.registers.rPC cpu
      return (cpu'{currentInstruction=Just (RST SubOpPushLow val)}, 1)
    RST SubOpPushLow val -> do
      cpu' <- push8Low cpu.registers.rPC cpu
      return (cpu'{registers=cpu'.registers{rPC=val}}, 1)
    ADD_SP_imm8 i -> do
      let (res, c, h) = addSignedToSP (readR16 regs SP) i
          regs' =
            writeR16 regs SP res
              & setZflag False
              & setNflag False
              & setHflag h
              & setCflag c
      return (cpu {registers = regs'}, 2)
    ADD_HL_r16 r16 -> do
      let hl = readR16 regs HL
          val = readR16 regs r16
          res = hl + val
          h = ((hl .&. 0x0FFF) + (val .&. 0x0FFF)) > 0x0FFF
          c = ((fromIntegral hl :: Int) + (fromIntegral val :: Int)) > 0xFFFF
          regs' =
            writeR16 regs HL res
              & setNflag False
              & setHflag h
              & setCflag c
      return (cpu {registers = regs'}, 1)
    DAA -> do
      val <- readR8 regs bus A
      let subtracting = nflag regs
          carry = cflag regs || (not subtracting && val > 0x99)
          lowAdjust = if hflag regs || (not subtracting && (val .&. 0x0F) > 9)
                        then 0x06 else 0
          highAdjust = if carry then 0x60 else 0
          adjust = lowAdjust + highAdjust
          res = if subtracting then val - adjust else val + adjust
      regs' <-
        writeR8
          ( regs
              & setZflag (res == 0)
              & setHflag False
              & setCflag carry
          )
          bus
          A
          res
      return (cpu {registers = regs'}, 0)
    CCF -> return
        ( cpu
            { registers =
                regs
                  & setNflag False
                  & setHflag False
                  & setCflag (not (cflag regs))
            },
          0
        )
    INVALID _ -> return (cpu {currentInstruction=Just op}, 0)
    RRCA -> do
      val <- readR8 regs bus A
      let newc = (val .&. 0x01) /= 0
          val' = val .>>. 1 .|. (val .<<. 7)
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
    SCF -> return
        ( cpu
            { registers =
                regs
                  & setNflag False
                  & setHflag False
                  & setCflag True
            },
          0
        )
    RRA -> do
      val <- readR8 regs bus A
      let c = cflag regs
          newc = (val .&. 0x01) /= 0
          val' = val .>>. 1 .|. (if c then 0x80 else 0)
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
