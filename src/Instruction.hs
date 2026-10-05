{- HLINT ignore "Use camelCase" -}

module Instruction
  ( Instruction (..),
    OpCode (..),
    instructionDecoder,
    R16 (..),
    R8 (..),
    R16Stk (..),
    R16Mem (..),
    ALUOp (..),
    CBOp (..),
    SubOpMem (..),
    SubOpCall (..),
    SubOpPop (..),
    SubOpPush (..),
  )
where

import Data.Binary hiding (decodeFile)
import Data.Binary.Get
import Data.Bits ((.&.), shiftR)
import Data.Int (Int8)
import Registers

data ALUOp = ADD | ADC | SUB | SBC | AND | XOR | OR | CP
  deriving (Show, Eq)

data CBOp
  = RLC R8
  | RRC R8
  | RL R8
  | RR R8
  | SLA R8
  | SRA R8
  | SWAP R8
  | SRL R8
  | BIT Word8 R8
  | RES SubOpMem Word8 R8
  | SET SubOpMem Word8 R8
  deriving (Show, Eq)

data RstTarget
  = RST00
  | RST08
  | RST10
  | RST18
  | RST20
  | RST28
  | RST30
  | RST38
  deriving (Show, Eq)

data SubOpMem = SubOpRead | SubOpWrite Word8
  deriving (Show, Eq)

data SubOpCall = SubOpCallWait | SubOpCallPushHigh | SubOpCallJmp deriving (Show, Eq)
data SubOpPop = SubOpPopLow | SubOpPopHigh Word8 deriving (Show, Eq)
data SubOpPush = SubOpPushWait | SubOpPushHigh | SubOpPushLow deriving (Show, Eq)

data OpCode
  = ADD_HL_r16 R16
  | ADD_SP_imm8 Int8
  | ALU_A_R8 ALUOp R8
  | ALU_A_imm8 ALUOp Word8
  | CALL_cond_imm16 Cond Word16
  | CALL_addr16 SubOpCall Word16
  | CCF
  | CPL
  | DAA
  | DEC_r16 R16
  | DEC_r8 SubOpMem R8
  | DI
  | EI
  | HALT
  | INC_r16 R16
  | INC_r8 SubOpMem R8
  | INVALID Word8
  | JP_HL
  | JP_cond_imm16 Cond Word16
  | JP_imm16 Word16
  | JR_cond_imm8 Cond Int8
  | JR_imm8 Int8
  | LDH_A_C
  | LDH_A_AtImm8 Word8
  | LDH_AtC_A
  | LDH_AtImm8_A Word8
  | LD_A_imm16 Word16
  | LD_A_AtR16mem R16Mem
  | LD_HL_SP_plus_imm8 Int8
  | LD_SP_HL
  | LD_Addr16_A Word16
  | LD_imm16_SP Word16
  | LD_r16_imm16 R16 Word16
  | LD_AtR16mem_A R16Mem
  | LD_r8_imm8 R8 Word8
  | LD_r8_r8 R8 R8
  | NOP
  | POP SubOpPop R16Stk
  | PREFIX_CB CBOp
  | PUSH SubOpPush R16Stk
  | RET SubOpPop
  | RETI SubOpPop
  | RET_cond Cond
  | RLA
  | RLCA
  | RRA
  | RRCA
  | RST Word16
  | SCF
  | STOP
  deriving (Show, Eq)

data Instruction = Instruction
  { op :: OpCode,
    len :: Word8
  }
  deriving (Show, Eq)

cbOpDecoder :: Get CBOp
cbOpDecoder = do
  b <- getWord8
  let r8 = decodeR8 b
      bitIndex = (b `shiftR` 3) .&. 0x07
  return $ case b `shiftR` 6 of
    0 -> case bitIndex of
      0 -> RLC r8
      1 -> RRC r8
      2 -> RL r8
      3 -> RR r8
      4 -> SLA r8
      5 -> SRA r8
      6 -> SWAP r8
      _ -> SRL r8
    1 -> BIT bitIndex r8
    2 -> RES SubOpRead bitIndex r8
    _ -> SET SubOpRead bitIndex r8

-- The low three opcode bits select B, C, D, E, H, L, [HL], or A.
decodeR8 :: Word8 -> R8
decodeR8 b = toEnum (fromIntegral (b .&. 0x07))

decodeALU :: Word8 -> ALUOp
decodeALU b = case (b `shiftR` 3) .&. 0x07 of
  0 -> ADD
  1 -> ADC
  2 -> SUB
  3 -> SBC
  4 -> AND
  5 -> XOR
  6 -> OR
  _ -> CP

opCodeDecoder :: Get OpCode
opCodeDecoder = do
  b <- getWord8
  case b of
    _ | b >= 0x40 && b <= 0x7F ->
      return $ if b == 0x76 then HALT
        else LD_r8_r8 (decodeR8 (b `shiftR` 3)) (decodeR8 b)
    _ | b >= 0x80 && b <= 0xBF ->
      return $ ALU_A_R8 (decodeALU b) (decodeR8 b)
    0x00 -> return NOP
    0x01 -> LD_r16_imm16 BC <$> getWord16le
    0x02 -> return $ LD_AtR16mem_A BCm
    0x03 -> return $ INC_r16 BC
    0x04 -> return $ INC_r8 SubOpRead B
    0x05 -> return $ DEC_r8 SubOpRead B
    0x06 -> LD_r8_imm8 B <$> getWord8
    0x07 -> return RLCA
    0x08 -> LD_imm16_SP <$> getWord16le
    0x09 -> return $ ADD_HL_r16 BC
    0x0a -> return $ LD_A_AtR16mem BCm
    0x0b -> return $ DEC_r16 BC
    0x0c -> return $ INC_r8 SubOpRead C
    0x0d -> return $ DEC_r8 SubOpRead C
    0x0e -> LD_r8_imm8 C <$> getWord8
    0x0f -> return RRCA
    0x10 -> getWord8 >> return STOP
    0x11 -> LD_r16_imm16 DE <$> getWord16le
    0x12 -> return $ LD_AtR16mem_A DEm
    0x13 -> return $ INC_r16 DE
    0x14 -> return $ INC_r8 SubOpRead D
    0x15 -> return $ DEC_r8 SubOpRead D
    0x16 -> LD_r8_imm8 D <$> getWord8
    0x17 -> return RLA
    0x18 -> JR_imm8 <$> getInt8
    0x19 -> return $ ADD_HL_r16 DE
    0x1a -> return $ LD_A_AtR16mem DEm
    0x1b -> return $ DEC_r16 DE
    0x1c -> return $ INC_r8 SubOpRead E
    0x1d -> return $ DEC_r8 SubOpRead E
    0x1e -> LD_r8_imm8 E <$> getWord8
    0x1f -> return RRA
    0x20 -> JR_cond_imm8 NZ <$> getInt8
    0x21 -> LD_r16_imm16 HL <$> getWord16le
    0x22 -> return $ LD_AtR16mem_A HLi
    0x23 -> return $ INC_r16 HL
    0x24 -> return $ INC_r8 SubOpRead H
    0x25 -> return $ DEC_r8 SubOpRead H
    0x26 -> LD_r8_imm8 H <$> getWord8
    0x27 -> return DAA
    0x28 -> JR_cond_imm8 Z <$> getInt8
    0x29 -> return $ ADD_HL_r16 HL
    0x2a -> return $ LD_A_AtR16mem HLi
    0x2b -> return $ DEC_r16 HL
    0x2c -> return $ INC_r8 SubOpRead L
    0x2d -> return $ DEC_r8 SubOpRead L
    0x2e -> LD_r8_imm8 L <$> getWord8
    0x2f -> return CPL
    0x30 -> JR_cond_imm8 NC <$> getInt8
    0x31 -> LD_r16_imm16 SP <$> getWord16le
    0x32 -> return $ LD_AtR16mem_A HLd
    0x33 -> return $ INC_r16 SP
    0x34 -> return $ INC_r8 SubOpRead AtHL
    0x35 -> return $ DEC_r8 SubOpRead AtHL
    0x36 -> LD_r8_imm8 AtHL <$> getWord8
    0x37 -> return SCF
    0x38 -> JR_cond_imm8 Cc <$> getInt8
    0x39 -> return $ ADD_HL_r16 SP
    0x3a -> return $ LD_A_AtR16mem HLd
    0x3b -> return $ DEC_r16 SP
    0x3c -> return $ INC_r8 SubOpRead A
    0x3d -> return $ DEC_r8 SubOpRead A
    0x3e -> LD_r8_imm8 A <$> getWord8
    0x3f -> return CCF
    0xc0 -> return $ RET_cond NZ
    0xc1 -> return $ POP SubOpPopLow BCstk
    0xc2 -> JP_cond_imm16 NZ <$> getWord16le
    0xc3 -> JP_imm16 <$> getWord16le
    0xc4 -> CALL_cond_imm16 NZ <$> getWord16le
    0xc5 -> return $ PUSH SubOpPushWait BCstk
    0xc6 -> ALU_A_imm8 ADD <$> getWord8
    0xc7 -> return $ RST 0x00
    0xc8 -> return $ RET_cond Z
    0xc9 -> return $ RET SubOpPopLow
    0xca -> JP_cond_imm16 Z <$> getWord16le
    0xcb -> PREFIX_CB <$> cbOpDecoder
    0xcc -> CALL_cond_imm16 Z <$> getWord16le
    0xcd -> CALL_addr16 SubOpCallWait <$> getWord16le
    0xce -> ALU_A_imm8 ADC <$> getWord8
    0xcf -> return $ RST 0x08
    0xd0 -> return $ RET_cond NC
    0xd1 -> return $ POP SubOpPopLow DEstk
    0xd2 -> JP_cond_imm16 NC <$> getWord16le
    0xd4 -> CALL_cond_imm16 NC <$> getWord16le
    0xd5 -> return $ PUSH SubOpPushWait DEstk
    0xd6 -> ALU_A_imm8 SUB <$> getWord8
    0xd7 -> return $ RST 0x10
    0xd8 -> return $ RET_cond Cc
    0xd9 -> return $ RETI SubOpPopLow
    0xda -> JP_cond_imm16 Cc <$> getWord16le
    0xdc -> CALL_cond_imm16 Cc <$> getWord16le
    0xde -> ALU_A_imm8 SBC <$> getWord8
    0xdf -> return $ RST 0x18
    0xe0 -> LDH_AtImm8_A <$> getWord8
    0xe1 -> return $ POP SubOpPopLow HLstk
    0xe2 -> return LDH_AtC_A
    0xe5 -> return $ PUSH SubOpPushWait HLstk
    0xe6 -> ALU_A_imm8 AND <$> getWord8
    0xe7 -> return $ RST 0x20
    0xe8 -> ADD_SP_imm8 <$> getInt8
    0xe9 -> return JP_HL
    0xea -> LD_Addr16_A <$> getWord16le
    0xee -> ALU_A_imm8 XOR <$> getWord8
    0xef -> return $ RST 0x28
    0xf0 -> LDH_A_AtImm8 <$> getWord8
    0xf1 -> return $ POP SubOpPopLow AFstk
    0xf2 -> return LDH_A_C
    0xf3 -> return DI
    0xf5 -> return $ PUSH SubOpPushWait AFstk
    0xf6 -> ALU_A_imm8 OR <$> getWord8
    0xf7 -> return $ RST 0x30
    0xf8 -> LD_HL_SP_plus_imm8 <$> getInt8
    0xf9 -> return LD_SP_HL
    0xfa -> LD_A_imm16 <$> getWord16le
    0xfb -> return EI
    0xfe -> ALU_A_imm8 CP <$> getWord8
    0xff -> return $ RST 0x38
    _ -> return $ INVALID b

instructionDecoder :: Get Instruction
instructionDecoder = do
  start <- bytesRead
  opcodes <- opCodeDecoder
  end <- bytesRead
  return $ Instruction opcodes (fromIntegral $ end - start)
