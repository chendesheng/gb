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
    SubOp (..),
  )
where

import Data.Binary hiding (decodeFile)
import Data.Binary.Get
import Data.Int (Int8)
import Dbg
import Numeric (showHex)
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
  | RES SubOp Word8 R8
  | SET SubOp Word8 R8
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

data SubOp = SubOpRead | SubOpWrite Word8
  deriving (Show, Eq)

data OpCode
  = ADD_HL_r16 R16
  | ADD_SP_imm8 Int8
  | ALU_A_R8 ALUOp R8
  | ALU_A_imm8 ALUOp Word8
  | CALL_cond_imm16 Cond Word16
  | CALL_addr16 Word16
  | CCF
  | CPL
  | DAA
  | DEC_r16 R16
  | DEC_r8 SubOp R8
  | DI
  | EI
  | HALT
  | INC_r16 R16
  | INC_r8 SubOp R8
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
  | POP R16Stk
  | PREFIX_CB CBOp
  | PUSH R16Stk
  | RET
  | RETI
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
  case b of
    0x0b -> return $ RRC E
    0x4f -> return $ BIT 1 A
    0x7c -> return $ BIT 7 H
    0x11 -> return $ RL C
    _ -> todo $ "unknown cb opcode: 0x" ++ showHex b ""

opCodeDecoder :: Get OpCode
opCodeDecoder = do
  b <- getWord8
  case b of
    0 -> return NOP
    0x01 -> LD_r16_imm16 BC <$> getWord16le
    0x03 -> return $ INC_r16 BC
    0x04 -> return $ INC_r8 SubOpRead B
    0x05 -> return $ DEC_r8 SubOpRead B
    0x06 -> LD_r8_imm8 B <$> getWord8
    0x08 -> LD_imm16_SP <$> getWord16le
    0x0b -> return $ DEC_r16 BC
    0x0c -> return $ INC_r8 SubOpRead C
    0x0d -> return $ DEC_r8 SubOpRead C
    0x0e -> LD_r8_imm8 C <$> getWord8
    0x11 -> LD_r16_imm16 DE <$> getWord16le
    0x13 -> return $ INC_r16 DE
    0x15 -> return $ DEC_r8 SubOpRead D
    0x16 -> LD_r8_imm8 D <$> getWord8
    0x1a -> return $ LD_A_AtR16mem DEm
    0x17 -> return RLA
    0x18 -> JR_imm8 <$> getInt8
    0x1d -> return $ DEC_r8 SubOpRead E
    0x1e -> LD_r8_imm8 E <$> getWord8
    0x20 -> JR_cond_imm8 NZ <$> getInt8
    0x22 -> return $ LD_AtR16mem_A HLi
    0x23 -> return $ INC_r16 HL
    0x24 -> return $ INC_r8 SubOpRead H
    0x26 -> LD_r8_imm8 H <$> getWord8
    0x28 -> JR_cond_imm8 Z <$> getInt8
    0x2e -> LD_r8_imm8 L <$> getWord8
    0x2f -> return CPL
    0x31 -> LD_r16_imm16 SP <$> getWord16le
    0x32 -> return $ LD_AtR16mem_A HLd
    0x35 -> return $ DEC_r8 SubOpRead AtHL
    0x3c -> return $ INC_r8 SubOpRead A
    0x3d -> return $ DEC_r8 SubOpRead A
    0x3e -> LD_r8_imm8 A <$> getWord8
    0x47 -> return $ LD_r8_r8 B A
    0x4f -> return $ LD_r8_r8 C A
    0x57 -> return $ LD_r8_r8 D A
    0x66 -> return $ LD_r8_r8 H AtHL
    0x67 -> return $ LD_r8_r8 H A
    0x73 -> return $ LD_r8_r8 AtHL E
    0x77 -> return $ LD_r8_r8 AtHL A
    0x78 -> return $ LD_r8_r8 A B
    0x7b -> return $ LD_r8_r8 A E
    0x7c -> return $ LD_r8_r8 A H
    0x7d -> return $ LD_r8_r8 A L
    0x83 -> return $ ALU_A_R8 ADD E
    0x86 -> return $ ALU_A_R8 ADD AtHL
    0x88 -> return $ ALU_A_R8 ADC B
    0x89 -> return $ ALU_A_R8 ADC C
    0x90 -> return $ ALU_A_R8 SUB B
    0xaf -> return $ ALU_A_R8 XOR A
    0xbe -> return $ ALU_A_R8 CP AtHL
    0xc1 -> return $ POP BCstk
    0xc5 -> return $ PUSH BCstk
    0xc9 -> return RET
    0xcb -> PREFIX_CB <$> cbOpDecoder
    0xce -> ALU_A_imm8 ADC <$> getWord8
    0xcc -> CALL_cond_imm16 Z <$> getWord16le
    0xcd -> CALL_addr16 <$> getWord16le
    0xe0 -> LDH_AtImm8_A <$> getWord8
    0xea -> LD_Addr16_A <$> getWord16le
    0xe2 -> return LDH_AtC_A
    0xf0 -> LDH_A_AtImm8 <$> getWord8
    0xfe -> ALU_A_imm8 CP <$> getWord8
    0x21 -> LD_r16_imm16 HL <$> getWord16le
    0xdd -> return $ INVALID b
    0xFB -> return EI
    0xF3 -> return DI
    0xD9 -> return RETI
    _ -> return $ INVALID b

-- _ -> todo $ "unknown opcode: 0x" ++ showHex b ""

instructionDecoder :: Get Instruction
instructionDecoder = do
  start <- bytesRead
  opcodes <- opCodeDecoder
  end <- bytesRead
  return $ Instruction opcodes (fromIntegral $ end - start)
