module APU (APU(buffer), initAPU, execute, MixedSamples) where

import Bus (Bus)
import qualified Bus
import Data.Word
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Bits (testBit, (.&.), (.>>.), (.|.), (.<<.))
import Dbg
import Control.Monad (foldM)

type MixedSamples = Vector Float

data APU = APU
  { bus :: Bus
  , counter :: Int
  , buffer :: MixedSamples
  , channel2 :: Maybe Channel2
  , acc :: Word32
  }

data Channel2 = Channel2
  { waveDuty :: Word8
  , wavePos :: Word32 -- reset every 8
  , lengthTimer :: Word8
  , volume :: Word8
  , peroidValue :: Word16
  }

initAPU :: Bus -> APU
initAPU bus = APU{bus=bus, counter=0, buffer=mempty, channel2=Nothing, acc=0}

isAudioOn :: Word8 -> Bool
isAudioOn = flip testBit 7

isChannel2On :: Word8 -> Bool
isChannel2On = flip testBit 1

samplesPerSecond :: Word32
samplesPerSecond = 48000

dotsPerSecond :: Word32
dotsPerSecond = 0x400000

parseAudLen :: Word8 -> (Word8, Word8)
parseAudLen val =
  ((val .&. 0xC0) .>>. 6, val .&. 0x3F)

parsePeriodValue :: Word8 -> Word8 -> Word16
parsePeriodValue low high =
  ((fromIntegral high .&. 0x07) .<<. 8) .|. fromIntegral low

parseVolume :: Word8 -> Word8
parseVolume val = (val .&. 0xF0) .>>. 4

execute :: Word8 -> APU -> IO APU
execute elapsed apu@APU{bus} = do
  ena <- Bus.readNR52 bus
  if isAudioOn ena  then do
    if isChannel2On ena then
      foldM (\apu1 _ -> advanceDot apu1) apu [1 .. elapsed]
    else return apu
  else return apu
  where
    advanceDot apu1 =
      let apu2 = apu1{counter=apu1.counter+1}
      in
      case apu2.channel2 of
        Nothing -> do
          (waveDuty, initialLen) <- parseAudLen <$> Bus.readNR21 bus
          volume <- parseVolume <$> Bus.readNR22 bus
          periodValue <- parsePeriodValue <$> Bus.readNR23 bus <*> Bus.readNR24 bus
          return apu2{channel2=Just $ Channel2 waveDuty 0 initialLen volume periodValue}
        Just chan -> do
          let timer = if apu2.counter `mod` 16384 == 0 then chan.lengthTimer+1
                      else chan.lengthTimer
          let apu3 = generateChannel2Sample apu2 chan
          return apu3{channel2=if timer == 64 then Nothing
                               else fmap (\ch -> ch{lengthTimer=timer}) apu3.channel2}

generateChannel2Sample :: APU -> Channel2 -> APU
generateChannel2Sample apu chan =
  let acc = apu.acc + samplesPerSecond
      wavePos = chan.wavePos+1
  in
    if acc >= dotsPerSecond then
      let period = 2048 - fromIntegral chan.peroidValue :: Word32
          dutyPatterns =
            [ [False, False, False, True, False, False, False, False]
            , [False, False, False, True, True, False, False, False]
            , [False, False, True, True, True, True, False, False]
            , [True, False, False, True, True, True, True, False]
            ]
          sample = if dutyPatterns !! fromIntegral chan.waveDuty !! fromIntegral ((wavePos `div` period) `mod` 8) then
                      fromIntegral chan.volume / 15.0
                   else 0.0 :: Float
      in apu{buffer = apu.buffer V.++ V.singleton sample
            , acc=acc `mod` dotsPerSecond
            , channel2=Just chan{wavePos=wavePos}
            }
    else
      apu{acc=acc, channel2=Just chan{wavePos=wavePos}}
