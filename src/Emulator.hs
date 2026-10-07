module Emulator (
  Emulator,
  powerOn,
  advanceFrame,
  setJoypadKey,
  trySetJoypadKey,
  runInBackground,
  EmulatorWorker,
  stopWorker,
  nextWorkerError,
  nextDisplay,
  ) where

import qualified CPU
import Data.ByteString.Lazy (ByteString)
import qualified PPU
import Bus (initBus, JoypadKey, setJoypad)
import CPU (initCPU, CPU)
import PPU (initPPU, PPU, Display)
import Control.Concurrent.STM (TMVar, atomically, tryPutTMVar, tryTakeTMVar, newEmptyTMVarIO)
import Control.Concurrent.STM.TBQueue (TBQueue, isFullTBQueue, newTBQueue, tryReadTBQueue, writeTBQueue)
import Control.Concurrent (MVar, ThreadId, forkIOWithUnmask, killThread, newEmptyMVar, putMVar, readMVar)
import Control.Exception (AsyncException (ThreadKilled), SomeException, finally, fromException, mask_, try)
import Control.Monad (void, when)
import APU (APU(..), initAPU, MixedSamples)
import qualified Data.Vector.Unboxed as V
import qualified APU

data Emulator = Emulator
  { cpu :: !CPU
  , ppu :: !PPU
  , apu :: !APU
  , inputQueue  :: TBQueue KeyEvent
  , outputQueue :: TBQueue Display
  , outputAudioQueue :: TBQueue MixedSamples
  , workerError :: TMVar SomeException
  }

data EmulatorWorker = EmulatorWorker ThreadId (MVar ())

data KeyEvent = KeyEvent JoypadKey Bool -- True = pressed

powerOn :: ByteString -> ByteString -> IO Emulator
powerOn boot cartridge = do
  bus <- initBus boot cartridge
  let cpu = initCPU bus
  ppu <- initPPU cpu.bus
  inputQueue <- atomically $ newTBQueue 2
  outputQueue <- atomically $ newTBQueue 2
  outputAudioQueue <- atomically $ newTBQueue 2
  let apu = initAPU cpu.bus
  Emulator cpu ppu apu inputQueue outputQueue outputAudioQueue <$> newEmptyTMVarIO


runInBackground :: Emulator -> IO EmulatorWorker
runInBackground emulator = mask_ $ do
  finished <- newEmptyMVar
  thread <- forkIOWithUnmask $ \unmask ->
    (try (unmask $ go emulator) >>= reportResult)
      `finally` putMVar finished ()
  pure $ EmulatorWorker thread finished
  where
    go em = advanceFrame em >>= go

    reportResult :: Either SomeException () -> IO ()
    reportResult (Right ()) = pure ()
    reportResult (Left err) = case fromException err of
      Just ThreadKilled -> pure ()
      _ -> void $ atomically $ tryPutTMVar emulator.workerError err

stopWorker :: EmulatorWorker -> IO ()
stopWorker (EmulatorWorker thread finished) = mask_ $ do
  killThread thread
  readMVar finished

nextWorkerError :: Emulator -> IO (Maybe SomeException)
nextWorkerError emulator = atomically $ tryTakeTMVar emulator.workerError

-- Keep CPU and PPU in sync using the same scheduler as the boot test.
-- Carry the final instruction's overshoot into the next frame.
advanceFrame :: Emulator -> IO Emulator
advanceFrame emulator = do
    (cpu', elapsed) <- CPU.execute emulator.cpu
    let vblank = PPU.isVBlankMode emulator.ppu
    ppu' <- PPU.execute  (elapsed * 4) emulator.ppu

    when (not vblank && PPU.isVBlankMode ppu') $ presentDisplay emulator{ppu=ppu'}
    consumeEvent emulator

    apu1 <- APU.execute (elapsed * 4) emulator.apu
    apu2 <- sendAudioSamples emulator apu1

    let emulator' = emulator{cpu=cpu', ppu=ppu', apu=apu2}
    if vblank && not (PPU.isVBlankMode ppu') then
      return emulator'
    else
      advanceFrame emulator'
  where
    sendAudioSamples :: Emulator -> APU -> IO APU
    sendAudioSamples em apu = do
      -- if V.length apu.buffer > 600 then do
      --   atomically $ writeTBQueue em.outputAudioQueue apu.buffer
      --   return apu{buffer=mempty}
      -- else return apu
      return apu

    presentDisplay :: Emulator -> IO ()
    presentDisplay em = do
      frame <- PPU.snapshotDisplay em.ppu
      atomically $ writeTBQueue em.outputQueue frame

    consumeEvent :: Emulator -> IO ()
    consumeEvent em = do
      event <- atomically $ tryReadTBQueue em.inputQueue
      case event of
        Just (KeyEvent key press) -> setJoypad key press em.cpu.bus
        _ -> return ()

setJoypadKey :: JoypadKey -> Bool -> Emulator -> IO ()
setJoypadKey key press emulator =
  atomically $ writeTBQueue emulator.inputQueue (KeyEvent key press)

trySetJoypadKey :: JoypadKey -> Bool -> Emulator -> IO Bool
trySetJoypadKey key press emulator = atomically $ do
  full <- isFullTBQueue emulator.inputQueue
  if full then pure False else do
    writeTBQueue emulator.inputQueue (KeyEvent key press)
    pure True

nextDisplay :: Emulator -> IO (Maybe Display)
nextDisplay emulator = do
  atomically $ tryReadTBQueue emulator.outputQueue
