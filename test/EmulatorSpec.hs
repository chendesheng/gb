module EmulatorSpec (spec) where

import Bus (JoypadKey (AKey))
import Control.Concurrent (threadDelay)
import Control.Exception (SomeException, bracket, mask_)
import Control.Monad (foldM, replicateM_, unless)
import qualified Data.ByteString.Lazy as BL
import Data.Maybe (fromMaybe, isJust, isNothing)
import Emulator
import System.Directory (doesFileExist)
import System.Environment (lookupEnv)
import System.Timeout (timeout)
import Test.Hspec

spec :: Spec
spec = describe "Emulator worker" $ do
  it "runs the Tetris cartridge without an execution error" $ do
    -- Use a local cartridge rather than including the commercial ROM in the repo.
    romPath <- fromMaybe "./test/fixtures/tetris.gb" <$> lookupEnv "TETRIS_ROM"
    exists <- doesFileExist romPath
    unless exists $ pendingWith "Set TETRIS_ROM to a local Tetris (JUE) (V1.1).gb file"
    boot <- BL.readFile "resources/dmg.bin"
    rom <- BL.readFile romPath
    BL.length rom `shouldBe` 0x8000
    machine <- powerOn boot rom
    result <- timeout (30 * 1000000) $ foldM (\current _ -> do
      next <- advanceFrame current
      -- Drain every frame so the bounded output queue cannot stall execution.
      nextDisplay next >>= (`shouldSatisfy` isJust)
      pure next
      ) machine [1 .. 1200 :: Int]
    case result of
      Nothing -> expectationFailure "Tetris exceeded the 30-second timeout"
      Just _ -> pure ()

  it "does not block the UI when the input queue is full" $ do
    machine <- powerOn BL.empty BL.empty
    trySetJoypadKey AKey True machine `shouldReturn` True
    trySetJoypadKey AKey False machine `shouldReturn` True
    timeout 2000000 (trySetJoypadKey AKey True machine) `shouldReturn` Just False

  it "stops a running worker started with exceptions masked without reporting a failure" $ do
    machine <- powerOn (BL.pack $ [0x18, 0xFE] ++ replicate 254 0) BL.empty
    bracket (mask_ $ runInBackground machine) stopWorker $ \worker -> do
      -- A third event waits for the worker to consume the two queued events.
      started <- timeout 2000000 $ replicateM_ 3 $ setJoypadKey AKey True machine
      started `shouldBe` Just ()
      timeout 2000000 (stopWorker worker) `shouldReturn` Just ()
      nextWorkerError machine >>= (`shouldSatisfy` isNothing)

  it "stops a worker with a full display queue" $ do
    -- Enable LCD, then keep executing JR -2.
    machine <- powerOn (BL.pack $ [0x3E, 0x91, 0xE0, 0x40, 0x18, 0xFE] ++ replicate 250 0) BL.empty
    machine1 <- advanceFrame machine
    machine2 <- advanceFrame machine1
    -- Both queue slots now hold frames; publishing another frame must wait.
    bracket (runInBackground machine2) stopWorker $ \worker -> do
      threadDelay 100000
      timeout 2000000 (stopWorker worker) `shouldReturn` Just ()
      nextWorkerError machine2 >>= (`shouldSatisfy` isNothing)

  it "reports an execution failure once and allows the finished worker to be stopped" $ do
    -- Empty boot ROM fails on the worker's first instruction fetch.
    machine <- powerOn BL.empty BL.empty
    bracket (runInBackground machine) stopWorker $ \worker -> do
      failure <- timeout 2000000 $ waitForFailure machine
      failure `shouldSatisfy` isJust
      nextWorkerError machine >>= (`shouldSatisfy` isNothing)
      timeout 2000000 (stopWorker worker) `shouldReturn` Just ()

waitForFailure :: Emulator -> IO SomeException
waitForFailure machine = do
  failure <- nextWorkerError machine
  case failure of
    Just err -> pure err
    Nothing -> threadDelay 1000 >> waitForFailure machine
