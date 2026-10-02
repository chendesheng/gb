{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module NativeMenu (MenuAction (..), installMenu, pollMenuAction, setPowerState, showError) where

import Control.Exception (bracket)
import Foreign.C.Types (CInt)
import Foreign.Marshal.Alloc (free)
import Foreign.Ptr (nullPtr)
import qualified GHC.Foreign as UTF8
import GHC.IO.Encoding (utf8)
import qualified Language.C.Inline.ObjC as C

C.context C.objcCtx
C.include "NativeMenu.h"

data MenuAction = OpenCartridge FilePath | PowerOn | PowerOff

-- All AppKit calls run on raylib's main OS thread.
installMenu :: IO ()
installMenu = [C.block| void { gbInstallMenu(); } |]

pollMenuAction :: IO (Maybe MenuAction)
pollMenuAction = do
  event <- [C.exp| char * { gbTakeAction() } |]
  if event == nullPtr then pure Nothing else
    bracket (pure event) free $ \pointer -> do
      message <- UTF8.peekCString utf8 pointer
      pure $ case message of
        'O' : path -> Just (OpenCartridge path)
        "P" -> Just PowerOn
        "F" -> Just PowerOff
        _ -> Nothing

setPowerState :: Bool -> Bool -> IO ()
setPowerState loaded powered = do
  let hasCartridge = fromIntegral (fromEnum loaded) :: CInt
      isPowered = fromIntegral (fromEnum powered) :: CInt
  [C.block| void { gbSetPowerState($(int hasCartridge), $(int isPowered)); } |]

showError :: String -> IO ()
showError message = UTF8.withCString utf8 message $ \text ->
  [C.block| void { gbShowError($(char *text)); } |]
