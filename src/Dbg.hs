{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Dbg where

import Data.Word
import Debug.Trace (trace)
import Numeric (showHex)

class EchoShow a where
  echoShow :: a -> String

instance EchoShow Word8 where
  echoShow x = "0x" <> showHex x ""

instance EchoShow Word16 where
  echoShow x = "0x" <> showHex x ""

instance EchoShow Word32 where
  echoShow x = "0x" <> showHex x ""

instance EchoShow Word64 where
  echoShow x = "0x" <> showHex x ""

-- fallback for normal Show types

instance {-# OVERLAPPABLE #-} (Show a) => EchoShow a where
  echoShow = show

echo :: (EchoShow a) => String -> a -> a
echo prefix x =
  trace (prefix <> echoShow x) x

todo :: String -> a
todo s = error $ "TODO: " ++ s
