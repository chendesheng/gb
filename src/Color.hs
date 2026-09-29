module Color
  (
    ColorIndex(..),
    Color(..),
    ColorPalette,
    getColor,
  )
where

import Data.Bits (shiftR, (.&.))
import Data.Word

data ColorIndex = ID0 | ID1 | ID2 | ID3
data Color = Blank | LightGray | DarkGray | Black deriving (Enum, Show)

type ColorPalette = Word8

getColor :: ColorIndex -> ColorPalette -> Color
getColor ID0 palette = toEnum $ fromIntegral palette .&. 0x03
getColor ID1 palette = toEnum $ (fromIntegral palette `shiftR` 2) .&. 0x03
getColor ID2 palette = toEnum $ (fromIntegral palette `shiftR` 4) .&. 0x03
getColor ID3 palette = toEnum $ (fromIntegral palette `shiftR` 6) .&. 0x03
  
