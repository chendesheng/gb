module Color
  (
    ColorIndex,
    Color(..),
    ColorPalette,
    getColor,
    TileRow,
    tileRowColorIndexes,
  )
where

import Data.Bits ((.&.), (.|.), (.>>.), testBit)
import Data.Word

data ColorIndex = ID0 | ID1 | ID2 | ID3 deriving (Enum, Show)
data Color = Blank | LightGray | DarkGray | Black deriving (Enum, Show)

type ColorPalette = Word8

getColor :: ColorIndex -> ColorPalette -> Color
getColor ID0 palette = toEnum $ fromIntegral palette .&. 0x03
getColor ID1 palette = toEnum $ (fromIntegral palette .>>. 2) .&. 0x03
getColor ID2 palette = toEnum $ (fromIntegral palette .>>. 4) .&. 0x03
getColor ID3 palette = toEnum $ (fromIntegral palette .>>. 6) .&. 0x03

type TileRow = (Word8, Word8)

tileRowColorIndexes :: TileRow -> [ColorIndex]
tileRowColorIndexes (low, high) =
  let ls = [ if testBit low i then 1 else 0 | i <- [7,6..0] ]
      hs = [ if testBit high i then 2 else 0 | i <- [7,6..0] ]
  in
    zipWith (\l h -> toEnum $ l .|. h) ls hs
    
