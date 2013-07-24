module XorUtils
( xorHex
, xorInt
, hexToInt
, intToHex
) where

import Data.Char
import Data.Hex
import Data.Bits
import Control.Monad

xorHex :: String -> String -> String
xorHex x y = encrypt x (join $ unhex y)

xorInt :: [Int] -> [Int] -> [Int]
xorInt k m = zipWith xor m k

hexToInt :: String -> [Int]
hexToInt = map ord . join . unhex

intToHex :: [Int] -> String
intToHex = map toLower . hex . map chr

encrypt :: String -> String -> String
encrypt k m = map toLower $ hex $ map chr $ xorInt (map ord $ join $ unhex k) (map ord m)
