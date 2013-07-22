import Data.Char
import Data.Hex
import Data.Bits
import Control.Monad

-- main = putStrLn $ encrypt k "attack at dusk"

xorStrings :: String -> String -> String
xorStrings x y = encrypt x (join $ unhex y)

encrypt :: String -> String -> String
encrypt k m = map toLower $ hex $ map chr $ encrypt' (map ord $ join $ unhex k) (map ord m)

encrypt' :: [Int] -> [Int] -> [Int]
encrypt' k m = zipWith xor m k
