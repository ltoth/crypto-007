import Data.Char
import Data.Hex
import Data.Bits

main = putStrLn $ encrypt k "attack at dusk"

k = encrypt "09e1c5f70a65ac519458e7e53f36" "attack at dawn"

encrypt :: String -> String -> String
encrypt k m = map toLower $ hex $ map chr $ encrypt' (map ord m) (map ord $ unhex' k)

encrypt' :: [Int] -> [Int] -> [Int]
encrypt' k m = zipWith xor m k

unhex' :: (Monad m, Hex (m b)) => m b -> m b
unhex' h = do
    x <- unhex h
    x
