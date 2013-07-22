{-# LANGUAGE ViewPatterns #-}  
import Data.Char
import Data.Hex
import System.Environment
import Crypto.Hash.SHA256
import qualified Data.ByteString.Char8 as S

blockSize :: Int
blockSize = 1024

main :: IO ()
main = do
  (fileName:_) <- getArgs
  h0 <- recursiveHash fileName
  putStrLn $ toHex h0

recursiveHash :: FilePath -> IO S.ByteString
recursiveHash f = do
  contents <- S.readFile f
  return $ recursiveHash' contents

recursiveHash' :: S.ByteString -> S.ByteString
recursiveHash' c =
  let bs = splitEvery blockSize c
  in  foldr (\b a -> hash $ S.append b a) S.empty bs
    
toHex :: S.ByteString -> String
toHex b = map toLower $ toHex' b
  where toHex' = hex . S.unpack

-- based on chunk :: Int -> [a] -> [[a]] from
-- http://www.haskell.org/haskellwiki/Data.List.Split

-- | split at regular intervals
splitEvery :: Int -> S.ByteString -> [S.ByteString]
splitEvery _ (S.uncons -> Nothing) = []
splitEvery n xs = y1 : splitEvery n y2
  where (y1, y2) = S.splitAt n xs
