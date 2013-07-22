{-# LANGUAGE ViewPatterns #-}  
import Data.Int
import Data.Char
import Data.Hex
import System.Environment
import Crypto.Hash.SHA256
import qualified Data.ByteString.Char8 as S

blockSize = 1024

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
  in  foldr (\x acc -> hash $ S.append x acc) S.empty bs
    
toHex :: S.ByteString -> [Char]
toHex b = map toLower $ hex $ S.unpack b

-- based on chunk :: Int -> [a] -> [[a]] from
-- http://www.haskell.org/haskellwiki/Data.List.Split

-- | split at regular intervals
splitEvery :: Int -> S.ByteString -> [S.ByteString]
splitEvery _ (S.uncons -> Nothing) = []
splitEvery n xs = y1 : splitEvery n y2
  where (y1, y2) = S.splitAt n xs
