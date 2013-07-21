{-# LANGUAGE ViewPatterns #-}  
import Data.Int
import Data.Hex
import System.Environment
import Crypto.Hash.SHA256
import qualified Data.ByteString as S
import qualified Data.ByteString.Lazy as B

blockSize = 1024

--main = do
--  (fileName:_) <- getArgs
--  h0 <- recursiveHash fileName
--  putStrLn $ hex h0

recursiveHash f = do
  contents <- B.readFile f
  return $ foldr recursiveHash' S.empty $ B.toChunks contents

recursiveHash' c h =
  let bs = splitEvery blockSize c
  in  foldr (\x acc -> hash $ S.append x acc) h bs
    
-- based on chunk :: Int -> [a] -> [[a]] from
-- http://www.haskell.org/haskellwiki/Data.List.Split

-- | split at regular intervals
splitEvery :: Int -> S.ByteString -> [S.ByteString]
splitEvery _ (S.uncons -> Nothing) = [S.empty]
splitEvery n xs = y1 : splitEvery n y2
  where (y1, y2) = S.splitAt n xs
