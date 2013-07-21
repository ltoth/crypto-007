{-# LANGUAGE ViewPatterns #-}  
import Data.Int
import System.Environment
import System.Directory
import System.IO
import Crypto.Hash.SHA256
import qualified Data.ByteString.Lazy as B

blockSize = 1024

main = do
  (fileName:_) <- getArgs
  recursiveHash fileName

recursiveHash file = do
  contents <- B.readFile file
  B.foldrChunks recursiveHash' B.empty contents

recursiveHash' c h =
  let cs = chunk blockSize c
  in  foldr (\x acc -> hashlazy $ B.append x acc) h cs
    
-- | split at regular intervals
chunk :: Int64 -> B.ByteString -> [B.ByteString]
chunk _ (B.uncons -> Nothing) = [B.empty]
chunk n xs = y1 : chunk n y2
  where (y1, y2) = B.splitAt n xs
