import Data.Bits (xor)
import Data.Char (ord, chr, toLower)
import Data.Hex
import Control.Monad.State
import Network.HTTP (simpleHTTP, getRequest, getResponseCode, ResponseCode)
import Data.List.Split (chunksOf)

type Word = Int
type BlockIdx = Int
type WordIdx = Int
type Pad = Int

type Blocks = [(BlockIdx, [Word])]

blockSize = 16
target = "http://crypto-class.appspot.com/po?er="
cipher = "f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"

main :: IO ()
main =
  let cs = chunkize $ hexToInt cipher
  in  getPlainBlock 0 cs >>= print

getPlainBlock :: BlockIdx -> Blocks -> IO String
getPlainBlock b xs = do
  ms <- execStateT (replicateM_ blockSize $ getPlainState b xs) []
  return $ map (chr . third) ms
  where third (_,_,x) = x

getPlainState :: BlockIdx -> Blocks -> StateT [(BlockIdx, WordIdx, Word)] IO ()
getPlainState b xs = do
  ks <- get
  let p = length ks + 1
  let xs' = setKnown p b ks xs
  x <- liftIO $ getPlainWord p b xs'
  modify ((b, p, x):)

getPlainWord :: WordIdx -> BlockIdx -> Blocks -> IO Word
getPlainWord p b is = liftM length $ sequenceUntil not (map query qs) 
  where qs = [getQuery p g b is | g <- [0..255]]

setKnown :: Int -> BlockIdx -> [(BlockIdx, WordIdx, Word)] -> Blocks -> Blocks
setKnown p b ks xs = foldr setK (take (b+2) xs) (take (p-1) ks)
  where setK (b, idx, g) acc = chunkize $ setGuess p g b idx acc

getQuery :: Pad -> Word -> BlockIdx -> Blocks -> String
getQuery p g b = intToHex . setGuess p g b p

chunkize :: [Word] -> Blocks
chunkize xs = zip [0..] $ chunksOf blockSize xs

setGuess :: Pad -> Word -> BlockIdx -> WordIdx -> Blocks -> [Word]
setGuess p g b idx = concatMap (mutate (pad p g idx) b)

pad :: Pad -> Word -> WordIdx -> [Word] -> [Word]
pad p g idx = reverse . map (mutate (guess p g) idx) . zip [1..] . reverse

guess :: Pad -> Word -> Word -> Word
guess p g c = p `xor` g `xor` c

mutate :: Eq a => (t -> t) -> a -> (a, t) -> t
mutate f tidx (idx, xs) 
 | tidx == idx = f xs
 | otherwise   = xs

query :: String -> IO Bool
query q = do
  code <- getCode $ target ++ q
  return $ oracle code

oracle :: (Int, Int, Int) -> Bool
--oracle (2,0,0) = True
oracle (4,0,4) = True
oracle (_,_,_) = False

getCode :: String -> IO ResponseCode
getCode url = simpleHTTP (getRequest url) >>= getResponseCode

sequenceUntil :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
sequenceUntil p = foldr (myLiftM2 (:) []) (return [])
  where myLiftM2 f z m1 m2 = do
            x1 <- m1
            if p x1 then do x2 <- m2
                            return $ f x1 x2
                    else return z

hexToInt :: String -> [Int]
hexToInt = map ord . join . unhex

intToHex :: [Int] -> String
intToHex = map toLower . hex . map chr
