import Data.Bits
import Control.Monad
import Network.HTTP
import XorUtils
import Data.List.Split (chunksOf)

blockSize = 16
target = "http://crypto-class.appspot.com/po?er="
cipher = "f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"

type Word = Int
type BlockIdx = Int
type WordIdx = Int
type Pad = Int

type Blocks = [(BlockIdx, [Word])]

m0 :: [(BlockIdx, WordIdx, Int)]
m0 = [(0,1,32),(0,2,115),(0,3,100),(0,4,114),(0,5,111),(0,6,87),(0,7,32),(0,8,99),(0,9,105),(0,10,103),(0,11,97),(0,12,77),(0,13,32),(0,14,101),(0,15,104),(0,16,84)]

m1 :: [(BlockIdx, WordIdx, Int)]
m1 = [(1,1,115),(1,2,79),(1,3,32),(1,4,104),(1,5,115),(1,6,105),(1,7,109),(1,8,97),(1,9,101),(1,10,117),(1,11,113),(1,12,83),(1,13,32),(1,14,101),(1,15,114),(1,16,97)]

--ks = [(2,1,9),(2,2,9),(2,3,9),(2,4,9),(2,5,9),(2,6,9),(2,7,9),(2,8,9),(2,9,0)]

third (_,_,x) = x

setKnown :: Int -> [(BlockIdx, WordIdx, Word)] -> Blocks -> Blocks
setKnown p ks xs = foldl setK xs (take (p-1) ks)
  where setK acc (b, idx, g) = chunkize $ setGuess p g b idx acc

cs = chunkize $ hexToInt cipher

main = print ""

getPlain :: WordIdx -> BlockIdx -> Blocks -> IO Word
getPlain p b is = liftM length $ sequenceUntil not (map query qs) 
  where qs = [getQuery p g b is | g <- [0..255]]

getQuery :: Pad -> Word -> BlockIdx -> Blocks -> String
getQuery p g b = (intToHex . setGuess p g b p)

chunkize :: [Word] -> Blocks
chunkize xs = zip [0..] $ chunksOf blockSize xs

setGuess :: Pad -> Word -> BlockIdx -> WordIdx -> Blocks -> [Word]
setGuess p g b idx = concatMap (mutate (pad p g idx) b)

pad :: Pad -> Word -> WordIdx -> [Word] -> [Word]
pad p g idx = reverse . (map (mutate (guess p g) idx)) . (zip [1..]) . reverse

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
