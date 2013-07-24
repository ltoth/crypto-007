import Data.Bits
import Control.Monad
import Network.HTTP
import XorUtils
import Data.List.Split (chunksOf)

blockSize = 16
target = "http://crypto-class.appspot.com/po?er="
cipher = "f20bdba6ff29eed7b046d1df9fb7000058b1ffb4210a580f748b4ac714c001bd4a61044426fb515dad3f21f18aa577c0bdf302936266926ff37dbf7035d5eeb4"

-- iv = take blockSize $ hexToInt cipher
cs = getInts cipher

main = print ""

getPlain p b is = liftM length $ sequenceUntil not (map query qs) 
  where qs = [getQuery p g b is | g <- [0..255]]

getInts q = zip [0..] $ chunksOf blockSize (hexToInt q)

getQuery p g b = intToHex . concatMap (mutate (pad p g) b)

pad p g = pad' p g p
pad' p g idx = reverse . (map (mutate (guess g p) idx)) . (zip [1..]) . reverse

guess g p c = p `xor` g `xor` c

mutate f tidx (idx, xs) 
 | tidx == idx = f xs
 | otherwise   = xs

query q = do
  code <- getCode $ target ++ q
  return $ oracle code

oracle (4,0,3) = False
oracle (4,0,4) = True
oracle (_,_,_) = False

getCode url = simpleHTTP (getRequest url) >>= getResponseCode

sequenceUntil :: (Monad m) => (a -> Bool) -> [m a] -> m [a]
sequenceUntil p = foldr (myLiftM2 (:) []) (return [])
  where myLiftM2 f z m1 m2 = do
            x1 <- m1
            if p x1 then do x2 <- m2
                            return $ f x1 x2
                    else return z
