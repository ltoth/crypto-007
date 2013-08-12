import Data.Maybe (fromMaybe)
import Numeric (showHex)
import Data.List.Split (splitOn)
import Data.Hex (unhex)

n = 179769313486231590772930519078902473361797697894230657273430081157732675805505620686985379449212982959585501387537164015710139858647833778606925583497541085196591615128057575940752635007475935288710823649949940771895617054361149474865046711015101563940680527540071584560878577663743040086340742855278549092581 :: Integer

e = 65537 :: Integer

c = 22096451867410381776306561134883418017410069787892831071731839143676135600120538004282329650473509424343946219751512256465839967942889460764542040581564748988013734864120452325229320176487916666402997509188729971690526083222067771600019329260870009579993724077458967773697817571267229951148662959627934791540 :: Integer

a0 = 1 + squareRoot n

x a = squareRoot $ a ^ 2 - n

a = head $ [ a | a <- [a0..], (x a)^2 == a^2 - n ]

p = a - x a
q = a + x a

φ_n = n - p - q + 1

d = fromMaybe 0 $ e `modInv` φ_n

m :: Integer
m = powm c d n

m' :: String
m' = last . splitOn "00" $ showHex m ""

main :: IO ()
main = unhex m' >>= putStrLn

-- Utility functions below

(^!) :: Num a => a -> Int -> a
(^!) x n = x^n
 
squareRoot :: Integer -> Integer
squareRoot 0 = 0
squareRoot 1 = 1
squareRoot n =
   let twopows = iterate (^!2) 2
       (lowerRoot, lowerN) =
          last $ takeWhile ((n>=) . snd) $ zip (1:twopows) twopows
       newtonStep x = div (x + div n x) 2
       iters = iterate newtonStep (squareRoot (div n lowerN) * lowerRoot)
       isRoot r  =  r^!2 <= n && n < (r+1)^!2
   in  head $ dropWhile (not . isRoot) iters

-- http://rosettacode.org/wiki/Modular_exponentiation
-- Compute modular exponentiation b^e mod m
powm :: Integer -> Integer -> Integer -> Integer
powm b e m = powm' b e m 1

powm' :: Integer -> Integer -> Integer -> Integer -> Integer
powm' b 0 m r = r
powm' b e m r | e `mod` 2 == 1 = powm' (b * b `mod` m) (e `div` 2) m (r * b `mod` m)
powm' b e m r = powm' (b * b `mod` m) (e `div` 2) m r

-- http://rosettacode.org/wiki/Modular_inverse#Haskell
-- Given a and m, return Just x such that ax = 1 mod m.  If there is no such x
-- return Nothing.
modInv :: Integral a => a -> a -> Maybe a
modInv a m = let (i, _, g) = gcdExt a m
             in if g == 1 then Just (mkPos i) else Nothing
  where mkPos x = if x < 0 then x + m else x
 
-- Extended Euclidean algorithm.  Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).  Note that x or y may be negative.
gcdExt :: Integral t => t -> t -> (t, t, t)
gcdExt a 0 = (1, 0, a)
gcdExt a b = let (q, r) = a `quotRem` b
                 (s, t, g) = gcdExt b r
             in (t, s - q * t, g)
