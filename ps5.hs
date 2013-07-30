import Control.Applicative
import qualified Data.Map as M

p = 13407807929942597099574024998205846127479365820592393377723561443721764030073546976801874298166903427690031858186486050853753882811946569946433649006084171
g = 11717829880366207009516117596335367088558084999998952205599979459063929499736583746670572176471460312928594829675428279466566527115212748467589894601965568
h = 3239475104050450443565264378728065788649097520952449527834792452971981976143292558073856937958553180532878928001494706097394108577585732452307673444020333
b = 2 ^ 20
gb = powm g b p

-- 375374217830
main :: IO ()
main = print $ getX findX0X1

getX :: [(Integer, Maybe Integer)] -> Maybe Integer
getX ((x0, Just x1):xs) = Just $ x0 * b + x1
getX (x:xs) = Nothing
getX [] = Nothing

-- [(Just 787046,357984)]
findX0X1 :: [(Integer, Maybe Integer)]
findX0X1 = let lhsMap = M.fromList [(lhs x1, x1) | x1 <- [0..b]]
           in [(x0, M.lookup r lhsMap) | x0 <- [0..b], let r = rhs x0, r `M.member` lhsMap]

-- Compute h / g^x_1 in Z_p
lhs :: Integer -> Maybe Integer
lhs x1 = let mult = (*) <$> pure h <*> ((powm g x1 p) `modInv` p)
         in  (flip mod p) <$> mult

-- Compute (g^B)^x_0
rhs :: Integer -> Maybe Integer
rhs x0 = Just $ powm gb x0 p

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
 
