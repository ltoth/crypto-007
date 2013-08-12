n = 720062263747350425279564435525583738338084451473999841826653057981916355690188337790423408664187663938485175264994017897083524079135686877441155132015188279331812309091996246361896836573643119174094961348524639707885238799396839230364676670221627018353299443241192173812729276147530748597302192751375739387929

a = squareRoot (6 * n)

--    N = pq = 1/6 * (floor(a)^2 + floor(a) - x^2 - x)
-- => x = 1/2 * ( -1 + sqrt( 1 - 4 (6N - floor(a)^2 - floor(a))))
x = (`div` 2) $ (-1) + (squareRoot $ 1 - 4 * (6 * n - a^2 -a))

-- p = (floor(a) - x) `div` 3
-- q = (ceiling(a) + x) `div` 2 = (floor(a) + 1 + x) `div` 2
p = (a - x) `div` 3
q = (a + x + 1) `div` 2

main :: IO ()
main = print (p,q)

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
