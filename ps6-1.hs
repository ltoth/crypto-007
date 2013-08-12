import Control.Applicative
import qualified Data.Map as M

n = 179769313486231590772930519078902473361797697894230657273430081157732675805505620686985379449212982959585501387537164015710139858647833778606925583497541085196591615128057575940752635007475935288710823649949940771895617054361149474865046711015101563940680527540071584560878577663743040086340742855278549092581 :: Integer

-- Since squareRoot is floor . sqrt, we add 1 to get ceiling
a0 = 1 + squareRoot n

x a = squareRoot $ a ^ 2 - n

a' = head $ [ a | a <- [a0..], (x a)^2 == a^2 - n ]

p = a' - x a'
q = a' + x a'

main :: IO ()
main = print (p,q) 

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
