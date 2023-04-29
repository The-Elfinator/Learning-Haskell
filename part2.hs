import Data.Char

lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x == 0 then 0 else (if x > 0 then 1 else (-1))

discount :: Double -> Double -> Double -> Double
discount limit proc summ = if summ >= limit then summ * (100 - proc) / 100 else summ
standardDiscount :: Double -> Double
standardDiscount = discount 1000 5

infixl 6 *+*
infixl 6 |-|

a *+* b = a ^ 2 + b ^ 2
a |-| b = abs (a - b)

x = 3.2 :: Double
y = 5 :: Double

twoDigits2Int :: Char -> Char -> Int
twoDigits2Int x y = if isDigit x && isDigit y then digitToInt x * 10 + digitToInt y else 100

sampleCortege = (2, True, 'x')
pair = (2, True)

dist :: (Double, Double) -> (Double, Double) -> Double
dist p1 p2 = sqrt $ (fst p1 - fst p2) ^ 2 + (snd p1 - snd p2) ^ 2 

str =  (:) 'H' "ello"

main :: IO ()
main =  do
    print(fst pair)
    print(snd pair)
    print(dist (1, 0) (0, 1))
    print(str)
    print((++) [1,2] 3 : [4,5,6])
