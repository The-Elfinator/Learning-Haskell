lenVec3 x y z = sqrt (x ^ 2 + y ^ 2 + z ^ 2)

sign x = if x == 0 then 0 else (if x > 0 then 1 else (-1))

discount limit proc summ = if summ >= limit then summ * (100 - proc) / 100 else summ
standardDiscount = discount 1000 5

infixl 6 *+*
infixl 6 |-|

a *+* b = a ^ 2 + b ^ 2
a |-| b = abs (a - b)

main :: IO ()
main =  do
    print(sin $ 0)
