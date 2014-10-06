debut :: Integer -> [Integer] -> [Integer]
debut max (x:xs) = if x <= max then x:debut max xs else []

prem = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) ([3,5..truncate(sqrt (fromIntegral a::Float))]))) /= 0 ]
prem' = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) (takeWhile (<= truncate(sqrt (fromIntegral a::Float))) prem'))) /= 0 ]

prem2 = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) ([3,5..isqrt a]))) /= 0 ]
prem2' = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) (takeWhile (<= isqrt a) prem'))) /= 0 ]

--estPremier :: Integer -> Bool
--estPremier nb = product(map (\x -> nb `mod` x) 2:[3,5..truncate(sqrt (fromIntegral nb::Float))]) /= 0

{-resoudre a b c
        | ourDelta < 0 = (0,0)
        | ourDelta == 0 = (( (-b)) / (2 * a),0)
        | otherwise = (( (-b) + sqrt ourDelta) / (2 * a),( (-b) - sqrt ourDelta) / (2 * a))
        where 
            ourDelta = b^2 - 4*a*c
-}

fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n = fib (n-1) + fib (n-2)

table_des_carres :: [Int]
table_des_carres = [a^2|a<-[1..]]

isqrt :: Int -> Int
isqrt x = length (takeWhile (<= x) table_des_carres)
