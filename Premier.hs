#!env runhaskell

prem = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) ([3,5..truncate(sqrt (fromIntegral a::Float))]))) /= 0 ]
prem' = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) (takeWhile (<= truncate(sqrt (fromIntegral a::Float))) prem'))) /= 0 ]
prem'' = 2:[a | a <- [3,5..], (all (/= 0) (map (\x -> mod a x) (takeWhile (<= truncate(sqrt (fromIntegral a::Float))) prem''))) ]
prem''' = 2:[a | a <- [3,5..], (all (/= 0) (map (mod a) (takeWhile (<= truncate(sqrt (fromIntegral a::Float))) prem'''))) ]
prem'''' = 2:[a | a <- [3,5..], (all (/= 0) (map (mod a) (takeWhile (<= (isqrt' a)) prem''''))) ]

prem2 = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) ([3,5..isqrt a]))) /= 0 ]
prem2' = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) (takeWhile (<= isqrt a) prem2'))) /= 0 ]
prem2'' = 2:[a | a <- [3,5..], (all (/= 0) (map (\x -> mod a x) (takeWhile (<= isqrt a) prem2''))) ]

table_des_carres :: [Int]
table_des_carres = [a^2|a<-[1..]]

isqrt :: Int -> Int
isqrt x = length (takeWhile (<= x) table_des_carres)

isqrt' :: Int -> Int
isqrt' = truncate . sqrt . fromIntegral

main = putStrLn (show (prem'' !! 100000))

