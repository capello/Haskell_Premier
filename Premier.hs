prem = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) ([3,5..truncate(sqrt (fromIntegral a::Float))]))) /= 0 ]
prem' = 2:[a | a <- [3,5..], (product (map (\x -> mod a x) (takeWhile (<= truncate(sqrt (fromIntegral a::Float))) prem'))) /= 0 ]
prem'' = 2:[a | a <- [3,5..], (all (/= 0) (map (\x -> mod a x) (takeWhile (<= truncate(sqrt (fromIntegral a::Float))) prem'))) ]

main = putStrLn (show (prem'' !! 100000))
