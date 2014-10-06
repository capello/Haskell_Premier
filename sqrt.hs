-- On choisi a proche de sqrt x
-- a = x / 2

table_des_carres :: [Int]
table_des_carres = [a^2|a<-[1..]]

isqrt :: Int -> Int
isqrt x = length . (takeWhile (<= x) table_des_carres)

isqrt' x = isqrt_cal x (div x 2) 1

isqrt_cal a x_n x_n_moins_1 = if ((x_n == x_n_plus_1) || (x_n_plus_1 == x_n_moins_1)) then x_n else isqrt_cal a x_n_plus_1 x_n
        where x_n_plus_1 = (div (x_n + (div a x_n)) 2)

isqrt_n_plus_1 a x_n = (div (x_n + (div a x_n)) 2 )