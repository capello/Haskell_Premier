twice :: Integer -> (Integer -> Integer) -> Integer
twice x f = f x + f x

square :: Integer -> Integer
square x = x * x

-- main = twice 5 square