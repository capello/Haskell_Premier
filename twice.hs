twice :: (Num a) => a -> (a -> a) -> a
twice x f = f x + f x

square :: (Num a) => a -> a
square x = x * x

