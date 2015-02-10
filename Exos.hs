myLast :: [a] -> a 
myLast [x] = x
myLast (x:xs) = myLast xs

myButLast :: [a] -> a 
myButLast [] = error "List empty"
myButLast [x] = error "Singleton not allowed"
myButLast (x:y:[]) = x
myButLast (_:xs) = myButLast xs

elementAt :: (Num b,Eq b) => [a] -> b -> a
elementAt [] _  = error "List too short"
elementAt (x:_) 1 = x
elementAt (_:xs) n = elementAt xs (n-1)

myLength :: (Num b) => [a] -> b
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

isPalindrome :: Eq(a) => [a] -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome list = (head list == last list) && isPalindrome (tail (init list))
