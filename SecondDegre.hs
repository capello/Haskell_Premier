#!/usr/bin/runhaskell
import System.Environment

data Polynome2 = Polynome2 Float Float Float deriving (Show)

data Solution2Degre = DeuxSolution { x1 :: Float, x2 :: Float } 
                     | UneSolution { x :: Float } 
                     | None deriving (Show)



polyFromList :: [Float] -> Polynome2
polyFromList (a:b:c:[]) = Polynome2 a b c
polyFromList _ = error "Un polynome du second degré contient 3 parametres."

resoudre :: Polynome2 -> Solution2Degre
resoudre (Polynome2 a b c) 
      | delta > 0 = DeuxSolution { x1 = ((-b) + (sqrt delta))/(2*a) , x2 = ((-b) - (sqrt delta))/(2*a) }
      | delta == 0 = UneSolution { x = -b/(2*a) }
      | otherwise = None
      where delta = b^2 - 4*a*c

main = do
       args <- getArgs
       if length args == 3
       then do
            let poly = map read args :: [Float]
            print $ resoudre $ polyFromList poly
       else
           return ()

