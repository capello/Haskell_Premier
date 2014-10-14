data Polynome2 = Polynome2 Float Float Float deriving (Show)

data Solution2Degre = DeuxSolution { x1 :: Float, x2 :: Float } 
                     | UneSolution { x :: Float } 
                     | None deriving (Show)

resoudre :: Polynome2 -> Solution2Degre
resoudre (Polynome2 a b c) 
      | delta > 0 = DeuxSolution { x1 = ((-b) + (sqrt a))/(2*a) , x2 = ((-b) - (sqrt a))/(2*a) }
      | delta == 0 = UneSolution { x = -b/(2*a) }
      | otherwise = None
      where delta = b^2 - 4*a*c
