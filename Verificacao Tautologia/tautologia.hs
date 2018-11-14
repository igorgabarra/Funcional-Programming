module Tautologia where

import Data.List (union)

data Prop = Cte Bool
            | Var String
            | Neg Prop
            | Con Prop Prop
            | Dis Prop Prop
   deriving (Show)


p0 :: Prop
p0 = Neg (Dis (Neg (Var "p"))
              (Neg (Con (Cte False)
                        (Dis (Var "p")
                             (Neg (Var "q"))))))


type Memoria = [(String, Bool)]

avalia :: Memoria -> Prop -> Bool
avalia _ (Cte x) = x
avalia m (Var x) = case lookup x m of
                    Just y -> y 
                    Nothing -> False
avalia m (Neg p) = not (avalia m p)
avalia m (Con x y) = (avalia m x) && (avalia m y)
avalia m (Dis x y) = (avalia m x) || (avalia m y)

variaveis :: Prop -> [String]
variaveis (Cte _) = []
variaveis (Var a) = [a]
variaveis (Neg p) = variaveis p
variaveis (Con x y) = union (variaveis x) (variaveis y)
variaveis (Dis x y) = union (variaveis x) (variaveis y)

booleanos :: Integer -> [[Bool]]
booleanos 0 = [[]]
booleanos n = map (False:) aux ++ map (True:) aux 
              where aux = booleanos (n-1)

substs :: Prop -> [Memoria]
substs p = map (zip v) (booleanos (toInteger (length v)))
           where v = variaveis p


tautologia :: Prop -> Bool
tautologia p = all (flip avalia p) memorias 
       where memorias = substs p

