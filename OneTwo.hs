module Teste where

intercalar :: t -> [t] -> [t]
intercalar x [] = []
intercalar x (y:[]) = (y:[])
intercalar x (y:ys) = y : x : intercalar x ys

type Time = String
type Gols = Int 
type Jogo = ( (Time, Gols), (Time, Gols) )
type Campeonato = [Jogo]
type Pontos = Int
type Tabela = [ (Time, Pontos) ]

campMineiro :: Campeonato
campMineiro = [ (("cruzeiro", 0) , ("atletico", 0))
              , (("uberlandia", 5), ("america", 1))
              , (("atletico" ,1), ("america", 2))
              , (("uberlandia",2), ("cruzeiro", 1))
              , (("uberlandia",3), ("urt", 1))
              , (("atletico" ,4), ("uberlandia",2))
              , (("urt" ,0), ("atletico", 1))
              , (("urt" ,2), ("america", 2))
              , (("caldense" ,7), ("atletico", 1))
              ]

vitorias :: Campeonato -> Time -> Int
vitorias x y = transfJogo x y


transfJogo:: [Jogo] -> Time -> Int
transfJogo [] y = 0
transfJogo (x:xs) y = (calcVit x y) + (transfJogo xs y)
 

calcVit :: ((Time, Gols), (Time, Gols)) -> Time ->  Int 
calcVit ((x, xa), (xb, xc)) y
   | x  == y           =  if xa>xc then 1 else 0
   | xb == y           =  if xc>xa then 1 else 0
   | otherwise         = 0


