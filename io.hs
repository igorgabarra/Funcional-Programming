module Main (main) where

main:: IO()

main = do putStrLn " Digite a nota do trabalho de laboratório ...: "
          s1 <- getLine
          putStrLn " Digite a nota do exame final ...............: "
          s2 <- getLine
          putStr " Conceito obtido: "
          let n1 = read s1 :: Double
              n2 = read s2 :: Double
              s = ((4 * n1) + (6 * n2))/10
          mostraResultado s

          
mostraResultado:: Double -> IO()
mostraResultado s 
    |s>=8.0 && s <= 10.0 = putStrLn "A"
    |s>=7.0 && s < 8.0  = putStrLn "B"
    |s>=6.0 && s < 7.0  = putStrLn "C"
    |s>=4.0 && s < 6.0  = putStrLn "D"
    |s>=0.0 && s < 4.0  = putStrLn "E"

