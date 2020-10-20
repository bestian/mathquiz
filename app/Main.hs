module Main where

import Lib

main = do x <- rollDice
          y <- rollDice2
          o <- rollOp
--          putStrLn $ show o
          q <- return $ Op o x y
          ans <- return $ eval q
          putStrLn (show q ++ " = ?")
          myA <- getLine
          let a = (read myA) :: Int
          print $ ans == a
--          putStrLn (show ans)
