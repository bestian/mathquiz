module Main where

import Lib

loop = do b <- rollDice
          x <- rollDice
          y <- rollDice2
          o <- rollOp
--          putStrLn $ show o
          q <- return $ Op Add b (Op o x y)
          ans <- return $ eval q
          putStrLn (show q ++ " = ?")
          myA <- getLine
          let a = (read myA) :: Int
          if ans == a
             then print "Geart!"
          else
             print "Wrong!"
          if ans == a
             then loop
          else
             return ()

main = do putStrLn "Welcome to Math Quiz!"
          loop
--          putStrLn (show ans)
