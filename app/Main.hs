module Main where

import Lib

getQ b x y y2 z z2 o o2 lev | lev == 0 = return $ Op Add y y 
                            | lev == 1 = return $ Op Add b y 
                            | lev == 2 = return $ Op Sub b y
                            | lev == 3 = return $ Op Mul y y2
                            | lev == 4 = return $ Op Div (fst x) (snd x)
                            | lev == 5 = return $ Op Add b (Op o y y2)
                            | lev == 6 = return $ Op o2 z z2
                            | otherwise = return $ Op Add b y

makeTxt a 0 t = do return t
makeTxt a n t = do t <- makeTxt a (n-1) t
                   b <- rollDice
                   x <- rollDice4
                   y <- rollDice2
                   y2 <- rollDice2
                   z <- rollDice3
                   z2 <- rollDice3
                   o <- rollOp
                   o2 <- rollOp2
                   q <- getQ b x y y2 z z2 o o2 a
                   return (show q ++ " = ?\n\n" ++ t)

loop l s e = do b <- rollDice
                x <- rollDice4
                y <- rollDice2
                y2 <- rollDice2
                z <- rollDice3
                z2 <- rollDice3
                o <- rollOp
                o2 <- rollOp2
                q <- getQ b x y y2 z z2 o o2 l
                ans <- return $ eval q
                putStrLn (show q ++ " = ?")
                myA <- getLine
                let a = (read myA) :: Int
                if ans == a
                   then do putStrLn $ "答對了！Great!"
                           putStrLn $ "目前分數：" ++ show (s+1)
                           if s + 1 >= 10
                             then do putStrLn $ "你勝利了！\n\n"
                                     main
                             else loop l (s + 1) e
                else
                   do putStrLn $ "答錯了，沒關係，再接再勵。答案是" ++ show ans
                      if e + 1 >= 3
                        then main
                      else loop l s (e + 1)

main = do putStrLn "\n歡迎來到數學小測驗！\n"
          putStrLn "選擇一項: "
          putStrLn "0]加法一 1]加法二 2]減法 3]乘法 4]除法 5]綜合 6]負數加減"
          myA <- getLine
          let a = (read myA) :: Int
          txt <- makeTxt a 10 ""
          writeFile "./output.txt" txt
          loop a 0 0
