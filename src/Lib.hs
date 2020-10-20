module Lib
    ( Quiz (Lit, Op), Ops (Add, Sub, Mul), show, eval, 
      rollDice, rollDice2, rollOp
    ) where


import System.Random 

data Quiz = Lit Int | Op Ops Quiz Quiz
data Ops = Add | Sub | Mul

instance Show Quiz where 
  show (Lit a) = show a
  show (Op o a b) = show a ++ show o ++ show b

instance Show Ops where
  show Add = "+"
  show Sub = "-"
  show Mul = "x"

eval :: Quiz -> Int
eval (Lit x) = x
eval (Op Add a b) = eval a + eval b
eval (Op Sub a b) = eval a - eval b
eval (Op Mul a b) = eval a * eval b

rollDice :: IO Quiz
rollDice = do x <- getStdRandom (randomR (10,20))
              return $ Lit x


rollDice2 :: IO Quiz
rollDice2 = do x <- getStdRandom (randomR (1,10))
               return $ Lit x

rollOp :: IO Ops
rollOp = do k <- getStdRandom (randomR (0,2))
            return $ [Add, Sub, Mul] !! k