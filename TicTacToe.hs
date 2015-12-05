module TicTacToe where

data Result = First | Draw | Second
  deriving (Eq, Show)
  
type Field = [[Int]]