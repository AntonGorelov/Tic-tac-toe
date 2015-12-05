module TicTacToe where

data Result = First | Draw | Second
  deriving (Eq, Show)
  
type Field = [[Int]]

printElem :: Int -> IO ()
printElem 0 = putStr "-"
printElem 1 = putStr "X"
printElem 2 = putStr "0"
printElem _ = error "incorrect move"