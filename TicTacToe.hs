module TicTacToe where

data Result = First | Draw | Second
  deriving (Eq, Show)
  
type Field = [[Int]]

printElem :: Int -> IO ()
printElem 0 = putStr "-"
printElem 1 = putStr "X"
printElem 2 = putStr "0"
printElem _ = error "incorrect move"

printString :: [Int] -> IO ()
printString (x:y:z:[]) = do
  printElem x
  printElem y
  printElem z
printString _ = error "incorrect string"
  
printField :: Field -> IO ()
printField (x:y:z:[]) = do
  printString x
  putStrLn ""
  printString y
  putStrLn ""
  printString z
  putStrLn ""
printField _ = error "incorrect field"