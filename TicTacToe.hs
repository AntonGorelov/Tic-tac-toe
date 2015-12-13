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

move :: Int -> Int -> Field -> Field
move n p [[a,b,c], [d,e,f], [g,h,i]]
  | n == 1 && a == 0 = [[p,b,c], [d,e,f], [g,h,i]]
  | n == 2 && b == 0 = [[a,p,c], [d,e,f], [g,h,i]]
  | n == 3 && c == 0 = [[a,b,p], [d,e,f], [g,h,i]]
  | n == 4 && d == 0 = [[a,b,c], [p,e,f], [g,h,i]]
  | n == 5 && e == 0 = [[a,b,c], [d,p,f], [g,h,i]]
  | n == 6 && f == 0 = [[a,b,c], [d,e,p], [g,h,i]]
  | n == 7 && g == 0 = [[a,b,c], [d,e,f], [p,h,i]]
  | n == 8 && h == 0 = [[a,b,c], [d,e,f], [g,p,i]]
  | n == 9 && i == 0 = [[a,b,c], [d,e,f], [g,h,p]]
  | otherwise = error "incorrect move"
