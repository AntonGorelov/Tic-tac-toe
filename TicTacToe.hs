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
  
isRow :: Field -> Bool
isRow [[a,b,c], [d,e,f], [g,h,i]] = (a == b && b == c && a /= 0) || (d == e && e == f && d /= 0) || (g == h && h == i && g /= 0)
  || (a == d && d == g && a /= 0) || (b == e && e == h && b /= 0) || (c == f && f == i && c /= 0) || (a == e && e == i && a /= 0)
  || (c == e && e == g && c /= 0)
  
main :: IO ()
main = do
  let field = [[0,0,0],[0,0,0],[0,0,0]]
  putStr "Введите номер клетки: "
  move1 <- getLine
  let field1 = move (read move1 :: Int) 1 field
  printField field1
  putStr "Введите номер клетки: "
  move2 <- getLine
  let field2 = move (read move2 :: Int) 2 field1
  printField field2
  putStr "Введите номер клетки: "
  move3 <- getLine
  let field3 = move (read move3 :: Int) 1 field2
  printField field3
  putStr "Введите номер клетки: "
  move4 <- getLine
  let field4 = move (read move4 :: Int) 2 field3
  printField field4
  putStr "Введите номер клетки: "
  move5 <- getLine
  let field5 = move (read move5 :: Int) 1 field4
  printField field5
  if (not $ isRow field5) then do
   putStr "Введите номер клетки: "
   move6 <- getLine
   let field6 = move (read move6 :: Int) 2 field5
   printField field6
   if (not $ isRow field6) then do
    putStr "Введите номер клетки: "
    move7 <- getLine
    let field7 = move (read move7 :: Int) 1 field6
    printField field7
    if (not $ isRow field7) then do
     putStr "Введите номер клетки: ";
     move8 <- getLine;
     let field8 = move (read move8 :: Int) 2 field7;
     printField field8;
	 if (not $ isRow field8) then do
      putStr "Введите номер клетки: ";
      move9 <- getLine;
      let field9 = move (read move9 :: Int) 1 field8;
      printField field9;
	  if (not $ isRow field9) then print Draw
	  else print First
	 else print Second
	else print First
   else print Second
  else print First