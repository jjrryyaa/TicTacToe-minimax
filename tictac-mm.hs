module TicTacMinimax where 

import Text.Read
import Data.List
import Tictacmain


---------------------------------- data/ type declarations ----------------------------------

data Player = X | O 
      deriving (Show, Eq)
  -- note: a board will be a list of cells
data Cell = Occupied Player | Empty

data Result = EndOfGame Player [Cell] | ContinueGame [Cell]  | TiedGame [Cell] 

instance Show Result where
  show (EndOfGame player board) = "EndOfGame" ++ (show player)
  show (ContinueGame board) = "ContinueGame"
  show (TiedGame board) = "TiedGame"


instance Show Cell where
  show (Occupied X)     = "X"
  show (Occupied O)     = "O"
  show Empty            = " "

instance Eq Cell where
  Occupied X == Occupied X = True
  Occupied O == Occupied O = True
  Empty == Empty           = True
  _ == _                   = False

---------------------------------- supporting functions ----------------------------------

-- returns next player
nextPlayer :: Player -> Player
nextPlayer X = O
nextPlayer O = X


-- Helper functions that returns the state of a game after a move has been made 
getGameState :: [Cell] -> Result
getGameState board 
   | isWinningMove X board = EndOfGame X board
   | isWinningMove O board = EndOfGame O board
   | otherwise = isTiedOrContinue board


isTiedOrContinue :: [Cell] -> Result
isTiedOrContinue board = 
  if (boardIsFull board)
    then TiedGame board
    else ContinueGame board

createRow :: [Cell] -> String 
createRow row = intercalate " | " (map show row)

rowLine :: String 
rowLine = "----------"

createBoard :: [Cell] -> IO()
createBoard board = do
  putStrLn  $ createRow firstRow
  putStrLn rowLine
  putStrLn  $ createRow secondRow
  putStrLn rowLine
  putStrLn  $ createRow thirdRow 
  where firstRow = take 3 board -- first 3 of board list
        secondRow = drop 3 . take 6 $ board -- second triplet
        thirdRow = drop 6 board -- last triplet 


-- converts users input into an index from the board array
getBoardIndex :: String -> Int 
getBoardIndex "A1" = 0
getBoardIndex "A2" = 1
getBoardIndex "A3" = 2
getBoardIndex "B1" = 3
getBoardIndex "B2" = 4
getBoardIndex "B3" = 5
getBoardIndex "C1" = 6
getBoardIndex "C2" = 7
getBoardIndex "C3" = 8
getBoardIndex _ = 9

---------------------------------- validating each play ----------------------------------


-- checks to see if the cell user wants to move to is free
checkFree :: [Cell] -> Int -> Bool
checkFree board pos = board !! pos == Empty 

-- checks to see if the last move played is a winning move, by checking if player has filled any of the rows or diagonals
isWinningMove :: Player -> [Cell] -> Bool 
isWinningMove player board = 
  or [
    -- top most row
  (board !! 0 == (Occupied player) && board !! 1 == (Occupied player) && board !! 2 == (Occupied player)),
    -- second row
  (board !! 3 == (Occupied player) && board !! 4 == (Occupied player) && board !! 5 == (Occupied player)),
    -- third row
  (board !! 6 == (Occupied player) && board !! 7 == (Occupied player) && board !! 8 == (Occupied player)),
    -- 0 to 8, diagonal
  (board !! 0 == (Occupied player) && board !! 4 == (Occupied player) && board !! 8 == (Occupied player)),
    -- 2 to 6, diagonal
  (board !! 2 == (Occupied player) && board !! 4 == (Occupied player) && board !! 6 == (Occupied player)),
    -- 2,5,8 column
  (board !! 2 == (Occupied player) && board !! 5 == (Occupied player) && board !! 8 == (Occupied player)),
    -- 0,3,6 column
  (board !! 0 == (Occupied player) && board !! 3 == (Occupied player) && board !! 6 == (Occupied player)),
   -- 1,4,7 column
  (board !! 1 == (Occupied player) && board !! 4 == (Occupied player) && board !! 7 == (Occupied player))

    ]

-- checks to see if the board is full
boardIsFull :: [Cell] -> Bool
boardIsFull board = not (elem Empty board)

-- puts the player's move at the correct index, returns the new board
boardArray :: Int -> [Cell] -> Player -> [Cell]
boardArray index board player =  (take index board) ++ [Occupied player] ++ (drop (index+1) board)

-- places move on the board, returns the result and the new board
placeMove :: String -> Player -> [Cell] -> (Result, [Cell])
placeMove pos player board = 
    let index = getBoardIndex pos in 
    if (index /= 9 && (checkFree board index)) 
     then
      let newBoard = boardArray index board player in
        if (isWinningMove player newBoard)  
            then (EndOfGame player newBoard, newBoard)
            else 
             if (boardIsFull newBoard)
              then (TiedGame newBoard, newBoard) 
              else (ContinueGame board, newBoard)  
      else (ContinueGame board, board)



---------------------------------- START GAME ----------------------------------

initialBoard = replicate 9 Empty
main = startMessage


startMessage :: IO()
startMessage = do 
  putStrLn "Welcome to Tic-Tac-Toe! Enter (1) to play against our smart computer, or (2) to play two player version, or (3) to exit"
  ans <- getLine
  case ans of 
    "1" -> startGame 
    "2" -> mainGUI
    _ -> return ()


---------------------------------- two player implementations ----------------------------------

playGame:: Player -> [Cell] -> IO()
playGame player board = do
  putStrLn "Pick a cell, from A1 to C3. For example: A1 means top row, left most box."
  createBoard board 
  putStrLn "Enter Cell:"
  cell <- getLine
  let (result, newBoard) = placeMove cell player board
  if (gameState result) then 
    playGame (nextPlayer player) (newBoard)
  else endOfGame result

gameState :: Result -> Bool
gameState (EndOfGame _ _) = False
gameState (ContinueGame _) = True
gameState (TiedGame _) = False

endOfGame :: Result -> IO()
endOfGame (EndOfGame X board) = do 
  putStrLn "Player X won. Enter 'main' to start again"
  createBoard board
endOfGame (EndOfGame O board) = do 
  putStrLn "Player O won. Enter 'main' to start again"
  createBoard board
endOfGame (TiedGame board) = do 
  putStrLn "No one won! Tied Game. Enter 'main' to start again"
  createBoard board


---------------------------------- player vs computer implementation  ----------------------------------


startGame :: IO ()
startGame = do
  endState <- gameLoop initialBoard X 
  putStrLn $ case endState of
    EndOfGame player _ -> (show player) ++ "won!"
    TiedGame _ -> "Uh oh, it was a tie! No one wins"
  putStrLn "Play again y/n"
  answer <- getLine
  case answer of
    "y" -> startGame
    _ -> return ()


gameLoop :: [Cell] -> Player -> IO Result 
gameLoop board player = do
  (result,newBoard) <- case player of
    X -> do putStrLn "X's move:"
            let (result, newBoard) = computerMove board
            createBoard newBoard
            return (result, newBoard)
    O -> do putStrLn "Your turn!"
            (result, newBoard) <- playerMove board
            createBoard newBoard
            return (result, newBoard)
  case (result, newBoard) of
    ((EndOfGame _ _), _) -> return result 
    ((ContinueGame _), newBoard) -> gameLoop newBoard $ nextPlayer player
    ((TiedGame _), _) -> return result



-- player places move, function returns board
playerMove :: [Cell] -> IO (Result, [Cell])
playerMove board = do 
  putStrLn "Pick a cell, from A1 to C3. For example: A1 means top row, left most box."
  putStrLn "Enter Cell:"
  cell <- getLine
  let (result, newBoard) = placeMove cell O board 
  return (result, newBoard)


computerMove :: [Cell] -> (Result, [Cell])
computerMove board = (result, newBoard)
  where    
    (pos, val) = minimax board X
    (result, newBoard) = placeMove (posToString pos) X board


posToString :: Int -> String
posToString 0 = "A1"
posToString 1 = "A2"
posToString 2 = "A3"
posToString 3 = "B1"
posToString 4 = "B2"
posToString 5 = "B3"
posToString 6 = "C1"
posToString 7 = "C2"
posToString 8 = "C3"


minimax :: [Cell] -> Player -> (Int, Int) 
minimax board player = argmax player (list_of_nextboards)
  where
    list_of_nextboards = nextMoves board player


nextMoves :: [Cell] -> Player -> [([Cell], Int)]
nextMoves board player = foldr (\x y -> ((placeOnBoard x board player), x):y) [] lst
  where
    lst = foldr(\x y -> if (board !! x == Empty) then x:y else y) [] [0..8]

-- placeOnBoard renders the board assuming that player chose to place move on postion pos 
placeOnBoard :: Int -> [Cell] -> Player -> [Cell]
placeOnBoard pos board player = boardArray pos board player


valueAction :: Player -> [Cell] -> Int 
valueAction player board = value board result player 
  where
    result = getGameState board 

value :: [Cell] -> Result -> Player -> Int
value board (EndOfGame _ _) player = 1
value board (TiedGame _) player = 0
value board (ContinueGame _) player = -snd (minimax board (nextPlayer player))


argmax ::  Player -> [([Cell], Int)] -> (Int, Int)
argmax player [(a,b)] = (b, valueAction player a)
argmax player ((x,y):t) 
   | valueAction player x > fy = (y, valueAction player x)                       -- valueAction player [first board] > valueAction of other boards, then ret (index, value first board)
   | otherwise = (by, fy)                    -- otherwise return the best from the rest of the moves
   where
      (by, fy) = argmax player t

