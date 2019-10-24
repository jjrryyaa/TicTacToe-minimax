module Tictacgame where

  ---------------- DATA DECLARATIONS FOR GUI  -----------------

  data Player = X | O 
      deriving (Show, Eq)
  -- note: a board will be a list of cells
  data Cell = Occupied Player | Empty

  data Result = EndOfGame Player [Cell] | ContinueGame [Cell]  | TiedGame [Cell]
      deriving (Show,Eq)

  data Game = Game [Cell] Player Result 
      deriving (Show)

  instance Show Cell where
    show (Occupied X)     = "X"
    show (Occupied O)     = "O"
    show Empty            = " "

  instance Eq Cell where
    Occupied X == Occupied X = True
    Occupied O == Occupied O = True
    Empty == Empty           = True
    _ == _                   = False



  ---------------- SCREEN CONSTANTS FOR GUI -----------------

  screenWidth :: Int
  screenWidth = 640

  screenHeight :: Int
  screenHeight = 480

  cellWidth :: Float
  cellWidth = fromIntegral screenWidth / fromIntegral 3

  cellHeight :: Float
  cellHeight = fromIntegral screenHeight / fromIntegral 3


  
-- Helper functions that returns the state of a game after a move has been made 
  getGameState :: [Cell] -> Result
  getGameState board 
     | isWinningMove X board = EndOfGame X board
     | isWinningMove O board = EndOfGame O board
     | otherwise = isTiedOrContinue board

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

  isTiedOrContinue :: [Cell] -> Result
  isTiedOrContinue board = 
    if (boardIsFull board)
      then TiedGame board
      else ContinueGame board

  -- checks to see if the board is full
  boardIsFull :: [Cell] -> Bool
  boardIsFull board = not (elem Empty board)

  nextPlayer:: Player -> Player
  nextPlayer O = X 
  nextPlayer X = O

  startingBoard = replicate 9 Empty
  initialGame = Game startingBoard X (ContinueGame startingBoard)

 