module Tictacplay where 

import Tictacgame 

import Data.Array
import Graphics.Gloss.Interface.Pure.Game

-- transform game: takes mouse event -> transforms picture based on game state
transformGame (EventKey (MouseButton LeftButton) Up _ mousePos) (Game board player result) =
    case result of
      ContinueGame _ -> playerTurn (Game board player result) $ mousePosAsCellCoord mousePos
      EndOfGame _ _ -> initialGame
      TiedGame _ -> initialGame
transformGame _ game = game

-- transforms screen coordinates to cell coordinates 
mousePosAsCellCoord :: (Float, Float) -> (Int, Int)
mousePosAsCellCoord (x, y) = ( floor ((y + (fromIntegral screenHeight * 0.5)) / cellHeight)
                             , floor ((x + (fromIntegral screenWidth * 0.5)) / cellWidth)
                             )

-- is the cell coordinate in a valid range
isCoordCorrect = inRange ((0, 0), (2,2))

-- cell coord to list index:
indexToPos :: (Int, Int) -> Int
indexToPos (0,0) = 0
indexToPos (0,1) = 1
indexToPos (0,2) = 2
indexToPos (1,0) = 3
indexToPos (1,1) = 4
indexToPos (1,2) = 5
indexToPos (2,0) = 6
indexToPos (2,1) = 7
indexToPos (2,2) = 8


playerTurn :: Game -> (Int, Int) -> Game
playerTurn (Game board player result) cellCoord
    | isCoordCorrect cellCoord && board !! (indexToPos cellCoord) == Empty =
        (Game (addOnBoard cellCoord player board) (nextPlayer player) (getGameState (addOnBoard cellCoord player board)) )
    | otherwise = (Game board player result)


addOnBoard :: (Int, Int) -> Player -> [Cell] -> [Cell]
addOnBoard (r,c) player board = boardArray (indexToPos (r,c)) board player

-- puts the player's move at the correct index, returns the new board
boardArray :: Int -> [Cell] -> Player -> [Cell]
boardArray index board player =  (take index board) ++ [Occupied player] ++ (drop (index+1) board)