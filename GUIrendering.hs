module GUIRendering where 

import Graphics.Gloss

import Tictacgame


---------------------------- CONSTANTS ---------------------------

-- colour constants 
boardGridColor = makeColorI 255 255 255 255
playerXColor = makeColorI 255 50 50 255
playerOColor = makeColorI 50 100 255 255
tieColor = greyN 0.5

-- if there is a winner, change color of board to:
outcomeColor X = playerXColor
outcomeColor O = playerOColor


---------------------------- RENDERING BOARD FUNCTIONS ---------------------------


-- renders picture based on current state of game
gameAsPicture :: Game -> Picture 
gameAsPicture (Game cell player result) = translate (fromIntegral screenWidth * (-0.5))
                                          (fromIntegral screenHeight * (-0.5))
                                          frame 
    where frame = case getGameState cell of 
                    ContinueGame board -> boardAsRunningPicture board
                    TiedGame board -> boardAsTiedPicture board
                    EndOfGame player board -> boardAsGameOverPicture player board

-- if game is running:
boardAsRunningPicture board =
    pictures [ color playerXColor $ xCellsOfBoard board
             , color playerOColor $ oCellsOfBoard board
             , color boardGridColor $ boardGrid
             ]
-- when you need to show board at end game state
boardAsPicture board =
    pictures [ xCellsOfBoard board
             , oCellsOfBoard board
             , boardGrid
             ]
-- board when game over and there is winner
boardAsGameOverPicture winner board = color (outcomeColor winner) (boardAsPicture board)
-- board when game over but it is tie state
boardAsTiedPicture board = color tieColor (boardAsPicture board)


---------------------------- RENDERING EACH LAYER OF BOARD ---------------------------
-- form the three layers of our picture: X cells, O cells, grid


-- function which places picture in appropriate cell of screen
snapPictureToCell picture (row, column) = translate x y picture
    where x = fromIntegral column * cellWidth + cellWidth * 0.5
          y = fromIntegral row * cellHeight + cellHeight * 0.5

-- x CELL RENDERING:

-- picture of X cell:
xCell :: Picture
xCell = pictures [ rotate 45.0 $ rectangleSolid side 10.0
                 , rotate (-45.0) $ rectangleSolid side 10.0
                 ]
    where side = min cellWidth cellHeight * 0.75

xCellsOfBoard :: [Cell] -> Picture
xCellsOfBoard board = cellsOfBoard board X xCell

-- O CELL RENDERING

-- picture of O cell:
oCell :: Picture
oCell = thickCircle radius 10.0
    where radius = min cellWidth cellHeight * 0.25

oCellsOfBoard :: [Cell] -> Picture 
oCellsOfBoard board = cellsOfBoard board O oCell 

-- BOARD GRID RENDERING
boardGrid :: Picture
boardGrid =
    pictures
    $ concatMap (\i -> [ line [ (i * cellWidth, 0.0)
                              , (i * cellWidth, fromIntegral screenHeight)
                              ]
                       , line [ (0.0,                      i * cellHeight)
                              , (fromIntegral screenWidth, i * cellHeight)
                              ]
                       ])
      [0.0 .. fromIntegral 3]

cellsOfBoard :: [Cell] -> Player -> Picture -> Picture
cellsOfBoard board player cellPicture =
    pictures
    $ map (snapPictureToCell cellPicture) (listToIndex board player)


-- converts list position to index of board (Row, Column) format
posIndex :: Int -> (Int, Int)
posIndex 0 = (0,0)
posIndex 1 = (0,1)
posIndex 2 = (0,2)
posIndex 3 = (1,0)
posIndex 4 = (1,1)
posIndex 5 = (1,2)
posIndex 6 = (2,0)
posIndex 7 = (2,1)
posIndex 8 = (2,2)


-- returns the list of indices that are occupied by player 
listToIndex :: [Cell] -> Player -> [(Int, Int)]
listToIndex board player = foldr (\x y -> if (board !! x == (Occupied player)) then (posIndex x):y else y) [] [0..8] 
