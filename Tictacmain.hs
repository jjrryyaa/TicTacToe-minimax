module Tictacmain where

import Graphics.Gloss
import Graphics.Gloss.Data.Color

import Tictacgame
import GUIRendering
import Tictacplay

window = InWindow "Tic Tac Toe" (screenWidth, screenHeight) (100, 100)
backgroundColor = makeColor 0 0 0 255

mainGUI :: IO ()
mainGUI = play window backgroundColor 30 initialGame gameAsPicture transformGame (const id)