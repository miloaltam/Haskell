

module Computer (
    getRandomMove
) where

--import board and components
import Board (Board, mkBoard, mkPlayer, mkOpponent,size, row, column, mark, isEmpty, isMarked, isMarkedBy, marker, isFull, isWonBy, isDraw, isGameOver, boardToStr)

import System.Random (randomRIO)

--get random computer move
getRandomMove :: Board -> IO (Int, Int)
getRandomMove board = do
    x <- randomRIO (1, size board)
    y <- randomRIO (1, size board)
    if isEmpty x y board
        then return (x, y)
        else getRandomMove board

