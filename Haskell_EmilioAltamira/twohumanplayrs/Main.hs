module Main where

import Board
    ( boardToStr,
      isDraw,
      isEmpty,
      isGameOver,
      isWonBy,
      mark,
      mkBoard,
      mkPlayer,
      mkOpponent,
      Board(size),
      Player(..) )

--  print board state
printBoard :: Board -> IO ()
printBoard board = putStrLn (boardToStr board)

-- Function to read a pair of 1-based indices (x, y) for a player
readXY :: Board -> Player -> IO (Int, Int)
readXY board player = do
    putStr ("Player " ++ show player ++ ", enter row and column (e.g., '3 5'): ")
    input <- getLine
    let [x, y] = map read (words input)
    if x >= 1 && x <= size board && y >= 1 && y <= size board && isEmpty x y board
        then return (x, y)
        else do
            putStrLn "Invalid input! Please enter a valid empty cell."
            readXY board player

-- play a game of Omok 
playGame :: Board -> Player -> IO ()
playGame board player = do
    printBoard board
    if isGameOver board
        then handleGameOver board
        else do
            (x, y) <- readXY board player
            let newBoard = mark x y board player
            if isWonBy newBoard player
                then handleGameOver newBoard
                else playGame newBoard (if player == mkPlayer then mkOpponent else mkPlayer)




--  handle the end of the game
handleGameOver :: Board -> IO ()
handleGameOver board
    | isWonBy board Player1 = putStrLn "Player 1 wins!"
    | isWonBy board Player2 = putStrLn "Player 2 wins!"
    | isDraw board = putStrLn "It's a draw!"
    | otherwise = putStrLn "Unknown game state."

-- start the game
main :: IO ()
main = do
    putStrLn "Welcome to Omok!"
    let boardSize = 15 -- Change this if you want a different board size
    let initialBoard = mkBoard boardSize
    playGame initialBoard mkPlayer

