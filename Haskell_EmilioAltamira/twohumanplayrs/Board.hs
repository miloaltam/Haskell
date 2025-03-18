module Board (
    mkBoard,
    mkPlayer,
    mkOpponent,
    Board(..),
    size,
    row,
    column,
    mark,
    isEmpty,
    isMarked,
    isMarkedBy,
    marker,
    isFull,
    isWonBy,
    isDraw,
    isGameOver,
    boardToStr,
    Player(..)
) where

import Data.List (transpose)

-- Define the data type for the player
data Player = Player1 | Player2 deriving (Eq, Show)

-- Define the data type for the board
data Board = Board {
    size :: Int,
    grid :: [[Maybe Player]]
}

-- Create an empty nxn board
mkBoard :: Int -> Board
mkBoard n = Board {
    size = n,
    grid = replicate n (replicate n Nothing)
}

-- Create the first player
mkPlayer :: Player
mkPlayer = Player1

-- Create the opponent
mkOpponent :: Player
mkOpponent = Player2

-- Return a row of the board
row :: Int -> Board -> [Maybe Player]
row y board = (grid board) !! (y - 1)

-- Return a column of the board
column :: Int -> Board -> [Maybe Player]
column x board = map (!! (x - 1)) (grid board)

-- Mark a place on the board by a player
mark :: Int -> Int -> Board -> Player -> Board
mark x y board player = 
    if isEmpty x y board
        then
            let newRow = take (y - 1) (grid board) ++ [replaceAtIndex (x - 1) (Just player) (row y board)] ++ drop y (grid board)
            in board { grid = newRow }
        else board

-- Check if a place on the board is empty
isEmpty :: Int -> Int -> Board -> Bool
isEmpty x y board = case ((grid board) !! (y - 1)) !! (x - 1) of
                        Nothing -> True
                        _ -> False

-- Check if a place on the board is marked
isMarked :: Int -> Int -> Board -> Bool
isMarked x y board = not (isEmpty x y board)

-- Check if a place on the board is marked by a specific player
isMarkedBy :: Int -> Int -> Board -> Player -> Bool
isMarkedBy x y board player = case ((grid board) !! (y - 1)) !! (x - 1) of
                                    Just p -> p == player
                                    _ -> False

-- Get the player who marked a place on the board
marker :: Int -> Int -> Board -> Maybe Player
marker x y board = ((grid board) !! (y - 1)) !! (x - 1)

-- Check if the board is full
isFull :: Board -> Bool
isFull board = all (\row -> all (/= Nothing) row) (grid board)


-- Check if the game is won by a player
isWonBy :: Board -> Player -> Bool
isWonBy board player =
    any (all (== Just player)) (grid board) ||   -- Horizontal win
    any (all (== Just player)) (transpose $ grid board) ||  -- Vertical win
    any (hasFiveInARow player) (grid board) || -- Five in a row horizontally
    any (hasFiveInARow player) (transpose $ grid board) || -- Five in a row vertically
    any (hasFiveInARow player) (allDiagonals $ grid board) || -- Five in a row diagonally
    any (hasFiveInARow player) (allDiagonals $ reverseGrid $ grid board) -- Five in a row diagonally (reverse)

-- Helper function to check if there are five in a row
hasFiveInARow :: Player -> [Maybe Player] -> Bool
hasFiveInARow player row =
    any (== replicate 5 (Just player)) (chunksOf 5 row)

-- Split a list into chunks of a given size
chunksOf :: Int -> [a] -> [[a]]
chunksOf _ [] = []
chunksOf n xs = take n xs : chunksOf n (drop n xs)

-- Get all diagonals of a matrix
allDiagonals :: [[a]] -> [[a]]
allDiagonals m = cols ++ rows
  where
    cols = transpose $ zipWith drop [0..] m ++ zipWith drop [1..] m ++ zipWith drop [0..] (reverse m) ++ zipWith drop [1..] (reverse m)
    rows = transpose cols

-- Reverse the grid
reverseGrid :: [[a]] -> [[a]]
reverseGrid = map reverse

-- Check if the game ended in a draw
isDraw :: Board -> Bool
isDraw board = isFull board && not (isGameOver board)

-- Check if the game is over
isGameOver :: Board -> Bool
isGameOver board = isWonBy board Player1 || isWonBy board Player2 || isDraw board

-- Replace an element at a specific index in a list
replaceAtIndex :: Int -> a -> [a] -> [a]
replaceAtIndex _ _ [] = []
replaceAtIndex i newVal (x:xs)
    | i == 0 = newVal:xs
    | otherwise = x:replaceAtIndex (i - 1) newVal xs

-- Convert the board to a string for printing
boardToStr :: Board -> String
boardToStr board =
    let n = size board
        horizontalBorder = concat (replicate (n + 1) "+---") ++ "+\n"--Board border
        numberedColumns = "  " ++ concat (map (\i -> " " ++ show (i `mod` 10) ++ "  ") [1..n]) ++ "\n"--numbers Columns
        numberedRows = concat [show (i `mod` 10) ++ "|" ++ concatMap markToStr (row i board) ++ "\n" | i <- [1..n]]--numbered Rows
    in unlines $ numberedColumns : horizontalBorder : numberedRows : [horizontalBorder]
    where
        markToStr :: Maybe Player -> String
        markToStr Nothing = ". "  -- Empty
        markToStr (Just Player1) = "X "  -- Player 1's mark
        markToStr (Just Player2) = "O "  -- Player 2's mark
