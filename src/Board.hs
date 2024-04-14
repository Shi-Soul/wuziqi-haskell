module Board where

import Types

-- | Create an empty board
emptyBoard :: Board
emptyBoard = replicate 15 $ replicate 15 Empty

-- | Check if a position is valid
isValidPosition :: Position -> Bool
isValidPosition (x, y) = x >= 1 && x <= 15 && y >= 1 && y <= 15

-- | Check if a position is empty
isPositionEmpty :: Board -> Position -> Bool
isPositionEmpty board (x, y) = (board !! (x - 1) !! (y - 1)) == Empty

-- | Place a stone on the board
placeStone :: Player -> Position -> Board -> Board
placeStone player (x, y) board =
  let newBoard = take (x - 1) board ++ [take (y - 1) (board !! (x - 1)) ++ [Filled player] ++ drop y (board !! (x - 1))] ++ drop (x) board
  in if isValidPosition (x, y) && isPositionEmpty board (x, y) then newBoard else board

-- | Check if a player has won
hasWon :: Player -> Board -> Bool
hasWon _ [] = False
hasWon player (x:xs) =
    -- horizontal: for each row, check if there is a winning line
    let horizontal = any (checkLine player) (x:xs)
        -- vertical = any (checkLine player (map ((x:xs) !!) )) [0..14]
        vertical = any (\i -> checkLine player (map (!! i) (x:xs) ) ) [0..14]
        -- diagonal1 = checkLine player (head x) (map (xs !!) [0..14]) 
        -- diagonal2 = checkLine player (head x) (map (xs !!) [2..13])
        diagonals_1 = [ [ (x:xs) !! i !! j | i <- [0..14], j <- [0..14], i - j == d ] | d <- [-14..14] ]
        diagonal1 = any (checkLine player) diagonals_1
        diagonals_2 = [ [ (x:xs) !! i !! j | i <- [0..14], j <- [0..14], i + j == d ] | d <- [-14..14] ]
        diagonal2 = any (checkLine player) diagonals_2
    in horizontal || vertical || diagonal1 || diagonal2

-- | Check if there are any empty positions left on the board
areAnyPositionsEmpty :: Board -> Bool
areAnyPositionsEmpty = any (any (== Empty))

-- | Check a line for a win
checkLine :: Player -> [Stone] -> Bool
checkLine _ [] = False
checkLine player stones = isWinningLine player (take 5 stones) || checkLine player (drop 1 stones)

-- | Check if a line is a winning line
isWinningLine :: Player -> [Stone] -> Bool
isWinningLine player stones = all (== Filled player) stones && length stones == 5