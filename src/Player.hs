module Player where

import Types
import Board

-- | Get the next move for a player
getNextMove :: Player -> Board -> IO Position
getNextMove _ [] = error "No more moves possible"
getNextMove player board = do
  putStrLn $ "Player " ++ show player ++ "'s turn:"
--   showBoard board
  putStrLn "Enter your move (x,y): "
  input <- getLine
  let (x, y) = (read input :: (Int, Int))
  if isValidPosition (x, y) && isPositionEmpty board (x, y) then return (x, y) else do
    putStrLn "Invalid move, try again."
    getNextMove player board

-- | Show the board
showBoard :: Board -> IO ()
showBoard board = do
  putStrLn $ unlines $ map (concatMap showStone) board

showStone :: Stone -> String
showStone Empty = "."
showStone (Filled Black) = "B"
showStone (Filled White) = "W"