module Game where

import Types
import Board
import Player

-- | Play a game of Wuziqi
playGame :: IO ()
playGame = do
  let board = emptyBoard
  let players = cycle [Black, White]
--   let currentPlayer = head players
--   let nextPlayer = tail players
  putStrLn "Welcome to Wuziqi!"
  putStrLn "Enter your moves as (x y), where x and y are the coordinates of the board."
  putStrLn "The game ends when one player has won or there are no more moves possible."
  showBoard board
  let gameLoop p b = do
        let currentPlayer = head p
        position <- getNextMove currentPlayer b
        let new_board = placeStone currentPlayer position b
        if hasWon currentPlayer new_board then do
          putStrLn $ "Player " ++ show currentPlayer ++ " has won!"
          showBoard new_board
        else if not (areAnyPositionsEmpty new_board) then do
          putStrLn "The game is a draw!"
          showBoard new_board
        else do
          putStrLn $ "Player " ++ show currentPlayer ++ " placed a stone at (" ++ show (fst position) ++ ", " ++ show (snd position) ++ ")."
          showBoard new_board
          gameLoop (tail p) new_board
  gameLoop players board