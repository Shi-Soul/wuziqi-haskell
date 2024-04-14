module Types where

data Player = Black | White deriving (Eq, Show)

type Position = (Int, Int)

data Stone = Empty | Filled Player deriving (Eq, Show)

type Board = [[Stone]]