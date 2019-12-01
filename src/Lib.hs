module Lib
    ( someFunc
    ) where

import Control.Monad
import qualified Data.Map as M
import Data.Maybe (fromJust)

data Piece = Black | White deriving (Eq, Show)
data Tile = Tile Int Int deriving (Eq, Ord, Show)
data Move = Move Piece Tile deriving Show
type Position = M.Map Tile Piece
data Match = Match Position Piece deriving Show -- Piece denotes the side that moves next

isValidTile :: Tile -> Bool
isValidTile (Tile x y) = x >= 0 && x < 8 && y >= 0 && y < 8

moveDirections = [
    \(Tile x y) -> Tile (x-1) y,
    \(Tile x y) -> Tile (x+1) y,
    \(Tile x y) -> Tile x (y-1),
    \(Tile x y) -> Tile x (y+1),
    \(Tile x y) -> Tile (x-1) (y-1),
    \(Tile x y) -> Tile (x-1) (y+1),
    \(Tile x y) -> Tile (x+1) (y-1),
    \(Tile x y) -> Tile (x+1) (y+1)
  ]

tryMove' :: Position -> Piece -> Tile -> (Tile -> Tile) -> Bool -> [Tile] -> [Tile]
tryMove' pos side tile tileFunc isMoving takenTiles =
  if isValidTile tile'
    then case M.lookup tile' pos of
      Nothing -> []
      Just piece | piece == side -> if isMoving then takenTiles else []
      Just _ -> tryMove' pos side tile' tileFunc True (tile' : takenTiles)
    else []
  where tile' = tileFunc tile

tryMove :: Position -> Move -> [Tile]
tryMove pos (Move side tile) = case M.lookup tile pos of
  Just _ -> []
  Nothing -> do
    tileFunc <- moveDirections
    tryMove' pos side tile tileFunc False [tile]

isValidMove :: Position -> Move -> Bool
isValidMove pos move = not $ null $ tryMove pos move

validMoves :: Position -> Piece -> [Move]
validMoves pos side = do
  y <- [0..7]
  x <- [0..7]
  let move = Move side $ Tile x y
  guard $ isValidMove pos move
  [move]

applyMove :: Position -> Move -> Maybe Position
applyMove pos move@(Move side _) = case tryMove pos move of
  [] -> Nothing
  takenTiles -> Just $ foldr (`M.insert` side) pos takenTiles

initialPosition :: Position
initialPosition = M.fromList [
  (Tile 3 3, Black), (Tile 3 4, White), (Tile 4 3, White), (Tile 4 4, Black)]

dumpPosition :: Position -> IO ()
dumpPosition pos = mapM_ (printRow pos) [7,6..0]
  where printRow pos y = putStrLn $ fmap (\x -> printTile pos x y) [0..7]
        printTile pos x y = case M.lookup (Tile x y) pos of
          Nothing -> '.'
          Just Black -> 'x'
          Just White -> 'o'

someFunc :: IO ()
someFunc = do
  let pos0 = initialPosition
  dumpPosition pos0
  print $ validMoves pos0 Black
  let pos1 = fromJust $ applyMove pos0 $ Move Black $ Tile 4 2
  dumpPosition pos1
  print $ validMoves pos1 White
  let pos2 = fromJust $ applyMove pos1 $ Move White $ Tile 5 2
  dumpPosition pos2
  print $ validMoves pos2 Black
  let pos3 = fromJust $ applyMove pos2 $ Move Black $ Tile 6 2
  dumpPosition pos3
