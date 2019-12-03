module Lib
    ( someFunc
    ) where

import Control.Monad
import qualified Data.Map as M
import Data.Char (ord)
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

validMoves :: Position -> Piece -> [Tile] -- [Move]?
validMoves pos side = do
  y <- [0..7]
  x <- [0..7]
  let tile = Tile x y
  guard $ isValidMove pos $ Move side tile
  [tile]

applyMove :: Position -> Move -> Maybe Position
applyMove pos move@(Move side _) = case tryMove pos move of
  [] -> Nothing
  takenTiles -> Just $ foldr (`M.insert` side) pos takenTiles

initialPosition :: Position
initialPosition = M.fromList [
  (Tile 3 3, Black), (Tile 3 4, White), (Tile 4 3, White), (Tile 4 4, Black)]

dumpPosition :: Position -> [Tile] -> IO ()
dumpPosition pos moves = mapM_ (printRow pos) [7,6..0]
  where printRow pos y = putStrLn $ fmap (\x -> printTile pos x y) [0..7]
        printTile pos x y = case M.lookup (Tile x y) pos of
          Nothing -> if elem (Tile x y) moves then '\x00d7' else '.'
          Just Black -> '\x26c0'
          Just White -> '\x26c2'

opposite :: Piece -> Piece
opposite White = Black
opposite Black = White

play :: Position -> Piece -> Bool -> IO ()
play pos side opponentPassed =
  let moves = validMoves pos side
  in if null moves
    then if opponentPassed
      then endMatch pos
      else play pos (opposite side) True
    else do
      dumpPosition pos moves
      tile <- getMove moves
      case applyMove pos $ Move side tile of
        Nothing -> fail "Should not happen"
        Just pos' -> play pos' (opposite side) False
  where endMatch pos =
          let whites = M.size $ M.filter (== White) pos
              blacks = M.size pos - whites
          in do
            dumpPosition pos []
            if whites > blacks
              then putStrLn $ "Whites won " ++ show whites ++ " by " ++ show blacks
              else putStrLn $ "Blacks won " ++ show blacks ++ " by " ++ show whites
        getMove moves = do
          putStr "Input move (x y): "
          [x, ' ', y] <- getLine
          let tile = Tile (ord x - ord '0') (ord y - ord '0')
          if elem tile moves then return tile else getMove moves

someFunc :: IO ()
someFunc = play initialPosition White False