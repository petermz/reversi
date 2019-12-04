module Game (
  Match(..), MatchSummary(..), Piece(..), Position, Tile(..),
  startMatch, move, summary
) where

import Control.Monad
import qualified Data.Map as M
import Data.Char (ord)
import Data.Maybe (fromJust)

data Piece = Black | White deriving (Eq, Show)
data Tile = Tile Int Int deriving (Eq, Ord, Show)
data Move = Move Piece Tile deriving Show
type Position = M.Map Tile Piece
data Match = Match
  Position
  Piece     -- denotes the side that moves next
  [Tile]    -- valid moves for that side
  deriving Show
data MatchSummary = MatchSummary
  Bool      -- True if match is over
  Int       -- number of white pieces
  Int       -- number of black pieces
  deriving Show

isValidMove :: Position -> Move -> Bool
isValidMove pos move = not $ null $ tryMove pos move

validMoves :: Position -> Piece -> [Tile] -- [Move]?
validMoves pos side = do
  y <- [0..7]
  x <- [0..7]
  let tile = Tile x y
  guard $ isValidMove pos $ Move side tile
  [tile]

tryMove :: Position -> Move -> [Tile]
tryMove pos (Move side tile) = case M.lookup tile pos of
  Just _ -> []
  Nothing -> do
    tileFunc <- moveDirections
    tryMove' pos side tile tileFunc False [tile]
    where moveDirections = [ \(Tile x y) -> Tile (x + dx) (y + dy)
            | dx <- [-1..1]
            , dy <- [-1..1]
            , not (dx == 0 && dy == 0) ]

tryMove' :: Position -> Piece -> Tile -> (Tile -> Tile) -> Bool -> [Tile] -> [Tile]
tryMove' pos side tile tileFunc isMoving takenTiles =
  if isValidTile tile'
    then case M.lookup tile' pos of
      Nothing -> []
      Just piece | piece == side -> if isMoving then takenTiles else []
      Just _ -> tryMove' pos side tile' tileFunc True (tile' : takenTiles)
    else []
  where tile' = tileFunc tile
        isValidTile (Tile x y) = x >= 0 && x < 8 && y >= 0 && y < 8

applyMove :: Position -> Move -> Maybe Position
applyMove pos move@(Move side _) = case tryMove pos move of
  [] -> Nothing
  takenTiles -> Just $ foldr (`M.insert` side) pos takenTiles

opposite :: Piece -> Piece
opposite White = Black
opposite Black = White

startMatch :: Match
startMatch = Match pos White $ validMoves pos White
  where pos = M.fromList [
          (Tile 3 3, White), (Tile 3 4, Black), (Tile 4 3, Black), (Tile 4 4, White)]

move :: Match -> Tile -> Match
move match@(Match pos side _) tile =
  case applyMove pos $ Move side tile of
    Nothing -> match
    Just pos' -> Match pos' side' moves'
      where (side', moves') = if null opponentMoves
              then (side, validMoves pos' side)
              else (opponent, opponentMoves)
            opponent = opposite side
            opponentMoves = validMoves pos' opponent

summary :: Match -> MatchSummary
summary (Match pos side moves) = MatchSummary (null moves) whites blacks
  where whites = M.size $ M.filter (== White) pos
        blacks = M.size pos - whites
