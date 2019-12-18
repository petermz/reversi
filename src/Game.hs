module Game (
  Match(..), MatchSummary(..), Piece(..), Position(..), Tile(..),
  startMatch, move, isOver, summary,
  validMoves
) where

import Control.Monad
import Data.Char (ord)
import Data.Maybe (fromJust)
import qualified Data.Vector as Vec
import qualified Data.Set as Set

data Piece = Black | White deriving (Eq, Show)
data Tile = Tile Int Int deriving (Eq, Ord, Show)
data Move = Move Piece Tile deriving Show
data Position = Position (Vec.Vector (Maybe Piece)) (Set.Set Tile) deriving Show
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

isValidTile :: Tile -> Bool
isValidTile (Tile x y) = x >= 0 && x < 8 && y >= 0 && y < 8

isValidMove :: Position -> Move -> Bool
isValidMove pos move = not $ null $ tryMove pos move

validMoves :: Position -> Piece -> [Tile] -- [Move]?
validMoves pos@(Position _ candidates) side =
  [tile | tile <- Set.toList candidates, isValidMove pos (Move side tile)]

moveDirections = [ \(Tile x y) -> Tile (x + dx) (y + dy)
  | dx <- [-1..1]
  , dy <- [-1..1]
  , not (dx == 0 && dy == 0) ]

toIndex (Tile x y) = x + y * 8

tryMove :: Position -> Move -> [Tile]
tryMove pos@(Position board _) (Move side tile) = case board Vec.! toIndex tile of
  Just _ -> []
  Nothing -> do
    tileFunc <- moveDirections
    tryMove' pos side tile tileFunc False [tile]

tryMove' :: Position -> Piece -> Tile -> (Tile -> Tile) -> Bool -> [Tile] -> [Tile]
tryMove' pos@(Position board _) side tile tileFunc isMoving takenTiles =
  if isValidTile tile'
    then case board Vec.! toIndex tile' of
      Nothing -> []
      Just piece | piece == side -> if isMoving then takenTiles else []
      Just _ -> tryMove' pos side tile' tileFunc True (tile' : takenTiles)
    else []
  where tile' = tileFunc tile

applyMove :: Position -> Move -> Maybe Position
applyMove pos@(Position board tiles) move@(Move side tile) = case tryMove pos move of
  [] -> Nothing
  takenTiles -> Just $ Position board' tiles'
    where board' = board Vec.// map (\tile -> (toIndex tile, Just side)) takenTiles
          tiles' = Set.union (Set.fromList emptyNeighbors) (Set.delete tile tiles)
          tileNeighbors = filter isValidTile $ map ($ tile) moveDirections
          emptyNeighbors = filter (\t -> board Vec.! toIndex t == Nothing) tileNeighbors

opposite :: Piece -> Piece
opposite White = Black
opposite Black = White

startMatch :: Match
startMatch = Match pos White $ validMoves pos White
  where board = Vec.fromList $
          replicate 27 Nothing ++
          [Just White, Just Black] ++
          replicate 6 Nothing ++
          [Just Black, Just White] ++
          replicate 27 Nothing
        candidates = Set.fromList $
          [Tile x y | x <- [2..5], y<-[2,5]] ++
          [Tile x y | x <- [2,5], y<-[3,4]]
        pos = Position board candidates

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

isOver :: Match -> Bool
isOver (Match _ _ moves) = null moves

summary :: Match -> MatchSummary
summary match@(Match (Position board _) _ moves) = MatchSummary (isOver match) whites blacks
  where whites = Vec.length $ Vec.filter (== Just White) board
        blacks = Vec.length $ Vec.filter (== Just Black) board
