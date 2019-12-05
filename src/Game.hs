module Game (
  Match(..), MatchSummary(..), Piece(..), Position, Tile(..),
  startMatch, move, isOver, summary,
  validMoves
) where

import Control.Monad
import qualified Data.Vector as Vec
import Data.Char (ord)
import Data.Maybe (fromJust)

data Piece = Black | White deriving (Eq, Show)
data Tile = Tile Int Int deriving (Eq, Ord, Show)
data Move = Move Piece Tile deriving Show
type Position = Vec.Vector (Maybe Piece)
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

moveDirections = [ \(Tile x y) -> Tile (x + dx) (y + dy)
  | dx <- [-1..1]
  , dy <- [-1..1]
  , not (dx == 0 && dy == 0) ]

toIndex (Tile x y) = x + y * 8

tryMove :: Position -> Move -> [Tile]
tryMove pos (Move side tile) = case pos Vec.! toIndex tile of
  Just _ -> []
  Nothing -> do
    tileFunc <- moveDirections
    tryMove' pos side tile tileFunc False [tile]

tryMove' :: Position -> Piece -> Tile -> (Tile -> Tile) -> Bool -> [Tile] -> [Tile]
tryMove' pos side tile tileFunc isMoving takenTiles =
  if isValidTile tile'
    then case pos Vec.! toIndex tile' of
      Nothing -> []
      Just piece | piece == side -> if isMoving then takenTiles else []
      Just _ -> tryMove' pos side tile' tileFunc True (tile' : takenTiles)
    else []
  where tile' = tileFunc tile
        isValidTile (Tile x y) = x >= 0 && x < 8 && y >= 0 && y < 8

applyMove :: Position -> Move -> Maybe Position
applyMove pos move@(Move side _) = case tryMove pos move of
  [] -> Nothing
  takenTiles -> Just $ pos Vec.// map (\tile -> (toIndex tile, Just side)) takenTiles

opposite :: Piece -> Piece
opposite White = Black
opposite Black = White

startMatch :: Match
startMatch = Match pos White $ validMoves pos White
  where pos = Vec.fromList $
          replicate 27 Nothing ++
          [Just White, Just Black] ++
          replicate 6 Nothing ++
          [Just Black, Just White] ++
          replicate 27 Nothing

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
summary match@(Match pos _ moves) = MatchSummary (isOver match) whites blacks
  where whites = Vec.length $ Vec.filter (== Just White) pos
        blacks = Vec.length $ Vec.filter (== Just Black) pos
