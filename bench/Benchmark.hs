import Data.Maybe
import Game

type Player = Match -> Maybe Tile

nMover :: Int -> Player
nMover n (Match pos side _) =
  if null moves then Nothing
  else Just $ moves !! n'
  where moves = validMoves pos side
        n' = n `mod` length moves

autoPlay :: Player -> Player -> Match -> Match
autoPlay p1 p2 match =
  if isOver match then match
  else autoPlay p2 p1 match'
  where match' = move match mv
        mv = fromMaybe (error "Should not happen") $ p1 match

main =
  let players = nMover <$> [1..30]
      games = autoPlay <$> players <*> players
  in print $ all isOver $ map ($ startMatch) games
