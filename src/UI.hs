module UI (showGUI) where

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import qualified Data.Vector as Vec
import Graphics.UI.Gtk hiding (get, rectangle)
import Graphics.Rendering.Cairo
import Game

tile :: Num a => a
tile = 80

margin = 30
padding = 9

toCanvas i = fromIntegral $ margin + tile * i
toTopLeft i = toCanvas i + padding
toCenter i = toCanvas i + tile / 2
toTile z = (truncate z - margin) `div` tile
boardSize = 2 * margin + 8 * tile

showGUI :: IORef Match -> IO ()
showGUI matchRef = do
  initGUI
  white <- pixbufNewFromFile "Realistic_White_Go_Stone.svg"
  black <- pixbufNewFromFile "Realistic_Go_Stone.svg"

  canvas <- drawingAreaNew
  canvas `on` draw $ drawCanvas canvas white black matchRef
  canvas `on` buttonPressEvent $ handleClick canvas matchRef

  window <- windowNew
  set window [
    windowTitle := "Reversi",
    windowDefaultWidth := boardSize,
    windowDefaultHeight := boardSize,
    windowResizable := False]
  window `on` deleteEvent $ liftIO mainQuit >> return False
  containerAdd window canvas
  widgetShowAll window
  mainGUI

handleClick :: WidgetClass w => w -> IORef Match -> EventM EButton Bool
handleClick canvas matchRef = do
  (x, y) <- eventCoordinates
  liftIO $ do
    match <- readIORef matchRef
    let match' = move match (Tile (toTile x) (toTile y))
    writeIORef matchRef match'
    let MatchSummary isOver whites blacks = summary match'
    when isOver $ do
      md <- messageDialogNew Nothing [DialogModal] MessageInfo ButtonsOk $
        if whites > blacks
          then "White won " ++ show whites ++ " by " ++ show blacks
          else "Black won " ++ show blacks ++ " by " ++ show whites
      set md [ windowTitle := "Game Over" ]
      dialogRun md >> widgetDestroy md
    widgetQueueDraw canvas
    return True

drawCanvas :: DrawingArea -> Pixbuf -> Pixbuf -> IORef Match -> Render ()
drawCanvas canvas white black positionRef = do
  let margin = 30
      cell = 80
  width <- fromIntegral <$> liftIO (widgetGetAllocatedWidth canvas)
  height <- fromIntegral <$> liftIO (widgetGetAllocatedHeight canvas)

  setSourceRGB 0 0.45 0
  rectangle 0 0 width height
  fill

  setSourceRGB 0 0 0
  setLineWidth 2
  mapM_ (\i -> do
    moveTo margin (margin + cell * i)
    lineTo (width - margin) (margin + cell * i)
    moveTo (margin + cell * i) margin
    lineTo (margin + cell * i) (height - margin)
    stroke) [1..7]

  Match (Position board _) side moves <- liftIO $ readIORef positionRef
  mapM_ (\(i, maybePiece) -> case maybePiece of
    Nothing -> return ()
    Just piece ->
      let pixbuf = if piece == White then white else black
      in drawPiece (divMod i 8) pixbuf
    ) $ zip [0..] (Vec.toList board)

  mapM_ (\(Tile x y) -> drawHint x y) moves

drawPiece :: (Int, Int) -> Pixbuf -> Render ()
drawPiece (y,x) pixbuf = do
  save
  translate (toTopLeft x) (toTopLeft y)
  scale 0.25 0.25
  setSourcePixbuf pixbuf 0 0
  paint
  restore

drawHint :: Int -> Int -> Render ()
drawHint x y = do
  arc (toCenter x) (toCenter y) 6 0 (pi * 2)
  stroke
