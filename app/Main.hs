module Main where

import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Lib

showGUI :: IO ()
showGUI = do
  initGUI

  window <- windowNew
  set window [ windowTitle := "Reversi" ]
  window `on` deleteEvent $ liftIO mainQuit >> return False

  widgetShowAll window
  mainGUI

main :: IO ()
main = someFunc
