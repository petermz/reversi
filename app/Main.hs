module Main where

import Control.Monad.IO.Class
import Graphics.UI.Gtk
import Lib

main :: IO ()
main = do
  initGUI
  
  window <- windowNew
  set window [ windowTitle := "Reversi" ]
  window `on` deleteEvent $ liftIO mainQuit >> return False
    
  widgetShowAll window
  mainGUI
