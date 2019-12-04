module Main where

import Data.IORef
import Game
import UI

main = newIORef startMatch >>= showGUI
