module Main where

import Data.IORef
import Lib
import UI

main = newIORef startMatch >>= showGUI
