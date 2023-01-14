module Main (main) where

import Control.Monad (void)
import Tracker (tracker)

main :: IO ()
main = void $ tracker 57110
