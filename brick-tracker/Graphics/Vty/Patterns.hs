{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vty.Patterns (pattern Key) where

import           Data.List    (sort)
import           Graphics.Vty (Event (EvKey),
                               Key (KChar, KDown, KLeft, KRight, KUp),
                               Modifier (MAlt, MCtrl, MMeta, MShift))

pattern Key :: String -> Event
pattern Key k <- (match -> Just k)

match :: Event -> Maybe String
match = \case
  EvKey (KChar k) mods -> Just $ concatMap f (sort mods) ++ [k]
  EvKey KUp mods       -> Just $ concatMap f (sort mods) ++ "<up>"
  EvKey KDown mods     -> Just $ concatMap f (sort mods) ++ "<down>"
  EvKey KLeft mods     -> Just $ concatMap f (sort mods) ++ "<left>"
  EvKey KRight mods    -> Just $ concatMap f (sort mods) ++ "<right>"
  _                    -> Nothing
 where
  f MCtrl  = "C-"
  f MMeta  = "M-"
  f MShift = "S-"
  f MAlt   = "A-"
