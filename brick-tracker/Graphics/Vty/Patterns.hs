{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns    #-}
module Graphics.Vty.Patterns (pattern Key) where

import           Data.List                 (sort)
import           Graphics.Vty.Input.Events (Event (EvKey),
                                            Key (KChar, KDown, KEnter, KLeft
                                                , KPageDown, KPageUp, KRight, KUp),
                                            Modifier (MAlt, MCtrl, MMeta, MShift))

pattern Key :: String -> Event
pattern Key k <- (match -> Just k)

match :: Event -> Maybe String
match = \case
  EvKey k (concatMap f . sort -> m) -> case k of
    KChar '\t' -> Just $ m ++ "TAB"
    KChar k    -> Just $ m ++ pure k
    KEnter     -> Just $ m ++ "RET"
    KPageUp    -> Just $ m ++ "<prior>"
    KUp        -> Just $ m ++ "<up>"
    KDown      -> Just $ m ++ "<down>"
    KPageDown  -> Just $ m ++ "<next>"
    KLeft      -> Just $ m ++ "<left>"
    KRight     -> Just $ m ++ "<right>"
    _          -> Nothing
  _ -> Nothing
 where
  f MCtrl  = "C-"
  f MMeta  = "M-"
  f MShift = "S-"
  f MAlt   = "A-"
