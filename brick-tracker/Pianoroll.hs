{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Pianoroll (Pianoroll, defaultPianoroll, getPitch, renderPianoroll, handlePianorollEvent) where
import           Brick.Types
import           Brick.Widgets.Border
import           Brick.Widgets.Core
import           Control.Monad         (when)
import           Graphics.Vty.Patterns (pattern Key)
import           Lens.Micro.Mtl        (use, (.=))
import           Lens.Micro.TH         (makeLenses)

data Pianoroll = Pianoroll {
  _prLocation :: Location
} deriving (Eq, Read, Show)

makeLenses ''Pianoroll

defaultPianoroll :: Pianoroll
defaultPianoroll = Pianoroll (Location (0, 0))

getPitch :: Floating freq => Pianoroll -> freq
getPitch (Pianoroll (Location (_, y))) =
  let p = 127 - y in 440 * 2 ** (fromIntegral (p + 3 - 6*12) / 12)

renderPianoroll :: (Ord n, Show n) => n -> n -> Pianoroll -> Bool -> Widget n
renderPianoroll lvp n (Pianoroll cloc@(Location (_, y))) focused =
  labels <+> vBorder <+> roll
 where
  labels = hLimit 3 . viewport lvp Vertical . vBox $ mkLabel <$> [0 .. 127] where
    mkLabel i = maybeVisible i $ str $ show i
  roll = viewport n Both $
    (if focused then showCursor n cloc else id) $
    visibleRegion cloc (1, 1) $
    vBox (map (str . show) [127 :: Int, 126 .. 0])
  maybeVisible i | i == y = visible
                 | otherwise = id

handlePianorollEvent :: BrickEvent n e -> EventM n Pianoroll ()
handlePianorollEvent = \case
  VtyEvent (Key "<down>") -> do
    Location (x, y) <- use prLocation
    when (y < 127) $ prLocation .= Location (x, succ y)
  VtyEvent (Key "<next>") -> do
    Location (x, y) <- use prLocation
    when (y < 127 - 12) $ prLocation .= Location (x, y+12)
  VtyEvent (Key "<up>") -> do
    Location (x, y) <- use prLocation
    when (y > 0) $ prLocation .= Location (x, pred y)
  VtyEvent (Key "<prior>") -> do
    Location (x, y) <- use prLocation
    when (y > 0 + 12) $ prLocation .= Location (x, y-12)

  _ -> pure ()

