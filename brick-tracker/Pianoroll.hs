{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
module Pianoroll (Pianoroll, defaultPianoroll, renderPianoroll, handlePianorollEvent) where
import           Brick.Types
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

renderPianoroll :: (Ord n, Show n) => n -> Pianoroll -> Bool -> Widget n
renderPianoroll n (Pianoroll cloc) focused =
  viewport n Both $
  (if focused then showCursor n cloc else id) $
  visibleRegion cloc (1, 1) $
  vBox (map (str . show) [127 :: Int, 126 .. 0])

handlePianorollEvent :: BrickEvent n e -> EventM n Pianoroll ()
handlePianorollEvent = \case
  VtyEvent (Key "<down>") -> do
    Location (x, y) <- use prLocation
    when (y < 127) $ prLocation .= Location (x, pred y)
  VtyEvent (Key "<up>") -> do
    Location (x, y) <- use prLocation
    when (y > 0) $ prLocation .= Location (x, succ y)

  _ -> pure ()

