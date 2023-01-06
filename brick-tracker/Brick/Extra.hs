module Brick.Extra (
  renderBottomUp
) where

import           Brick.Types        (Size (Greedy), Widget (Widget),
                                     availHeight, getContext, image, render)
import           Brick.Widgets.Core (fill, raw, (<=>))
import           Graphics.Vty       (cropTop, emptyImage, imageHeight, vertJoin)

renderBottomUp :: [Widget n] -> Widget n
renderBottomUp ws = Widget Greedy Greedy $
    render . (fill ' ' <=>) . raw =<< go ws . availHeight =<< getContext
  where
    go [] _ = pure emptyImage
    go (c:cs) remainingHeight = do
        img <- image <$> render c
        let newRemainingHeight = remainingHeight - imageHeight img
        case compare newRemainingHeight 0 of
            EQ -> pure img
            LT -> pure $ cropTop remainingHeight img
            GT -> flip vertJoin img <$> go cs newRemainingHeight
