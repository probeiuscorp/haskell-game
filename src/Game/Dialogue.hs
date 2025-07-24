module Game.Dialogue where

import Game.Prelude
import Game.UI.UI
import qualified SDL

bDialogueUI :: Behavior UI
bDialogueUI = pure $
  box (p 100 x . p 20 y . h (px 400) . bg (SDL.V4 60 60 60 255)) $
    box (p 18 xy . w (ratio 1) . bg (SDL.V4 100 100 100 255)) $
      text id "Photo goes here"
