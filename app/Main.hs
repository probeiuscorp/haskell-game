{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import SDL.Vect
import Control.Monad (unless)

-- Adapted from https://github.com/haskell-game/sdl2/blob/master/examples/twinklebear/Lesson04.hs
main :: IO ()
main = do
    SDL.initialize [ SDL.InitVideo ]
    let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }
    
    window <- SDL.createWindow "Wheel of Time" winConfig
    renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

    let loop = do
          SDL.present renderer
          quit <- fmap (\ev -> case SDL.eventPayload ev of
              SDL.QuitEvent -> True
              SDL.KeyboardEvent e -> SDL.keyboardEventKeyMotion e ==  SDL.Pressed
              SDL.MouseButtonEvent e -> SDL.mouseButtonEventMotion e == SDL.Pressed
              _ -> False) SDL.waitEvent
          unless quit loop
    loop

    SDL.destroyRenderer renderer
    SDL.destroyWindow window
    SDL.quit