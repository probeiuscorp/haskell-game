{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import qualified SDL
import SDL.Vect
import Control.Monad (unless)

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp

renderTexture :: SDL.Renderer -> SDL.Texture -> IO ()
renderTexture renderer texture = do
  ti <- SDL.queryTexture texture
  let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
  SDL.copy renderer texture Nothing (Just $ SDL.Rectangle (P $ V2 300 300) (V2 w h))

-- Adapted from https://github.com/haskell-game/sdl2/blob/master/examples/twinklebear/Lesson04.hs
main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }

  window <- SDL.createWindow "Wheel of Time" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  image <- getDataFileName "blast.bmp" >>= loadTexture renderer

  let loop = do
    renderTexture renderer image
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
