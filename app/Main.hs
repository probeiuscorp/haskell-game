{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import qualified SDL
import SDL.Vect
import Control.Monad (unless, join)
import qualified Data.IORef as R
import Data.Int
import Data.Functor ((<&>))
import Reactive.Banana (Event)
import Reactive.Banana.Frameworks (newEvent, compile, actuate, reactimate, MonadIO (liftIO))
import Data.Traversable (for)

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return

loadTexture :: SDL.Renderer -> FilePath -> IO SDL.Texture
loadTexture renderer path = do
  bmp <- SDL.loadBMP path
  SDL.createTextureFromSurface renderer bmp <* SDL.freeSurface bmp

untilM :: Monad m => m Bool -> m ()
untilM act = go
  where go = (`unless` go) =<< act

-- | In do blocks, `let` is particular about the indentation of nested do blocks
-- Rewrite to binds to avoid double indenting
is :: Applicative m => a -> m a
is = pure

-- Adapted from https://github.com/haskell-game/sdl2/blob/master/examples/twinklebear/Lesson04.hs
main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  let winConfig = SDL.defaultWindow { SDL.windowInitialSize = V2 800 600 }

  window <- SDL.createWindow "Wheel of Time" winConfig
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  image <- getDataFileName "blast.bmp" >>= loadTexture renderer

  handleEvent <- R.newIORef (undefined :: SDL.Event -> IO ())
  handlePaint <- R.newIORef (pure () :: IO ())
  network <- compile $ do
    (eMouseLocation :: Event (Point V2 Int32), setMouseLocation) <- newEvent

    ePaint <- is $ eMouseLocation <&> \pos -> do
      ti <- SDL.queryTexture image
      let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      SDL.copy renderer image Nothing (Just $ SDL.Rectangle (fromIntegral <$> pos) (V2 w h))

    reactimate $ ePaint <&> R.writeIORef handlePaint

    liftIO $ R.writeIORef handleEvent $ \event -> case SDL.eventPayload event of
      SDL.MouseMotionEvent e -> setMouseLocation $ SDL.mouseMotionEventPos e
      _ -> pure ()
  actuate network
  sendEvent <- R.readIORef handleEvent

  untilM $ do
    SDL.clear renderer
    join $ R.readIORef handlePaint
    SDL.present renderer
    events <- SDL.pollEvents
    quits <- for events $ \event -> case SDL.eventPayload event of
      SDL.QuitEvent -> pure True
      _ -> False <$ sendEvent event
    pure $ or quits

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
