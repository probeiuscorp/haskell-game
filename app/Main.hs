{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Game.Prelude
import qualified SDL
import SDL.Vect
import Data.Int
import Data.Functor ((<&>))
import Control.Monad (unless, join)
import qualified Data.IORef as R
import Reactive.Banana
import Reactive.Banana.Frameworks
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

shouldRight :: a -> Bool -> Either a a
shouldRight a b = if b then Right a else Left a

type World = V2 Int32
type Screen = Point V2 Int32
-- Adapted from https://github.com/haskell-game/sdl2/blob/master/examples/twinklebear/Lesson04.hs
main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  window <- SDL.createWindow "Wheel of Time" $ SDL.defaultWindow { SDL.windowMode = SDL.Fullscreen }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  image <- getDataFileName "blast.bmp" >>= loadTexture renderer

  handleEvent <- R.newIORef (undefined :: SDL.Event -> IO ())
  handleTick <- R.newIORef (undefined :: IO ())
  handlePaint <- R.newIORef (pure () :: IO ())
  network <- compile $ do
    (eMouseLocation :: Event Screen, setMouseLocation) <- newEvent
    (eAnyMouseButton :: Event SDL.MouseButtonEventData, setMouseButton) <- newEvent

    (eTick :: Event (), setTick) <- newEvent
    (bCamera :: Behavior (Screen -> World), _) <- newBehavior unP

    (((eLMBUp, eLMBDown), (eRMBUp, eRMBDown)), _) <- is $
      first (both split . split) $ split $ eAnyMouseButton <&> \e ->
        let f cons = Left $ cons $ shouldRight e $ SDL.mouseButtonEventMotion e == SDL.Released in
        case SDL.mouseButtonEventButton e of
          SDL.ButtonLeft -> f Left
          SDL.ButtonRight -> f Right
          _ -> Right e

    let eIsCasting = mergeWith (const True) (const False) (const $ const False) eLMBUp eLMBDown
    bIsCasting <- stepper False eIsCasting
    let (eCastingStop, _eCastingStop) = split $ shouldRight () <$> eIsCasting
    bMouseLocation <- stepper (P $ V2 0 0) eMouseLocation
    let eTickedMouse = bMouseLocation <@ eTick
    let eCastingActive = whenE bIsCasting eTickedMouse
    eCast <- is $ unions
      [ (:) <$> eCastingActive
      , const [] <$ eCastingStop
      ]
    bCast <- accumB [] eCast
    let eCastCompleted = bCast <@ eCastingStop
    reactimate $ eCastCompleted <&> print . length

    ePaint <- is $ eMouseLocation <&> \pos -> do
      ti <- SDL.queryTexture image
      let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      SDL.copy renderer image Nothing (Just $ SDL.Rectangle (fromIntegral <$> pos) (V2 w h))

    handler <- is $ \event -> case SDL.eventPayload event of
      SDL.MouseMotionEvent e -> setMouseLocation $ SDL.mouseMotionEventPos e
      SDL.MouseButtonEvent e -> setMouseButton e
      _ -> pure ()

    reactimate $ R.writeIORef handlePaint `fmap` ePaint
    liftIO $ R.writeIORef handleEvent handler
    liftIO $ R.writeIORef handleTick $ setTick ()
  actuate network
  sendEvent <- R.readIORef handleEvent
  sendTick <- R.readIORef handleTick

  untilM $ do
    SDL.clear renderer
    sendTick
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
