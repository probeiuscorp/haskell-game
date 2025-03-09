{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Game.Prelude
import qualified SDL
import SDL.Vect
import Data.Functor ((<&>))
import Control.Monad (unless, join)
import qualified Data.IORef as R
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Traversable (for)
import qualified Game.Data.Queue as Q
import GHC.IsList (IsList(fromList))

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

maxLength len vec
  | norm vec < len = vec
  | otherwise = len *^ normalize vec

data Command
  = MoveTo World
  deriving (Eq, Ord, Show)

-- Adapted from https://github.com/haskell-game/sdl2/blob/master/examples/twinklebear/Lesson04.hs
main :: IO ()
main = do
  SDL.initialize [ SDL.InitVideo ]
  window <- SDL.createWindow "Wheel of Time" $ SDL.defaultWindow { SDL.windowMode = SDL.FullscreenDesktop }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  image <- getDataFileName "blast.bmp" >>= loadTexture renderer

  handleEvent <- R.newIORef (undefined :: SDL.Event -> IO ())
  handleTick <- R.newIORef (undefined :: Field -> IO ())
  handlePaint <- R.newIORef (pure () :: IO ())
  network <- compile $ mdo
    (eMouseLocation :: Event Screen, setMouseLocation) <- newEvent
    (eAnyMouseButton :: Event SDL.MouseButtonEventData, setMouseButton) <- newEvent
    (eAnyKey :: Event SDL.KeyboardEventData, setAnyKey) <- newEvent
    eShift <- is $ filterJust $ eAnyKey <&> \e -> if (SDL.keysymKeycode . SDL.keyboardEventKeysym) e `elem` [SDL.KeycodeLShift, SDL.KeycodeRShift]
      then Just $ SDL.keyboardEventKeyMotion e == SDL.Pressed
      else Nothing
    bEnqueueCommand <- stepper False eShift

    (eTick :: Event Field, setTick) <- newEvent
    (bCamera :: Behavior (Screen -> World), _) <- newBehavior $ \v -> unP $ fromIntegral <$> v
    (bUnCamera :: Behavior (World -> Screen), _) <- newBehavior $ \v -> P $ fromIntegral . round <$> v

    (((eLMBUp, eLMBDown), (eRMBUp, eRMBDown)), _) <- is $
      first (both split . split) $ split $ eAnyMouseButton <&> \e ->
        let f cons = Left $ cons $ shouldRight e $ SDL.mouseButtonEventMotion e == SDL.Released in
        case SDL.mouseButtonEventButton e of
          SDL.ButtonLeft -> f Left
          SDL.ButtonRight -> f Right
          _ -> Right e

    let initialPosition = V2 0 0 :: World
    let eCommand = MoveTo <$> (bCamera <@> (SDL.mouseButtonEventPos <$> eRMBDown))
    bCommandQueue <- accumB (mempty :: Q.Queue Command) $ (((,) <$> bEnqueueCommand) <@> eCommand) <&> \(enqueue, command) queue -> if enqueue
      then Q.enqueue command queue
      else Q.singleton command
    reactimate $ (print . length <$> bCommandQueue) <@ eLMBDown
    bPlayerTarget <- stepper initialPosition $ bCamera <@> (SDL.mouseButtonEventPos <$> eRMBDown)
    mover <- is $ \startPos bTarget -> mdo
      let bVelocityTarget = liftA2 (\pos target -> 2 *^ maxLength 72 $ pos - target) bTarget bPos :: Behavior World
      let easing = const
      bVelocity <- accumB 0 $ easing <$> (bVelocityTarget <@ eTick)
      bPos <- accumB startPos $ (+) <$> (((^*) <$> bVelocity) <@> eTick)
      pure bPos
    bPlayer <- mover initialPosition bPlayerTarget
    bMonster <- mover 600 bPlayer

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

    bPaintCasting <- is $ bCast <&> \cast -> do
      let color = SDL.rendererDrawColor renderer
      initialColor <- SDL.get color
      color SDL.$= SDL.V4 155 180 30 0
      SDL.drawLines renderer $ fromList $ fmap fromIntegral <$> cast
      color SDL.$= initialColor
    bPaintCreatures <- is $ (sequence_ <$>) . for [bPlayer, bMonster] $ \bPos -> (bUnCamera <*> bPos) <&> \pos -> do
      ti <- SDL.queryTexture image
      let (w, h) = (SDL.textureWidth ti, SDL.textureHeight ti)
      SDL.copy renderer image Nothing (Just $ SDL.Rectangle (fromIntegral <$> pos) (V2 w h))
    bPaint <- is $ sequence_ <$> sequenceA [bPaintCreatures, bPaintCasting]
    let ePaint = bPaint <@ eTick

    handler <- is $ \event -> case SDL.eventPayload event of
      SDL.MouseMotionEvent e -> setMouseLocation $ SDL.mouseMotionEventPos e
      SDL.MouseButtonEvent e -> setMouseButton e
      SDL.KeyboardEvent e -> setAnyKey e
      _ -> pure ()

    reactimate $ R.writeIORef handlePaint `fmap` ePaint
    liftIO $ R.writeIORef handleEvent handler
    liftIO $ R.writeIORef handleTick setTick
  actuate network
  sendEvent <- R.readIORef handleEvent
  sendTick <- R.readIORef handleTick

  untilM $ do
    timeStart <- SDL.ticks
    SDL.clear renderer
    sendTick $ 1 / 60
    join $ R.readIORef handlePaint
    SDL.present renderer
    events <- SDL.pollEvents
    quits <- for events $ \event -> case SDL.eventPayload event of
      SDL.QuitEvent -> pure True
      _ -> False <$ sendEvent event
    let quit = or quits
    unless quit $ do
      timeEnd <- SDL.ticks
      let msPerFrame = 1000 `div` 60
      let timeTaken = timeEnd - timeStart
      unless (msPerFrame < timeTaken) $ do
        SDL.delay $ msPerFrame - timeTaken
    pure quit

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  SDL.quit
