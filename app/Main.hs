{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Game.Prelude
import qualified SDL
import qualified SDL.Font as TTF
import SDL.Vect
import Control.Monad (unless, join)
import qualified Data.IORef as R
import Reactive.Banana
import Reactive.Banana.Frameworks
import Data.Traversable (for)
import qualified Game.Data.Queue as Q
import Game.Data.BBox
import Game.Data.Cycled
import Game.Casts
import GHC.IsList (IsList(fromList))
import Data.String (IsString(fromString))
import qualified Data.List.NonEmpty as NE
import qualified Control.Lens as L
import Data.Int (Int32)
import Game.UI.UI
import Game.UI.RenderUI (setupRenderUI)

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

emitEvery :: MonadMoment m => Int -> Event a -> m (Event a)
emitEvery n e = fmap (fmap snd . filterE ((== 0) . fst)) $ accumE (1 :: Int, undefined) $ e ## \a (k, _) -> (posmod $ k + 1, a)
  where
    posmod k = if k >= n then k - n else k

data Command
  = MoveTo { _cmdMoveTarget :: World }
  deriving (Eq, Ord, Show)
$(L.makePrisms ''Command)

class Config cfg where
  defaultConfig :: cfg

data MoverConfig = MoverConfig
  { mvMaxVelocity :: Double
  , mvAcceleration :: World
  }
instance Config MoverConfig where
  defaultConfig = MoverConfig 72 2

data Cast = Cast
  { _hasHit :: Bool
  } deriving (Eq, Ord, Show)
$(L.makeLenses ''Cast)

paints :: Traversable t => t (Behavior (IO a)) -> Behavior (IO ())
paints = fmap sequence_ . sequenceA

ui :: UI
ui = box (padding 16 xy) $
  box (padding 24 xy . bg (V4 255 0 0 255)) $
    text (fontSize 42) "emam ka Selxz dazyel yundin vi stogyin, ogxzhxz smizjanaskz ska ulud. yum emikxnak tevum mikum da kanulz yel an."

-- Adapted from https://github.com/haskell-game/sdl2/blob/master/examples/twinklebear/Lesson04.hs
main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  TTF.initialize

  window <- SDL.createWindow "Selxz Em" $ SDL.defaultWindow { SDL.windowMode = SDL.FullscreenDesktop }
  SDL.windowGrab window SDL.$= True
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer
  font <- TTF.load "/usr/share/fonts/truetype/ubuntu/Ubuntu-M.ttf" 24
  let load asset = getDataFileName ("assets/" ++ asset ++ ".bmp") >>= loadTexture renderer
  let loadSet base = fmap mkCycled $ traverse load $ NE.fromList $ (base ++) . ("-" ++) $$ ["0", "1", "2", "1"]
  walkSet <- loadSet "celes-walk"

  handleEvent <- R.newIORef (undefined :: SDL.Event -> IO ())
  handleTick <- R.newIORef (undefined :: Field -> IO ())
  handlePaint <- R.newIORef (pure () :: IO ())
  let toScreenField = fromIntegral :: Int32 -> ScreenField
  renderUI <- setupRenderUI renderer
  network <- compile $ mdo
    (eMouseLocation :: Event Screen, setMouseLocation) <- newEvent
    (eAnyMouseButton :: Event SDL.MouseButtonEventData, setMouseButton) <- newEvent
    (eAnyKey :: Event SDL.KeyboardEventData, setAnyKey) <- newEvent
    eShift <- is $ filterJust $ eAnyKey ## \e -> if (SDL.keysymKeycode . SDL.keyboardEventKeysym) e `elem` [SDL.KeycodeLShift, SDL.KeycodeRShift]
      then Just $ SDL.keyboardEventKeyMotion e == SDL.Pressed
      else Nothing
    bEnqueueCommand <- stepper False eShift
    (eWindowResize, pushWindowResize) <- newEvent
    bWindowSize <- resampleM eWindowResize $ SDL.get $ SDL.windowSize window
    let dpPanArea = 20
    panDirection <- is $ \(width :: ScreenField) (x :: ScreenField) -> if
      | x <= dpPanArea -> -1
      | x >= width - dpPanArea -> 1
      | otherwise -> 0
    let bMousePanDirection = pairA bMouseLocation bWindowSize ## \(mousePos, windowSize) -> (panDirection $$ windowSize) <*> unP mousePos
    let eMousePanCamera = ((\mouseDir dt -> (288 * dt) *^ mouseDir) $$ bMousePanDirection) @@ eUITick
    bCameraPosition <- accumB 0 $ (+) $$ eMousePanCamera

    (eTick :: Event Field, setTick) <- newEvent
    -- eUITick might have different dt than eTick
    let eUITick = eTick
    eAnim <- emitEvery 16 eTick
    let (bCamera :: Behavior (Screen -> World)) = bCameraPosition ## \c v -> unP (fromIntegral $$ v) + c
    let (bUnCamera :: Behavior (World -> Screen)) = bCameraPosition ## \c v -> P $ fromIntegral . round $$ v - c

    (((eLMBUp, eLMBDown), (eRMBUp, eRMBDown)), _) <- is $
      first (both split . split) $ split $ eAnyMouseButton ## \e ->
        let f cons = Left $ cons $ shouldRight e $ SDL.mouseButtonEventMotion e == SDL.Released in
        case SDL.mouseButtonEventButton e of
          SDL.ButtonLeft -> f Left
          SDL.ButtonRight -> f Right
          _ -> Right e

    let initialPosition = V2 0 0 :: World
    let eIssueCommand = MoveTo $$ (bCamera <@> (fmap toScreenField . SDL.mouseButtonEventPos $$ eRMBDown))
    let emitNothing = Nothing
    let emitNewCommand = Just . Just
    let emitClearCurrentCommand = Just Nothing
    eEnqueue <- is $ pairBE bEnqueueCommand eIssueCommand ## \(enqueue, command) queue -> if enqueue
      then (emitNothing, Q.enqueue command queue)
      else (emitNewCommand command, Q.singleton command)
    eDequeue <- is $ eCommandFinished $> \q -> (first emitNewCommand $$ Q.dequeue q) ?? (emitClearCurrentCommand, q)
    -- Dequeue then enqueue, and only keep new command event from enqueue
    let eChangeCommand = unionWith (\enq deq q -> enq . snd $ deq q) eEnqueue eDequeue
    (emCommand, bCommandQueue) <- mapAccum mempty eChangeCommand
    let eCommand = filterJust emCommand
    bCommand <- stepper Nothing eCommand
    mover <- is $ \(cfg :: MoverConfig) startPos bTarget -> mdo
      let bVelocityTarget = liftA2 (\pos target -> mvAcceleration cfg *^ maxLength (mvMaxVelocity cfg) $ pos - target) bTarget bPos :: Behavior World
      let easing = const
      bVelocity <- accumB 0 $ easing $$ (bVelocityTarget <@ eTick)
      bPos <- accumB startPos $ (+) $$ (((^*) $$ bVelocity) <@> eTick)
      pure bPos
    bPlayer <- mover defaultConfig initialPosition bTarget
    mkHandleCommand <- is $ \case
      Nothing -> pure (bPlayer, never)
      Just (MoveTo target) -> do
        let bHasReachedTarget = liftA2 (\tar pos -> norm (tar - pos) < 0.5) bTarget bPlayer
        let eHasReachedTarget = void $ whenE bHasReachedTarget eTick
        pure (pure target, eHasReachedTarget)
    initialHandleCommand <- mkHandleCommand Nothing
    eHandleCommand <- execute $ mkHandleCommand $$ eCommand
    bTarget <- switchB (fst initialHandleCommand) $ fst $$ eHandleCommand
    eCommandFinished <- switchE (snd initialHandleCommand) $ snd $$ eHandleCommand
    bMonster <- mover defaultConfig 600 bPlayer

    let bMonsterHitbox = mkCircleBB 100 $$ bMonster
    let eIsCastingHittingMonster = (isWithin $$ bMonsterHitbox) @@ eCastPointerPos
    let eIsMonsterHurt = void $ filterE id eIsCastingHittingMonster
    bHealth <- accumB (144 :: Int) (max 0 . subtract 4 <$ eIsMonsterHurt)

    let eIsCasting = mergeWith (const True) (const False) (const $ const False) eLMBUp eLMBDown
    bIsCasting <- stepper False eIsCasting
    let (eCastingStop, _eCastingStop) = split $ shouldRight () $$ eIsCasting
    bMouseLocation <- stepper (P 0) eMouseLocation
    let eIsCastingPos = pairBE (bCamera <*> bMouseLocation) eIsCasting
    let eMouseEdge = eIsCastingPos ## \(pos, isMouse) -> if isMouse then Just pos else Nothing
    let eMouseMove = whenE bIsCasting $ Just $$ (bCamera <*> bMouseLocation) <@ eTick
    let eCastTarget = filterJust $ unionWith const eMouseEdge eMouseMove
    bCastTarget <- stepper 0 eCastTarget
    bCastPos <- switchM (0, False) eIsCastingPos $ \(pos, isMouse) -> if isMouse
      then Just $$$ mover (MoverConfig 288 12) pos bCastTarget
      else pure $ pure Nothing
    let eCastPointerPos = filterJust $ whenE bIsCasting $ bCastPos <@ eTick
    let eCastPos = pairBE bPlayer eCastPointerPos :: Event (World, World)
    eCast <- is $ unions
      [ (:) $$ eCastPos
      , const [] <$ eCastingStop
      ]
    bCast <- accumB [] eCast
    let eCastCompleted = bCast <@ eCastingStop
    reactimate $ eCastCompleted ## print . scoreSweepCast

    bWalkTexture <- (fst $$$) $ accumB (next walkSet) $ next . snd <$ eAnim
    let bScreenCast = (\uncamera cast -> uncamera . snd $$ cast) $$ bUnCamera <*> bCast
    bPaintCastingLine <- is $ bScreenCast ## \cast -> do
      let color = SDL.rendererDrawColor renderer
      color SDL.$= SDL.V4 155 180 30 0
      SDL.drawLines renderer $ fromList cast
    bPaintCastingTarget <- is $ (fmap $$ bUnCamera <*> bCastPos) ## \case
      Just pos -> do
        let color = SDL.rendererDrawColor renderer
        color SDL.$= SDL.V4 180 180 180 0
        SDL.drawRect renderer $ Just $ flip SDL.Rectangle 17 $ pos - 8
      Nothing -> pure ()
    let bPaintCasting = paints [bPaintCastingLine, bPaintCastingTarget]
    bPaintText <- is $ bHealth ## \health -> do
      let content = fromString $ show health
      (w, h) <- both fromIntegral $$ TTF.size font content
      let dim = V2 w h
      surface <- TTF.blended font 255 content
      texture <- SDL.createTextureFromSurface renderer surface
      bottomRight <- SDL.get $ SDL.windowSize window
      SDL.copy renderer texture Nothing $ Just $ SDL.Rectangle (P $ (subtract 25 $$ bottomRight) - dim) dim
      SDL.freeSurface surface
      SDL.destroyTexture texture
    paintCreature <- is $ \(pos :: Screen) (image :: SDL.Texture) -> do
      let (w, h) = both (* 4) (16, 24)
      SDL.copy renderer image Nothing (Just $ SDL.Rectangle pos (V2 w h))
    bPaintCreatures <- is $ (sequence_ $$) . for [bPlayer, bMonster] $ \bPos -> paintCreature $$ (bUnCamera <*> bPos) <*> bWalkTexture
    let bPaintUI = bWindowSize ## \windowSize -> renderUI (SDL.Rectangle 0 windowSize) ui
    let bPaint = paints [bPaintCreatures, bPaintCasting, bPaintText, bPaintUI]
    let ePaint = bPaint <@ eTick

    handler <- is $ \event -> case SDL.eventPayload event of
      SDL.MouseMotionEvent e -> setMouseLocation $ toScreenField $$ SDL.mouseMotionEventPos e
      SDL.MouseButtonEvent e -> setMouseButton e
      SDL.KeyboardEvent e -> setAnyKey e
      SDL.WindowSizeChangedEvent _ -> pushWindowResize ()
      _ -> pure ()

    reactimate $ R.writeIORef handlePaint $$ ePaint
    liftIO $ R.writeIORef handleEvent handler
    liftIO $ R.writeIORef handleTick setTick
  actuate network
  sendEvent <- R.readIORef handleEvent
  sendTick <- R.readIORef handleTick

  untilM $ do
    timeStart <- SDL.ticks
    SDL.rendererDrawColor renderer SDL.$= 0
    SDL.clear renderer
    sendTick $ 1 / 60
    join $ R.readIORef handlePaint
    SDL.present renderer
    events <- SDL.pollEvents
    quit <- fmap or . for events $ \event -> case SDL.eventPayload event of
      SDL.QuitEvent -> pure True
      _ -> False <$ sendEvent event
    unless quit $ do
      timeEnd <- SDL.ticks
      let msPerFrame = 1000 `div` 60
      let timeTaken = timeEnd - timeStart
      unless (msPerFrame < timeTaken) $ do
        SDL.delay $ msPerFrame - timeTaken
    pure quit

  SDL.destroyRenderer renderer
  SDL.destroyWindow window
  TTF.free font
  TTF.quit
  SDL.quit
