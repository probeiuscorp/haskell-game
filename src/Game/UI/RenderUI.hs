module Game.UI.RenderUI (setupRenderUI) where

import Game.Prelude
import qualified Game.UI.UI as UI
import qualified Control.Lens as L
import qualified SDL
import qualified SDL.Font as TTF
import qualified Data.Map as Map
import Control.Monad (forM_)
import Data.String (IsString(fromString))
import Data.IORef (newIORef, readIORef, modifyIORef)
import Foreign.C (CInt)

minusPadding :: SDL.V4 CInt -> UI.Dim -> UI.Dim
minusPadding (SDL.V4 pt pr pb pl) (SDL.Rectangle (SDL.P origin) size) = SDL.Rectangle (SDL.P (origin + ptl)) (size - ptl - pbr)
  where
    ptl = V2 pl pt
    pbr = V2 pr pb

type FromSetup = Int -> IO TTF.Font

setupRenderUI :: SDL.Renderer -> IO RenderUI
setupRenderUI renderer = do
  fontsRef <- newIORef (Map.empty :: Map.Map Int TTF.Font)
  loadFontSize <- is $ \size -> do
    fontBySize <- readIORef fontsRef
    case Map.lookup size fontBySize of
      Just font -> pure font
      Nothing -> do
        font <- TTF.load "/home/caleb/Downloads/renogare/Renogare-Regular.otf" size
        modifyIORef fontsRef $ Map.insert size font
        pure font
  pure $ renderUI renderer loadFontSize

-- | Find Rectangle within given Rectangle with same size as V2
placeWithin :: V2 UI.Alignment -> V2 CInt -> UI.Dim -> UI.Dim
placeWithin aligns size (SDL.Rectangle (SDL.P boxTL) bounds) = SDL.Rectangle (SDL.P origin) size
  where
    origin = placeOrigin $$ boxTL <*> bounds <*> size <*> aligns
    placeOrigin :: CInt -> CInt -> CInt -> UI.Alignment -> CInt
    placeOrigin start end v = \case
      UI.AlignStart -> start
      UI.AlignCenter -> start + (end - start - v) `div` 2
      UI.AlignEnd -> end - v

applyAspectRatio :: V2 CInt -> V2 (Either CInt Rational) -> V2 CInt
applyAspectRatio bounds = (\case
  (V2 (Left x) (Left y)) -> V2 x y
  (V2 (Left x) (Right q)) -> V2 x (w x q)
  (V2 (Right q) (Left y)) -> V2 (w y q) y
  (V2 (Right q1) (Right q2)) -> w $$ bounds <*> V2 q1 q2
  ) {- HLint ignore applyAspectRatio -} where
    w :: CInt -> Rational -> CInt
    w x q = fromIntegral . round $ fromIntegral x * q

type RenderUI = UI.Dim -> UI.UI -> IO ()
renderUI :: SDL.Renderer -> FromSetup -> RenderUI
renderUI renderer loadFontSize = go
  where
    textureSize texture = V2 (SDL.textureWidth texture) (SDL.textureHeight texture)
    querySize = fmap textureSize . SDL.queryTexture
    go :: RenderUI
    go _ UI.UINone = pure ()
    go dim@(SDL.Rectangle _ bounds) (UI.UIBox (UI.BoxLayout align sizeSpec padding mColor) ui) = do
      eSize <- is $ pairA sizeSpec bounds ## \(t0, d0) -> flip (maybe $ Left d0) t0 $: \case
        UI.MzFn fn -> Left $ fn d0
        UI.MzRatio q -> Right q
        UI.MzPx x -> Left x
      let contentBox = minusPadding padding $ placeWithin align (applyAspectRatio bounds eSize) dim
      forM_ mColor $ \color -> withValue (SDL.rendererDrawColor renderer) color $
        SDL.fillRect renderer $ Just contentBox
      go contentBox ui
    go dim (UI.UICanvas render) = render dim
    go (SDL.Rectangle boxTL (V2 width _)) (UI.UIText style strContent) = do
      let content = fromString strContent
      font <- loadFontSize $ L.view UI.textStyleFontSize style
      surface <- TTF.blendedWrapped font (L.view UI.textStyleFontColor style) (fromIntegral width) content
      texture <- SDL.createTextureFromSurface renderer surface
      size <- querySize texture
      SDL.copy renderer texture Nothing $ Just $ SDL.Rectangle boxTL size
      SDL.freeSurface surface
      SDL.destroyTexture texture
    go dim (UI.UIBoth ui1 ui2) = go dim ui1 *> go dim ui2
