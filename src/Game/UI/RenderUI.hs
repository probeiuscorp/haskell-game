module Game.UI.RenderUI (setupRenderUI) where

import Game.Prelude
import Game.UI.UI
import qualified Control.Lens as L
import qualified SDL
import qualified SDL.Font as TTF
import qualified Data.Map as Map
import Control.Monad (forM_)
import Data.String (IsString(fromString))
import Data.IORef (newIORef, readIORef, modifyIORef)
import Foreign.C (CInt)

minusPadding :: SDL.V4 CInt -> Dim -> Dim
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

type RenderUI = Dim -> UI -> IO ()
renderUI :: SDL.Renderer -> FromSetup -> RenderUI
renderUI renderer loadFontSize = go
  where
    textureSize texture = V2 (SDL.textureWidth texture) (SDL.textureHeight texture)
    querySize t = textureSize $$ SDL.queryTexture t
    go :: RenderUI
    go _ UINone = pure ()
    go dim (UIBox (BoxLayout { _boxBackgroundColor = mColor, _boxPadding = padding }) ui) = do
      forM_ mColor $ \color -> withValue (SDL.rendererDrawColor renderer) color $
        SDL.fillRect renderer $ Just dim
      go (minusPadding padding dim) ui
    go dim (UICanvas render) = render dim
    go (SDL.Rectangle boxTL (V2 width _)) (UIText style strContent) = do
      let content = fromString strContent
      font <- loadFontSize $ L.view textStyleFontSize style
      surface <- TTF.blendedWrapped font (L.view textStyleFontColor style) (fromIntegral width) content
      texture <- SDL.createTextureFromSurface renderer surface
      size <- querySize texture
      SDL.copy renderer texture Nothing $ Just $ SDL.Rectangle boxTL size
      SDL.freeSurface surface
      SDL.destroyTexture texture
    go dim (UIBoth ui1 ui2) = go dim ui1 *> go dim ui2
