module Game.UI.UI (
  UI(..), box, text, none,
  HasSide(..), Side, Axis, Dim,
  BoxLayout(..), boxAlign, boxBackgroundColor, boxPadding, TextStyle(..), textStyleFontColor, textStyleFontSize,
  tl, tr, bl, br, top, left, right, bottom,
  x, y, xy,
  start, center, end, padding, bg,
  fontColor, fontSize,
) where

import Game.Prelude
import qualified Control.Lens as L
import qualified SDL
import SDL.Vect
import Data.Word (Word8)
import Foreign.C (CInt)

if' :: Bool -> a -> a -> a
if' k a b = if k then a else b
class HasSide s where
  toSide :: s -> Side
data Side = Side Bool Bool Bool Bool
instance HasSide Side where
  toSide = id
tl, tr, bl, br, top, left, right, bottom :: Side
tl = Side True False False True
tr = Side True True False False
bl = Side False False True True
br = Side False True True False
top = Side True False False False
left = Side False True False False
right = Side False False True False
bottom = Side False False False True

replaceSide :: a -> Side -> V4 a -> V4 a
replaceSide a (Side t r b l) (V4 t' r' b' l') = V4 (if' t a t') (if' r a r') (if' b a b') (if' l a l')

data Axis = Axis Bool Bool deriving (Eq, Ord, Show)
instance HasSide Axis where
  toSide (Axis hasX hasY) = Side hasY hasX hasY hasX
x, y, xy :: Axis
x = Axis True False
y = Axis False True
xy = Axis True True

replaceAxis :: a -> Axis -> V2 a -> V2 a
replaceAxis a (Axis modX modY) (V2 x' y') = V2 (if' modX a x') (if' modY a y')

data Alignment = AlignStart | AlignCenter | AlignEnd

type Config a = a -> a
data BoxLayout = BoxLayout
  { _boxAlign :: V2 Alignment
  , _boxPadding :: V4 CInt
  , _boxBackgroundColor :: Maybe (V4 Word8)
  }
L.makeLenses ''BoxLayout
emptyBoxLayout :: BoxLayout
emptyBoxLayout = BoxLayout (pure AlignStart) 0 Nothing

start, center, end :: Axis -> Config BoxLayout
(V3 start center end) = V3 AlignStart AlignCenter AlignEnd ## \a -> L.over boxAlign . replaceAxis a

padding :: HasSide s => CInt -> s -> Config BoxLayout
padding mz = L.over boxPadding . replaceSide mz . toSide

bg :: V4 Word8 -> Config BoxLayout
bg = L.over boxBackgroundColor . const . Just

data TextStyle = TextStyle
  { _textStyleFontColor :: V4 Word8
  , _textStyleFontSize :: Int
  }
L.makeLenses ''TextStyle
emptyTextStyle :: TextStyle
emptyTextStyle = TextStyle 255 24

fontColor :: V4 Word8 -> Config TextStyle
fontColor = L.over textStyleFontColor . const

fontSize :: Int -> Config TextStyle
fontSize = L.over textStyleFontSize . const

type Dim = SDL.Rectangle CInt
data UI
  = UIBox BoxLayout UI
  | UICanvas (Dim -> IO ())
  | UIText TextStyle String
  | UIBoth UI UI
  | UINone
instance Semigroup UI where
  (<>) = UIBoth
instance Monoid UI where
  mempty = UINone

box :: Config BoxLayout -> UI -> UI
box = UIBox . ($ emptyBoxLayout)
text :: Config TextStyle -> String -> UI
text = UIText . ($ emptyTextStyle)
none :: UI
none = UINone
