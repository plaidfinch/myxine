{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}
{-# language BlockArguments, RecordWildCards, TemplateHaskell #-}
module Main (main) where

-- Generally helpful:
import Prelude hiding (div, span)
import Text.Blaze.Html5 hiding (main, style, title, map, html)
import Text.Blaze.Html5.Attributes hiding (span, title)
import Data.String
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens

-- Myxine itself:
import Myxine

-- Specific to this application:
import Data.Word
import Data.UUID (UUID)
import Data.Map (Map)
import qualified Data.Map as Map
import System.Random (randomIO)
import Data.List (intercalate)
import System.Exit

data Circle = Circle
  { _identity :: UUID
  , _x :: Double
  , _y :: Double
  , _z :: Int
  , _r :: Double
  , _hue :: Word8
  } deriving (Eq, Ord, Show)
$(makeLenses ''Circle)

data Circles = Circles
  { _current   :: Maybe Circle
  , _rest      :: Map UUID Circle
  } deriving (Eq, Ord, Show)
$(makeLenses ''Circles)

main :: IO ()
main = do
  page <- runPage mempty
    Circles { _current = Nothing, _rest = mempty }
    (reactive allCircles)
  print =<< waitPage page

allCircles :: Reactive Circles
allCircles = do
  circles <- ask
  title $ "Circles: " <> fromString (show (count circles))
  if circles^.current == Nothing && null (circles^.rest)
    then
      markup $
      div ! styles canvasStyles $
      span ! styles greetingStyles $
      "Click and drag to make art!"
    else
      div ! styles canvasStyles @@
        maybe mempty drawCircle (circles^.current) <>
        mconcat (reverse (map drawCircle (Map.elems (circles^.rest))))
  on MouseDown \MouseEvent{clientX, clientY, shiftKey = False} ->
    do randomHue <- liftIO randomIO
       randomUUID <- liftIO randomIO
       current .= Just (Circle randomUUID clientX clientY (count circles) 0 randomHue)
  on MouseMove \MouseEvent{clientX, clientY, buttons = 1} ->
    zoom (current._Just) do
      circle <- get
      r .= sqrt ((circle^.x - clientX)**2 + (circle^.y - clientY)**2)
  on MouseUp \MouseEvent{} -> do
    use current >>= \case
      Nothing -> pure ()
      Just circle ->
        do current .= Nothing
           rest %= Map.insert (circle^.identity) circle
  on KeyUp \KeyboardEvent
    { key = "c"
    , ctrlKey = True
    , shiftKey = False
    , altKey = False
    , metaKey = False
    } -> liftIO exitFailure  -- end the program on Ctrl-C in the browser too!
  where
    count circles =
      maybe 0 (const 1) (circles^.current) + length (circles^.rest)
    canvasStyles =
      [ ("position", "relative")
      , ("padding", "0px")
      , ("height", "100vh")
      , ("width", "100vw")
      , ("overflow", "hidden")
      , ("background", "black") ]
    greetingStyles =
      [ ("transform", "translate(-50%, -100%)")
      , ("text-align", "center")
      , ("select", "none")
      , ("width", "100vw")
      , ("position", "absolute")
      , ("top", "50%")
      , ("left", "50%")
      , ("font-family", "Helvetica Neue")
      , ("font-size", "50pt")
      , ("color", "darkgrey") ]

drawCircle :: Circle -> Reactive Circles
drawCircle circle =
  div ! styles circleStyles @@ do
    on MouseOver \MouseEvent{buttons = 0} ->
      zoom (rest . at (circle^.identity) . _Just) do
        randomHue <- liftIO randomIO
        hue .= randomHue
    on' MouseDown \MouseEvent{shiftKey = True} ->
      do rest %= fmap \c ->
           if c^.identity == circle^.identity
           then c & z .~ 0
           else c & z +~ 1
         pure Stop
  where
    radius = fromIntegral (round (circle^.r) :: Int)
    diameter = radius * 2
    borderWidth = 3
    background = "hsla(" <> show (circle^.hue) <> ", 100%, 70%, 55%)"
    borderColor = "hsla(" <> show (circle^.hue) <> ", 50%, 50%, 100%)"
    circleStyles =
      [ ("position", "absolute")
      , ("top", show (circle^.y - radius - borderWidth/2) <> "px")
      , ("left", show (circle^.x - radius - borderWidth/2) <> "px")
      , ("width", show diameter <> "px")
      , ("height", show diameter <> "px")
      , ("z-index", show (circle^.z))
      , ("background", background)
      , ("border", show borderWidth <> "px solid " <> borderColor)
      , ("border-radius", show radius <> "px")
      , ("box-shadow", "0px 0px 25px " <> borderColor)
      ]

styles :: [(String, String)] -> Attribute
styles pairs =
  style . fromString $
    intercalate "; " $ (\(attr, val) -> attr <> ": " <> val) <$> pairs
