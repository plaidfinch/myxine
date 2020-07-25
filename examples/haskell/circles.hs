{-# language OverloadedStrings, DuplicateRecordFields #-}
{-# language NamedFieldPuns, BlockArguments, TypeApplications, RecordWildCards,
  TemplateHaskell #-}

module Main (main) where

import Prelude hiding (div, span)
import Control.Monad.IO.Class
import Control.Monad.State
import Data.Word (Word8)
import Data.String (fromString)
import Data.Functor ((<&>))
import Data.List (intercalate)
import System.Random (randomIO)
import Text.Blaze.Html5 hiding (main, style, title, map, html)
import Text.Blaze.Html5.Attributes hiding (span, title)
import qualified Text.Blaze.Html5 as Html
import Control.Lens
import Data.UUID (UUID)
import Data.Map (Map)
import qualified Data.Map as Map

import Myxine
import Myxine.Reactive

data Circle = Circle
  { _identity :: UUID
  , _x :: Double
  , _y :: Double
  , _z :: Int
  , _r :: Double
  , _hue :: Word8
  } deriving (Eq, Ord, Show)

data Circles = Circles
  { _current   :: Maybe Circle
  , _rest      :: Map UUID Circle
  } deriving (Eq, Ord, Show)

$(makeLenses ''Circle)
$(makeLenses ''Circles)

main :: IO ()
main = do
  page <- runPage mempty
    Circles { _current = Nothing, _rest = mempty }
    (reactive . drawCircles)
  print =<< waitPage page

drawCircles :: Circles -> Reactive Circles
drawCircles circles = do
  title $ "Circles: " <> fromString (show count)
  if circles^.current == Nothing && null (circles^.rest)
    then
      html $
      div ! styles canvasStyles $
      span ! styles greetingStyles $
      "Click and drag to make art!"
    else
      div ! styles canvasStyles @@
      foldr (>>)
        (maybe (pure ()) drawCircle (circles^.current))
        (reverse (map drawCircle (Map.elems (circles^.rest))))
  on MouseMove \MouseEvent{clientX, clientY, buttons} ->
    when (buttons == 1) $
      zoom (current._Just) do
        circle <- get
        r .= sqrt ((circle^.x - clientX)**2 + (circle^.y - clientY)**2)
  on MouseDown \MouseEvent{clientX, clientY, shiftKey} ->
    when (not shiftKey) $
    do randomHue <- liftIO randomIO
       randomUUID <- liftIO randomIO
       current .= Just (Circle randomUUID clientX clientY count 0 randomHue)
  on MouseUp \MouseEvent{} ->
    use current >>= \case
      Nothing -> pure ()
      Just circle ->
        do current .= Nothing
           rest %= Map.insert (circle^.identity) circle
  where
    count = maybe 0 (const 1) (circles^.current) + length (circles^.rest)
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
    on MouseOver \MouseEvent{buttons} ->
      when (buttons == 0) $
        zoom (rest . at (circle^.identity) . _Just) do
          randomHue <- liftIO randomIO
          hue .= randomHue
    on' MouseDown \MouseEvent{shiftKey} ->
      if shiftKey
      then do
        rest %= fmap \c ->
          if c^.identity == circle^.identity
          then c & z .~ 0
          else c & z +~ 1
        pure Stop
      else pure Bubble
  where
    radius = fromIntegral (round @_ @Int (circle^.r))
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
    intercalate "; " $ pairs <&> \(attr, val) -> attr <> ": " <> val
