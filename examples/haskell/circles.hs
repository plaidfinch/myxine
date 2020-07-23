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

import Myxine
import Myxine.Widget

data Circle = Circle
  { _x :: Double
  , _y :: Double
  , _r :: Double
  , _hue :: Word8
  } deriving (Eq, Ord, Show)

data Circles = Circles
  { _current :: Maybe Circle
  , _mouse   :: (Double, Double)
  , _rest    :: [Circle]
  } deriving (Eq, Ord, Show)

$(makeLenses ''Circle)
$(makeLenses ''Circles)

main :: IO ()
main = do
  page <- runPage mempty
    Circles { _current = Nothing, _mouse = (0, 0), _rest = [] }
    \circles -> widget do
      let count = maybe 0 (const 1) (circles^.current) + length (circles^.rest)
      title $ "Circles: " <> fromString (show count)
      html (toMarkup circles)
      listen do
        on MouseMove \MouseEvent{clientX, clientY} ->
          do mouse .= (clientX, clientY)
             zoom (current._Just) do
               circle <- get
               r .= sqrt ((circle^.x - clientX)**2 + (circle^.y - clientY)**2)
        on MouseDown \MouseEvent{clientX, clientY} ->
          do randomHue <- liftIO randomIO
             current .= Just (Circle clientX clientY 0 randomHue)
        on MouseUp \MouseEvent{} ->
          use current >>= \case
            Nothing -> pure ()
            Just circle ->
              do current .= Nothing
                 rest %= (circle :)
  print =<< waitPage page

instance ToMarkup Circle where
  toMarkup circle =
    div ! styles circleStyles $ ""
    where
      radius = fromIntegral (round @_ @Int (circle^.r))
      diameter = radius * 2
      borderWidth = 2
      circleStyles =
        [ ("position", "absolute")
        , ("top", show (circle^.y - radius - borderWidth/2) <> "px")
        , ("left", show (circle^.x - radius - borderWidth/2) <> "px")
        , ("width", show diameter <> "px")
        , ("height", show diameter <> "px")
        , ("background", "hsla(" <> show (circle^.hue) <> ", 100%, 75%, 25%)")
        , ("border", show borderWidth <> "px solid hsla(" <> show (circle^.hue) <> ", 50%, 50%, 75%)")
        , ("border-radius", show radius <> "px")
        ]

instance ToMarkup Circles where
  toMarkup circles | circles^.current == Nothing && circles^.rest == [] =
    span ! styles greetingStyles $ "Click and drag to make art!"
    where
      greetingStyles =
        [ ("transform", "translate(-50%, -100%)")
        , ("text-align", "center")
        , ("width", "100vw")
        , ("position", "absolute")
        , ("top", "50%")
        , ("left", "50%")
        , ("font-family", "Helvetica Neue")
        , ("font-size", "50pt")
        , ("color", "darkgrey") ]

  toMarkup circles =
    div ! styles canvasStyles $
      foldr (>>)
        (maybe (pure ()) toMarkup (circles^.current))
        (reverse (map toMarkup (circles^.rest)))
    where
      canvasStyles =
        [ ("position", "relative")
        , ("padding", "0px")
        , ("height", "100vh")
        , ("width", "100vw")
        , ("overflow", "hidden") ]

styles :: [(String, String)] -> Attribute
styles pairs =
  style . fromString $
    intercalate "; " $ pairs <&> \(attr, val) -> attr <> ": " <> val
