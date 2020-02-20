{-# language OverloadedStrings, DuplicateRecordFields #-}
{-# language NamedFieldPuns, BlockArguments, TypeApplications, RecordWildCards #-}

module Main (main) where

import Prelude hiding (div, span)
import Data.Word (Word8)
import Data.String (fromString)
import Data.Functor ((<&>))
import Data.List (intercalate)
import System.Random (randomIO)
import Text.Blaze.Html5 hiding (main, style, map)
import Text.Blaze.Html5.Attributes hiding (span)
import Text.Blaze.Renderer.Utf8 (renderMarkup)
import Myxine

data Circle = Circle
  { x :: Double
  , y :: Double
  , r :: Double
  , hue :: Word8
  } deriving (Eq, Ord, Show)

data Circles = Circles
  { current :: Maybe Circle
  , mouse   :: (Double, Double)
  , rest    :: [Circle]
  } deriving (Eq, Ord, Show)

main :: IO ()
main = do
  page <- runPage mempty
    Circles { current = Nothing, mouse = (0, 0), rest = [] }
    (mconcat [ on MouseMove \MouseEvent{clientX, clientY} _ state ->
                 pure (state { mouse = (clientX, clientY)
                             , current = current state <&> \Circle{..} ->
                                 Circle{r = sqrt ((x - clientX)**2 + (y - clientY)**2), ..} })
             , on MouseDown \MouseEvent{clientX, clientY} _ state ->
                 do hue <- randomIO
                    pure (state { current = Just (Circle{x = clientX, y = clientY, r = 0, hue}) })
             , on MouseUp \MouseEvent{} _ state ->
                 pure case current state of
                   Nothing -> state
                   Just circle -> state { current = Nothing, rest = circle : rest state }
             ])
    \state@Circles{current, rest} ->
      (Just ("Circles: " <> fromString (show (maybe 0 (const 1) current + length rest))),
       renderMarkup (toMarkup state))
  print =<< waitPage page

instance ToMarkup Circle where
  toMarkup Circle{x, y, r, hue} =
    div ! styles circleStyles $ ""
    where
      radius = fromIntegral (round @_ @Int r)
      diameter = radius * 2
      borderWidth = 2
      circleStyles =
        [ ("position", "absolute")
        , ("top", show (y - radius - borderWidth/2) <> "px")
        , ("left", show (x - radius - borderWidth/2) <> "px")
        , ("width", show diameter <> "px")
        , ("height", show diameter <> "px")
        , ("background", "hsla(" <> show hue <> ", 100%, 75%, 25%)")
        , ("border", show borderWidth <> "px solid hsla(" <> show hue <> ", 50%, 50%, 75%)")
        , ("border-radius", show radius <> "px")
        ]

instance ToMarkup Circles where
  toMarkup Circles{current = Nothing, rest = []} =
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
        , ("color", "darkgrey")
        ]

  toMarkup Circles{current, rest} =
    div ! styles canvasStyles $
      foldr (>>) (maybe (pure ()) toMarkup current) (reverse (map toMarkup rest))
    where
      canvasStyles =
        [ ("position", "relative")
        , ("padding", "0px")
        , ("height", "100vh")
        , ("width", "100vw")
        , ("overflow", "hidden")
        ]

styles :: [(String, String)] -> Attribute
styles pairs =
  style . fromString $
    intercalate "; " $ pairs <&> \(attr, val) -> attr <> ": " <> val
