{-# language NamedFieldPuns, BlockArguments, OverloadedStrings,
  DuplicateRecordFields #-}

module Main (main) where

import Data.Word

import System.Random
import qualified Data.Text as Text
import Text.Blaze
import Text.Blaze.Html
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import Text.Blaze.Renderer.Utf8 (renderMarkup)

import Myxine

data Circle
  = Circle
    { x :: Double
    , y :: Double
    , r :: Double
    , hue :: Word8
    } deriving (Eq, Ord, Show)

data Circles
  = Circles
    { current :: Maybe Circle
    , mouse :: (Double, Double)
    , rest :: [Circle]
    } deriving (Eq, Ord, Show)

main :: IO ()
main = do
  page <- runPage mempty
    Circles { current = Nothing, mouse = (0, 0), rest = [] }
    (mconcat [ on MouseMove \MouseEvent{clientX = mouseX, clientY = mouseY} _ state ->
                 print "move" >> pure (state { mouse = (mouseX, mouseY)
                             , current = (\Circle{x, y, hue} ->
                                            let r' = sqrt ((abs (x - mouseX))**2 + (abs (y - mouseY))**2)
                                            in Circle{x, y, r = r', hue}) <$> current state
                             })
             , on MouseDown \MouseEvent{clientX = x, clientY = y} _ state ->
                 do print "down"
                    hue <- randomIO
                    pure (state{current = Just (Circle{x, y, r = 0, hue})})
             , on MouseUp \MouseEvent{} _ state ->
                 print "up" >>
                 pure case current state of
                   Nothing -> state
                   Just circle -> state { current = Nothing, rest = circle : rest state }
             ])
    \state@Circles{current, rest} ->
      (Just ("Circles: " <> Text.pack (show (maybe 0 (const 1) current + length rest))),
       renderMarkup do
          pre (toHtml (show state)))
  print =<< waitPage page

-- TODO: Dependent-Map is discarding all but the first entry!!!
