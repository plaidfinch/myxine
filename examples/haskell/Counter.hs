{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}
{-# language BlockArguments, RecordWildCards, TemplateHaskell #-}
module Main (main) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Lens

import Myxine
import Myxine.Reactive

data Counter =
  Counter { _highlighted :: Bool, _count :: Int }
  deriving (Show)
$(makeLenses ''Counter)

main :: IO ()
main = do
  page <- runPage mempty
    Counter { _highlighted = False, _count = 0 }
    (reactive . counter)
  print =<< waitPage page

counter :: Counter -> Reactive Counter
counter model = do
  H.button
    ! A.style ("background: " <> (if model^.highlighted then "red" else "yellow") <> ";"
              <> "font-size: 50pt; margin: 40pt;")
    @@ do
      on Click \_ -> count += 1
      on MouseOver \_ -> highlighted .= True
      on MouseOut \_ -> highlighted .= False
      markup (show (model^.count))
