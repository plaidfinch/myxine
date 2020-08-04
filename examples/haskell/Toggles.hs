{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}
{-# language BlockArguments, RecordWildCards, TemplateHaskell #-}

module Main (main) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String
import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader
import Data.Foldable
import Control.Lens
import Myxine

import qualified Data.Sequence as Seq

main :: IO ()
main = do
  page <- runPage mempty (pure (Seq.empty)) (reactive toggles)
  print =<< waitPage page

toggle :: Reactive Bool
toggle = do
  active <- ask
  H.button ! A.style (if active then "background: green; color: white"
                                else "background: white")
    @@ do markup $ if active then H.span "ON" else H.span "OFF"
          on Click \_ -> modify not

toggles :: Reactive (Seq.Seq Bool)
toggles = do
  H.span @@ do
    H.button ! A.style "background: white; font-size: 20pt" @@ do
      "+"
      on Click \_ -> modify (Seq.|> False)
    H.button ! A.style "background: white; font-size: 20pt" @@ do
      "-"
      on Click \_ -> modify \case
        Seq.Empty -> Seq.Empty
        i Seq.:|> _ -> i
  H.div @@ do
    each ## toggle
