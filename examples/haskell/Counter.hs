{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}
{-# language BlockArguments, RecordWildCards, TemplateHaskell #-}
{-# language RecursiveDo #-}
module Main (main) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Lens
import Control.Concurrent
import Control.Monad
import Control.Monad.IO.Class

import Myxine

data Counter =
  Counter { _highlighted :: Bool, _count :: Int }
  deriving (Show)
$(makeLenses ''Counter)

main :: IO ()
main = mdo
  interrupt <-
    countdown 1000000 (modifyPage page (count %~ max 0 . subtract 1))
  page <- runPage mempty
    (pure Counter { _highlighted = False, _count = 0 })
    (reactive (counter interrupt))
  print =<< waitPage page

interval :: Int -> IO () -> IO ThreadId
interval i action =
  forkIO (forever (threadDelay i >> action))

countdown :: Int -> IO () -> IO (IO ())
countdown i action =
  do interrupts <- newChan
     _ <- forkIO $ forever do
       tid <- interval i action
       () <- readChan interrupts
       killThread tid
     pure (writeChan interrupts ())

counter :: IO () -> Reactive Counter
counter interrupt = do
  isHighlighted <- view highlighted
  H.button
    ! A.style ("background: " <> (if isHighlighted then "red" else "yellow") <> ";"
              <> "font-size: 50pt; margin: 40pt;")
    @@ do
      on Click \_ ->
        do count += 1
           liftIO interrupt
      on MouseOver \_ -> highlighted .= True
      on MouseOut \_ -> highlighted .= False
      markup =<< view count
