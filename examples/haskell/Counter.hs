-- {-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}
-- {-# language BlockArguments, RecordWildCards, TemplateHaskell #-}
-- {-# language RecursiveDo #-}
-- module Main (main) where

-- import Text.Blaze.Html5 ((!))
-- import qualified Text.Blaze.Html5 as H
-- import qualified Text.Blaze.Html5.Attributes as A
-- import Control.Lens
-- import Control.Concurrent
-- import Control.Monad
-- import Control.Monad.IO.Class

-- import Myxine
-- import Myxine.Reactive

-- data Counter =
--   Counter { _highlighted :: Bool, _count :: Int }
--   deriving (Show)
-- $(makeLenses ''Counter)

-- main :: IO ()
-- main = mdo
--   interrupt <-
--     countdown 1000000 (modifyPage page (count %~ max 0 . subtract 1))
--   page <- runPage mempty
--     Counter { _highlighted = False, _count = 0 }
--     (reactive . counter interrupt)
--   print =<< waitPage page

-- interval :: Int -> IO () -> IO ThreadId
-- interval i action =
--   forkIO (forever (threadDelay i >> action))

-- countdown :: Int -> IO () -> IO (IO ())
-- countdown i action =
--   do interrupts <- newChan
--      forkIO $ forever do
--        tid <- interval i action
--        () <- readChan interrupts
--        killThread tid
--      pure (writeChan interrupts ())

-- counter :: IO () -> Counter -> Reactive Counter
-- counter interrupt model = do
--   H.button
--     ! A.style ("background: " <> (if model^.highlighted then "red" else "yellow") <> ";"
--               <> "font-size: 50pt; margin: 40pt;")
--     @@ do
--       on Click \_ ->
--         do count += 1
--            liftIO interrupt
--       on MouseOver \_ -> highlighted .= True
--       on MouseOut \_ -> highlighted .= False
--       markup (show (model^.count))

{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}

module Main (main) where

import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Control.Lens

import Myxine
import Myxine.Reactive

main :: IO ()
main = do
  page <- runPage mempty (0, False) (reactive . component)
  print =<< waitPage page

component :: (Integer, Bool) -> Reactive (Integer, Bool)
component model = do
  H.div ! A.style ("background: " <> if model^._2 then "red" else "green") @@ do
    on MouseOver \_ -> _2 .= True
    on MouseOut  \_ -> _2 .= False
    H.button ! A.style "margin: 20pt" @@ do
      on Click \MouseEvent{shiftKey = False} -> _1 += 1
      on Click \MouseEvent{shiftKey = True} -> _1 *= 2
      markup $ do
        H.span ! A.style "font-size: 20pt" $
          H.string (show (model^._1))
