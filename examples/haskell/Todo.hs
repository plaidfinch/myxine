{-# language OverloadedStrings, DuplicateRecordFields, NamedFieldPuns #-}
{-# language BlockArguments, RecordWildCards, TemplateHaskell #-}

module Main (main) where

import Text.Blaze.Html5 ((!), ToMarkup(..))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Data.String
import Data.Text (Text)
import qualified Data.Text as Text
import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Myxine

data Priority = Low | Medium | High

data Task = Task { _priority :: Priority, _name :: Text, _completed :: Bool }

$(makeLenses ''Task)

instance ToMarkup Priority where
  toMarkup = \case
    Low    -> H.span ! A.style "color: blue"      $ "!"
    Medium -> H.span ! A.style "color: goldenrod" $ "!!"
    High   -> H.span ! A.style "color: red"       $ "!!!"

todoList :: WithinPage => Reactive [Task]
todoList = H.div ! A.style "padding: 20pt; padding-left: 40pt" @@ do
  markup $ H.h3 "To do:"
  H.button ! A.style "margin-left: 2em" @@ do
    markup $ H.span ! A.style "font-size: 12pt" $ "Add Task"
    on Click \_ ->
      modify (Task { _priority = Low, _name = "", _completed = False } :)
  H.ul ! A.style "list-style-type: none;" @@ each ## do
    done <- view completed
    when (not done) $ H.li @@ taskItem
  markup $ H.h3 "Completed:"
  H.ul ! A.style "list-style-type: none;" @@ each ## do
    done <- view completed
    when done $ H.li @@ taskItem

taskItem :: WithinPage => Reactive Task
taskItem = H.div @@ do
  completed ## checkBox
  name ## H.span ! A.style "margin-left: 3pt" @@ textInputBox
  priority ## H.span ! A.style "font-size: 14pt" @@ priorityToggle

checkBox :: Reactive Bool
checkBox =
  H.span ! A.style "font-weight: bold; cursor: pointer; font-size: 14pt" @@ do
    checked <- ask
    if checked then "☑" else "☐"
    on Click \_ -> modify not

textInputBox :: WithinPage => Reactive Text
textInputBox = target do
  currentValue <- asks (fromString . Text.unpack)
  markup $ H.input ! A.value currentValue
  e <- this
  on Input \_ -> do
    value <- eval $ e <> ".value"
    put value

priorityToggle :: Reactive Priority
priorityToggle = do
  H.span ! A.style "font-weight: bold; cursor: pointer; padding: 6pt" @@ do
    markup =<< ask
    on Click \_ ->
      modify \case
        Low -> Medium
        Medium -> High
        High -> Low

main :: IO ()
main = do
  page <- runPage mempty (pure []) (reactive todoList)
  _ <- waitPage page
  pure ()
