module Myxine.Reactive
  ( Reactive, ReactiveM, reactive, title, markup,
    on', on, Propagation(..), (@@), (##), target, this
  ) where

import Text.Blaze.Html5 (Html, ToMarkup(..), string, (!), dataAttribute)
import Text.Blaze.Renderer.Text
import Text.Blaze.Internal (Attributable)

import Data.String
import Data.List (intercalate)
import Control.Monad.State
import Control.Monad.Reader
import Data.Monoid
import Data.Text (Text)
import Data.List.NonEmpty (NonEmpty(..), (<|))
import qualified Data.List.NonEmpty as NonEmpty
import qualified Data.Text.Lazy as Text
import Control.Spoon (teaspoonWithHandles)
import qualified Control.Exception as Exception
import Control.Lens hiding ((<|))

import Myxine.Event
import qualified Myxine.Direct as Direct (PageContent, pageBody, pageTitle)
import Myxine.Handlers

-- | The builder state for a reactive component.
data ReactiveBuilder model =
  ReactiveBuilder
    { location   :: !(NonEmpty Word)
    -- ^ The current location in the tree of listener scopes we've created.
    -- Calls to 'on' refer to the enclosing scope (that is, the tail of this
    -- list).
    , handlers   :: !(Handlers model)
    -- ^ The accumulated handlers for events built up so far.
    , pageMarkup :: !Html
    -- ^ The accumulated markup for the page built up so far.
    , pageTitle  :: !(Last Text)
    -- ^ The most-recently set title for the page (via 'title').
    }

-- | The underlying builder monad for the 'Reactive' type.
--
-- This is almost always used with a return type of @()@, hence you will usually
-- see it aliased as 'Reactive' @model@.
newtype ReactiveM model a =
  ReactiveM (ReaderT model (State (ReactiveBuilder model)) a)
  deriving newtype (Functor, Applicative, Monad, MonadReader model)

-- | The 'Reactive' type interleaves the description of page markup with the
-- specification of event handlers for the page.
--
-- It is a 'Monoid' and its underlying type 'ReactiveM' is a 'Monad', which
-- means that just like the Blaze templating library, it can be (and is designed
-- to be!) used in @do@-notation. Importantly, it is also a 'MonadReader'
-- @model@, where the current model is returned by 'ask'.
type Reactive model = ReactiveM model ()

-- | Wrap an inner reactive component in some enclosing HTML. Any listeners
-- created via 'on' in the wrapped component will be scoped to only events that
-- occur within this chunk of HTML.
--
-- In the following example, we install a 'Click' handler for the whole page,
-- then build a @<div>@ with /another/ 'Click' handler inside that page, which
-- returns 'Stop' from 'on'' to stop the 'Click' event from bubbling out to the
-- outer handler when the inner @<div>@ is clicked.
--
-- @
-- do 'on' 'Click' $ \\_ ->
--      liftIO $ putStrLn "Clicked outside!"
--    div ! style "background: lightblue;" '@@' do
--      "Click here, or elsewhere..."
--      'on'' 'Click' $ \\_ -> do
--        liftIO $ putStrLn "Clicked inside!"
--        pure 'Stop'
-- @
(@@) :: (Html -> Html) -> ReactiveM model a -> ReactiveM model a
wrap @@ ReactiveM inner = ReactiveM do
  builder <- get
  model <- ask
  let originalLoc = location builder
      (result, builder') =
        runState
          (runReaderT inner model)
          builder
            { location = 0 <| originalLoc  -- descend a level in location cursor
            , pageMarkup = mempty
            -- handlers & pageTitle are threaded through and preserved
            }
  put builder'
    { location = let (h :| t) = originalLoc in (h + 1 :| t)  -- move sideways
    , pageMarkup =
      do pageMarkup builder
         wrapInTarget originalLoc $ wrap (pageMarkup builder')
    -- handlers & pageTitle are threaded through and preserved
    }
  pure result
  where
    wrapInTarget :: NonEmpty Word -> Html -> Html
    wrapInTarget loc = (! dataAttribute clientDataAttr (showLoc loc))
infixr 5 @@

-- | Create a scope for event listeners without wrapping any enclosing HTML. Any
-- listeners created via 'on' will apply only to HTML that is written inside
-- this block, rather than to the enclosing broader scope.
--
-- >>> target === (id @@)
target :: ReactiveM model a -> ReactiveM model a
target = (id @@)

-- | Return a piece of JavaScript code which looks up the object corresponding
-- to the current scope's location in the page. This is suitable to be used in
-- 'eval', for instance, to retrieve properties of a particular element.
--
-- If there is no enclosing '@@', then this is the @window@ object; otherwise,
-- it is the outermost HTML element object created by the first argument to the
-- enclosing '@@'. If there are multiple elements at the root of the enclosing
-- '@@', then the first of these is selected.
--
-- For example, here's an input which reports its own contents:
--
-- @
-- textbox :: Reactive Text
-- textbox = input @@ do
--   e <- this
--   on Input \_ -> do
--     value <- eval $ this <> ".value"
--     put value
-- @
this :: ReactiveM model Text
this = ReactiveM do
  loc <- gets (NonEmpty.tail . location)
  pure case loc of
    [] -> "window"
    h : t ->
      let selector = "[data-" <> clientDataAttr <> "=\"" <> showLoc (h :| t) <> "\"]"
      in "(document.querySelector('" <> selector <> "'))"

-- | Write an atomic piece of HTML (or anything that can be converted to it) to
-- the page in this location. Event listeners for its enclosing scope can be
-- added by sequential use of 'on'. If you need sub-pieces of this HTML to have
-- their own scoped event listeners, use '@@' to build a composite component.
--
-- >>> markup h === const (toMarkup h) @@ pure ()
markup :: ToMarkup h => h -> Reactive model
markup h = const (toMarkup h) @@ pure ()

-- | Set the title for the page. If this function is called multiple times in
-- one update, the most recent call is used.
title :: Text -> Reactive model
title t = ReactiveM (modify \wb -> wb { pageTitle = Last (Just t) })

-- | Listen to a particular event and react to it by modifying the model for the
-- page. This function's returned 'Propagation' value specifies whether or not
-- to propagate the event outwards to other enclosing contexts. The event target
-- is scoped to the enclosing '@@', or the whole page if at the top level.
--
-- When the specified 'EventType' occurs, the event handler will be called with
-- that event type's corresponding property record, e.g. a 'Click' event's
-- handler will receive a 'MouseEvent' record. A handler can modify the page's
-- model via 'State'ful actions and perform arbitrary IO using 'liftIO'. In the
-- context of a running page, a handler also has access to the 'eval' and
-- 'evalBlock' functions to evaluate JavaScript in that page.
--
-- __Exception behavior:__ This function catches @PatternMatchFail@ exceptions
-- thrown by the passed function. That is, if there is a partial pattern match
-- in the pure function from event properties to stateful update, the stateful
-- update will be silently skipped. This is useful as a shorthand to select only
-- events of a certain sort, for instance:
--
-- @
-- 'on'' 'Click' \\'MouseEvent'{shiftKey = True} ->
--   do putStrLn "Shift + Click!"
--      pure 'Bubble'
-- @
on' ::
  EventType props ->
  (props -> StateT model IO Propagation) ->
  Reactive model
on' event reaction = ReactiveM do
  loc <- gets (NonEmpty.tail . location)
  let selector =
        case loc of
          [] -> window
          (h : t) -> ("data-" <> clientDataAttr) `attrIs`
            Text.toStrict (Text.pack (showLoc (h :| t)))
  modify \builder ->
    builder { handlers = mappend (handlers builder) $
              onEvent event [selector] $
              \props model ->
                -- We need to do a pure and impure catch, because GHC might
                -- decide to inline things inside the IO action, or it might
                -- not! So we check in both circumstances.
                case tryMatch (runStateT (reaction props) model) of
                  Nothing -> pure (Bubble, model)
                  Just io ->
                    do result <- Exception.try @Exception.PatternMatchFail io
                       case result of
                         Left _ -> pure (Bubble, model)
                         Right update -> pure update }
  where
    tryMatch = teaspoonWithHandles
      [Exception.Handler \(_ :: Exception.PatternMatchFail) -> pure Nothing]

-- | Listen to a particular event and react to it by modifying the model for the
-- page. This is a special case of 'on'' where the event is always allowed to
-- bubble out to listeners in enclosing contexts.
--
-- See the documentation for 'on''.
on ::
  EventType props ->
  (props -> StateT model IO ()) ->
  Reactive model
on event action = on' event (\props -> action props >> pure Bubble)

-- | Focus a reactive page fragment to manipulate a piece of a larger model,
-- using a 'Traversal'' to specify what part(s) of the larger model to
-- manipulate.
--
-- This is especially useful when creating generic components which can be
-- re-used in the context of many different models. For instance, we can define
-- a toggle button and specify separately which part of a model it toggles:
--
-- @
-- toggle :: 'Reactive' Bool
-- toggle =
--   button '@@' do
--     active <- 'ask'
--     if active then \"ON\" else \"OFF\"
--     'on' 'Click' \\_ -> 'modify' not
--
-- twoToggles :: 'Reactive' (Bool, Bool)
-- twoToggles = do
--   _1 '##' toggle
--   _2 '##' toggle
-- @
--
-- This function takes a 'Traversal'', which is strictly more general than a
-- 'Lens''. This means you can use traversals with zero or more than one target,
-- and this many replicas of the given 'Reactive' fragment will be generated,
-- each separately controlling its corresponding portion of the model. This
-- means the above example could also be phrased:
--
-- @
-- twoToggles :: 'Reactive' (Bool, Bool)
-- twoToggles = 'each' '##' toggle
-- @
(##) :: Traversal' model model' -> Reactive model' -> Reactive model
l ## ReactiveM action =
  ReactiveM $
    ReaderT \model ->
      iforOf_ (indexing l) model \i model' ->
        StateT \b@ReactiveBuilder{handlers = priorHandlers} ->
          let b' = flip execState (b {handlers = mempty}) $
                runReaderT action model'
          in Identity $
            ((), b' { handlers =
                      priorHandlers <>
                      focusHandlers (indexing l . index i) (handlers b')
                    })
infixr 5 ##

-- | Evaluate a reactive component to produce a pair of 'Direct.PageContent' and
-- 'Handlers'. This is the bridge between the 'Direct.runPage' abstraction and
-- the 'Reactive' abstraction: use this to run a reactive component in a
-- 'Myxine.Page'.
reactive :: Reactive model -> model -> (Direct.PageContent, Handlers model)
reactive (ReactiveM action) model =
  let ReactiveBuilder{handlers, pageMarkup, pageTitle = pageContentTitle} =
        execState (runReaderT action model) initialBuilder
      pageContentBody = renderMarkup pageMarkup
  in (Direct.pageBody (Text.toStrict pageContentBody)
      <> foldMap Direct.pageTitle pageContentTitle,
      handlers)
  where
    initialBuilder = ReactiveBuilder
      { location = (0 :| [])
      , handlers = mempty
      , pageMarkup = pure ()
      , pageTitle = Last Nothing }

-- | 'Reactive' pages can be combined using '<>', which concatenates their HTML
-- content and merges their sets of 'Handlers'.
instance Semigroup a => Semigroup (ReactiveM model a) where
  m <> n = (<>) <$> m <*> n

-- | The empty 'Reactive' page, with no handlers and no content, is 'mempty'.
instance Monoid a => Monoid (ReactiveM model a) where mempty = pure mempty

-- | You can apply an HTML attribute to any 'Reactive' page using '!'.
instance Attributable (ReactiveM model a) where
  w ! a = (! a) @@ w

-- | You can apply an HTML attribute to any function between 'Reactive' pages
-- using '!'. This is useful when building re-usable widget libraries, allowing
-- their attributes to be modified after the fact but before they are filled
-- with contents.
instance Attributable (ReactiveM model a -> ReactiveM model a) where
  f ! a = (! a) . f

-- | A string literal is a 'Reactive' page containing that selfsame text.
instance (a ~ ()) => IsString (ReactiveM model a) where
  fromString = markup . string

-- | The in-browser name for the data attribute holding our tracking id. This is
-- not the same as the @id@ attribute, because this means the user is free to
-- use the _real_ @id@ attribute as they please.
clientDataAttr :: IsString a => a
clientDataAttr = "myxine-client-widget-id"

-- | Helper function to show a location in the page: add hyphens between every
-- number.
showLoc :: IsString a => (NonEmpty Word) -> a
showLoc = fromString . intercalate "-" . map show . NonEmpty.toList
