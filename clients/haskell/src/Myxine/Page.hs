{-# options_haddock not-home #-}

module Myxine.Page
  (
  -- * #Top# Creating Interactive Pages
{-|

To create an interactive page, we need to build a 'Page'. A 'Page' is a handle
to a running page in the browser, providing a stateful typed mapping between the
view and interactions in the browser page and the types and actions available
within Haskell. To create a 'Page', we use 'runPage':

@
'Myxine.Page.runPage' ::
  'Direct.PageLocation' ->
  model ->
  ('Myxine.WithinPage' => model -> ('Direct.PageContent', 'Handlers' model)) ->
  IO ('Myxine.Page.Page' model)
@

The beginning of a typical @myxine-client@ app looks something like:

@
data Model = ...  -- the model that defines the page's current state

do page <- 'runPage' location (pure initialModel) ('reactive' . component)
   finalModel <- 'waitPage' page
where
  location :: 'PageLocation'
  location = 'pagePort' 1123 <> 'pagePath' \'/\'  -- where to connect to the server

  initialModel :: Model
  initialModel = ...  -- the initial state of the model

  component :: 'WithinPage' => 'Reactive' Model
  component = ...  -- how to render the model and listen for events that would update it
@

To describe the interactive behavior of the page, we need to define:

* __@location@:__ the 'pagePath' and 'pagePort' to connect to the Myxine server.
Use 'mempty' to use the default port and the root path. See the section on [page
locations](#Locations).

* __@initialModel@:__ the starting value for the model of the page, which can be
any Haskell data type of your choice.

* __@component@:__ an interleaved description in the 'Reactive' monad explaining
both how to render the current state of the model as HTML, and how to handle
events that occur within that selfsame HTML by updating the model and performing
IO.

See also the sections on [building reactive pages](#Building) and [manipulating
pages](#Manipulating).
-}
-- ** #NoReactive# Using 'Page' without 'Reactive'
{-|
Although the 'Reactive' abstraction is typically the most convenient way to
build a 'Page', the 'runPage' abstraction is not bound to a specific way of
formatting HTML 'Direct.PageContent' or gathering a set of 'Handlers'. Instead,
as noted above, 'runPage' takes as input any function of type
@'Myxine.WithinPage' => model -> ('Direct.PageContent', 'Handlers' model)@. We
provide the 'Reactive'-built pages to 'runPage' by /evaluating/ them using:

@
'reactive' :: 'Reactive' model -> model -> ('Direct.PageContent', 'Handlers' model)
@

This might not always suit your desires, though, and that's precisely why it's
not baked in. You are free to construct 'PageContent' using 'pageTitle' and
'pageBody', and to construct 'Handlers' using 'Myxine.Handlers.onEvent' and
'<>', avoiding the 'Reactive' abstraction altogether if you so choose.
-}
  -- * #Pages# Creating and Waiting For Pages
  Page, runPage, waitPage, stopPage

  -- * #Locations# Specifying Page Locations
  {-| If you are building a single-page application using Myxine, and you don't
      intend to share its address space, you don't need to change the default
      settings for the 'PageLocation': 'mempty' will do. However, the Myxine
      server will gladly host your page at any path you desire; just use
      'pagePath' to specify. Similarly, use 'pagePort' to specify if the Myxine
      server is running on a different port than its default of 1123.
  -}
  , PageLocation, pagePath, pagePort

  -- * #Building# Building Reactive Pages
  {-| When using the 'Reactive' DSL for building pages, it's intended that you
      import it alongside the 'Text.Blaze.Html5' and
      'Text.Blaze.Html5.Attributes' modules from the
      [blaze-html](https://hackage.haskell.org/package/blaze-html), which
      provide the HTML combinators required for building markup.

      While nothing about this module hard-codes the use of
      [lenses](https://hackage.haskell.org/package/lens), it is often useful in
      the body of handlers given to 'on' to manipulate the model state using
      lens combinators, in particular the stateful 'Control.Lens.Setter..=' and
      friends. This library is designed to play well with the following import
      structure:

@
import Text.Blaze.Html5 ((!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Control.Monad.IO.Class
import Control.Monad.State
import Control.Monad.Reader

import Control.Lens

import Myxine
@

      __A small example:__

      Here's a simple 'Reactive' page that uses some of the combinators in this
      library, as well as the Lens combinators 'Control.Lens._1',
      'Control.Lens._2', 'Control.Lens.^.', 'Control.Lens..=',
      'Control.Lens.+=', and 'Control.Lens.*='.

      In this demonstration, you can see how the '@@' and 'on' functions work
      together to allow you to define event handlers scoped to specific regions
      of the page: handlers defined via 'on' receive only events from within the
      region delineated by the enclosing '@@'. As such, clicking on the
      background does not increment the counter, but clicking on the button
      does.

@
main :: IO ()
main = do
  page <- 'runPage' 'mempty' (0, False) ('reactive' component)
  print =<< 'waitPage' page

component :: Reactive (Integer, Bool)
component = do
  model <- 'ask'
  H.div ! A.style ("background: " '<>' if model 'Control.Lens.^.' 'Control.Lens._2' then "red" else "green") '@@' do
    'on' 'MouseOver' $ \\_ -> 'Control.Lens._2' 'Control.Lens..=' True
    'on' 'MouseOut'  $ \\_ -> 'Control.Lens._2' 'Control.Lens..=' False
    H.button ! A.style "margin: 20pt" '@@' do
      'on' 'Click' $ \\'MouseEvent'{shiftKey = False} -> 'Control.Lens._1' 'Control.Lens.+=' 1
      'on' 'Click' $ \\'MouseEvent'{shiftKey = True} -> 'Control.Lens._1' 'Control.Lens.*=' 2
      'markup' $ do
        H.span ! A.style "font-size: 20pt" $
          H.string (show (model 'Control.Lens.^.' 'Control.Lens._1'))
@
  -}
  , module Myxine.Reactive

  -- * #Evaluating# Evaluating JavaScript
  {-| The functions 'eval' and 'evalBlock' evaluate some 'JavaScript' in the
      context of the current 'Page' and return a deserialized Haskell type
      (inferred from the use site), or throw a 'JsException' containing a
      human-readable string describing any error that occurred.

      For instance, here's how we would query the width of the browser window on
      every 'Resize' event:

@
windowWidth :: 'WithinPage' => 'Reactive' Int
windowWidth = do
  currentWidth <- ask
  'markup' currentWidth
  'on' 'Resize' $ \\_ -> do
    width <- 'eval' "window.innerWidth"
    'Control.Monad.State.put' width
@

      __Possible errors__ (which manifest as 'JsException's):

        * Any exception in the given JavaScript

        * Invalid JSON response for the result type inferred (use 'JSON.Value' if you
        don't know what shape of data you're waiting to receive).

      __Further caveats:__

        * JavaScript @undefined@ is translated to @null@ in the results

        * Return types are limited to those which can be serialized via
        [JSON.stringify](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify),
        which does not work for cyclic objects (like @window@, @document@, and all
        DOM nodes), and may fail to serialize some properties for other non-scalar
        values. If you want to return a non-scalar value like a list or dictionary,
        construct it explicitly yourself by copying from the fields of the object
        you're interested in.

        * You're evaluating an arbitrary string as JavaScript, which means there
        are no guarantees about type safety or purity.

        * It is possible that you could break the Myxine server code running in
        the page that makes it update properly, or hang the page by passing a
        non-terminating piece of code.

        * Any modifications you make to the DOM will be immediately overwritten on
        the next re-draw of the page. Don't do this.

        * If there are multiple browser windows pointed at the same page, and the
        result of your query differs between them, it's nondeterministic which
        result you get back.
   -}
  , WithinPage, eval, evalBlock

  -- * #Manipulating# Manipulating Running Pages
  {-| Once a page is running, the only way to interact with its contents is via its
      'Page' handle (unless you use the methods in 'Myxine.Direct', but it is
      /strongly discouraged/ to mix the two different abstractions: you will
      almost certainly confuse yourself a lot).

      A 'Page' whose behavior relies solely on user interactions within the
      browser doesn't need any of these functions: these are the mechanism by
      which external information can be used to modify the model of a page, and
      thereby update the GUI to reflect the model.

      Keep in mind that Myxine, like most GUIs, is an inherently concurrent
      system. This interface reflects that: in between direct modifications
      of the model with 'modifyPage' and its friends, the model may change
      arbitrarily due to event handlers (or other threads) taking actions upon
      it. However, it's guaranteed that any single modification is atomic, and
      that sequences of modifications are not re-ordered (although there may be
      things that happen in between them).
  -}
  , modifyPage, modifyPageIO, setPage, getPage
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Control.Exception (SomeException, finally, throwIO, catch)
import qualified Data.Aeson as JSON
import Control.Concurrent
import Control.Concurrent.Async
import Data.List.NonEmpty (nonEmpty)
import Data.Text (Text)
import Data.Foldable
import qualified Unsafe.Coerce as Unsafe

import Myxine.Direct
import Myxine.Reactive
import Myxine.Handlers

-- | A handle to a running 'Page'. Create this using 'runPage', and wait for its
-- eventual result using 'waitPage'. In between, you can interact with it using
-- the rest of the functions in this module, such as 'modifyPage', 'stopPage',
-- etc.
data Page model
  = Page
    { pageActions     :: !(Chan (PageAction model))
    , pageFinished    :: !(MVar (Either SomeException model))
    , pageLocation    :: !PageLocation }

-- | An action to be performed in the page context. Either a call to stop the
-- page, or an effectful function to be performed on the model of the page.
data PageAction model
  = StopPage
  | PageAction !(WithinPage => model -> IO model)

-- | Run an interactive page, returning a handle 'Page' through which it can be
-- interacted further, via the functions in this module (e.g. 'waitPage',
-- 'modifyPage', etc.).
--
-- This function takes as input a 'PageLocation', an initial model, and a pure
-- function from the current state of the page's model to a rendered HTML view
-- of the page in its entirety, and the new set of 'Handlers' for page events. A
-- handler can modify the model of the page, and perform arbitrary 'IO' actions,
-- including evaluating JavaScript in the page using 'eval'. After each all
-- pertinent handlers for an event are dispatched, the page is re-rendered to
-- the browser.
--
-- This function itself is non-blocking: it immediately kicks off threads to
-- start running the page. It will not throw exceptions by itself. All
-- exceptions thrown by page threads (such as issues with connecting to the
-- server) are deferred until a call to 'waitPage'.
--
-- __Important:__ Because the GHC runtime does not wait for all threads to
-- finish when ending the main thread, you probably need to use 'waitPage' to
-- make sure your program stays alive to keep processing events.
--
-- Typical use of this function embeds a 'Reactive' page by using the 'reactive'
-- function to adapt it as the last argument (but this is not the only way to
-- use it, see [Using Page without Reactive](#NoReactive)):
--
-- @
-- 'runPage' location (pure initialModel) ('reactive' component)
-- @
runPage :: forall model.
  PageLocation
    {- ^ The location of the 'Page' (built using 'pagePort' and/or 'pagePath'). -} ->
  (WithinPage => IO model)
    {- ^ An IO action to return the initial @model@ for the 'Page'.
         Note that this is in a 'WithinPage' context and therefore can use
         'eval' and 'evalBlock'. -} ->
  (WithinPage => model -> (PageContent, Handlers model))
    {- ^ A function to draw the @model@ as some rendered 'Direct.PageContent'
         and produce the set of 'Handlers' for events on that new view of the
         page. Note that this is in a 'WithinPage' context and therefore can use
         'eval' and 'evalBlock'.
    -} ->
  IO (Page model)
    {- ^ A 'Page' handle to permit further interaction with the running page -}
runPage pageLocation getInitialModel drawAndHandle =
  do pageActions   :: Chan (PageAction model)           <- newChan
     currentUpdate :: Chan (Either PageEvent model)     <- newChan
     frames        :: Chan PageContent                  <- newChan
     eventLists    :: Chan (Maybe EventList)            <- newChan
     pageFinished  :: MVar (Either SomeException model) <- newEmptyMVar
     let inPage :: (WithinPage => a) -> a
         inPage = withPageContext (WithinPageContext pageLocation)

     -- The stream of events from the page
     nextEvent <- events pageLocation

     -- Renders the page:
     renderThread <- forkIO $
       forever (update pageLocation . Dynamic =<< readChan frames)

     -- Polls for the next event:
     pollThread <- forkIO $
       onLatest (writeChan currentUpdate . Left) $
         maybe (forever (threadDelay maxBound)) nextEvent <$>
           readChan eventLists

     -- Handles events and sends update actions to the model thread:
     updateThread <- forkIO $
       let loop handlers = do
             writeChan eventLists $
               SomeEvents <$> nonEmpty (handledEvents handlers)
             readChan currentUpdate >>= \case
               Left event -> do
                 writeChan pageActions (PageAction (handle handlers event))
                 loop handlers
               Right model ->
                 do let (content, handlers') = inPage (drawAndHandle model)
                    writeChan frames content
                    loop handlers'
       in catch @SomeException (loop mempty)
            (putMVar pageFinished . Left)

     -- Processes update actions to the model and sends to update thread:
     _modelThread <- forkIO $
       let loop model =
             readChan pageActions >>= \case
               StopPage -> putMVar pageFinished (Right model)
               PageAction action ->
                 do model' <- inPage (action model)
                    writeChan currentUpdate (Right model')
                    loop model'
       in finally
            (catch @SomeException
              (loop =<< inPage getInitialModel)
              (putMVar pageFinished . Left))
            (traverse_ killThread [renderThread, pollThread, updateThread])

     -- Kick off the cycle by "updating" the intial model
     writeChan pageActions (PageAction pure)

     pure Page{..}

-- | Race each action in a "stream" of actions against the arrival of the next
-- action, feeding the completed results into a function, but canceling actions
-- that take longer to produce a value than it takes to get the next action. In
-- other words, a sequential pre-emptible work queue.
onLatest :: (a -> IO ()) -> IO (IO a) -> IO ()
onLatest action rest = go =<< rest
  where
    go first =
      race first rest >>= \case
        Left a -> do
          action a
          next <- rest
          go next
        Right preempt -> do
          go preempt

-- | Wait for a 'Page' to finish executing and return its resultant @model@, or
-- re-throw any exception the page encountered.
--
-- This function may throw 'Req.HttpException' if it cannot connect to a running
-- instance of the server. Additionally, it will also re-throw any exception
-- that was raised by user code running within an event handler or
-- model-modifying action.
waitPage :: Page model -> IO model
waitPage Page{pageFinished} =
  do result <- readMVar pageFinished
     either throwIO pure result

-- | Politely request a 'Page' to shut down. This is non-blocking: to get the
-- final model of the 'Page', follow 'stopPage' with a call to 'waitPage'.
--
-- Before the page is stopped, all events and modifications which were pending
-- at the time of this command will be processed.
stopPage :: Page model -> IO ()
stopPage Page{pageActions, pageFinished} = do
  running <- isEmptyMVar pageFinished
  when running $ writeChan pageActions StopPage

-- | Modify the model of the page with a pure function, and update the view in
-- the browser to reflect the new model.
--
-- This function is non-blocking; the page view may not yet have been updated by
-- the time it returns.
modifyPage :: Page model -> (model -> model) -> IO ()
modifyPage page f = do
  running <- isEmptyMVar (pageFinished page)
  when running $ modifyPageIO page (pure . f)

-- | Modify the model of the page, potentially doing arbitrary other effects in
-- the 'IO' monad, then re-draw the page to the browser. The functions 'eval'
-- and 'evalBlock' are available for evaluating JavaScript within the context of
-- the current page.
--
-- This function is non-blocking; the page view may not yet have been updated by
-- the time it returns.
modifyPageIO :: Page model -> (WithinPage => model -> IO model) -> IO ()
modifyPageIO Page{pageActions} action =
  writeChan pageActions (PageAction action)

-- | Set the model of the page to a particular value, and update the view in the
-- browser to reflect the new model.
--
-- This function is non-blocking; the page view may not yet have been updated by
-- the time it returns.
setPage :: Page model -> model -> IO ()
setPage page = modifyPage page . const

-- | Get the current model of the page, blocking until it is retrieved.
--
-- __Note:__ it is not guaranteed that the model returned by this function is
-- "fresh" by the time you act upon it. That is:
--
-- @
-- 'getPage' page >>= 'setPage' page
-- @
--
-- __is not the same as__
--
-- @
-- 'modifyPage' id
-- @
--
-- This is because some other thread (notably, an event handler thread) could
-- have changed the page in between the call to 'getPage' and 'setPage'. As a
-- result, you probably don't want to use this function, except perhaps as a way
-- to extract intermediate reports on the value of the page.
getPage :: Page model -> IO model
getPage page = do
  v <- newEmptyMVar
  modifyPageIO page \model ->
    do putMVar v model
       pure model
  takeMVar v

-- Borrowing a technique from Data.Reflection:

-- | The @WithinPage@ constraint, when it is present, enables the use of the
-- 'eval' and 'evalBlock' functions. Only in the body of a call to 'runPage' or
-- 'modifyPageIO' is there a canonical current page, and it's a type error to
-- use these functions anywhere else.
class WithinPage where
  -- Acquire the JavaScript evaluation context from the ambient page. This
  -- function cannot be called directly; use 'eval' or 'evalBlock' to evaluate
  -- JavaScript within a page.
  withinPageContext :: WithinPageContext

-- Specialized version of Data.Reflection.Gift:
newtype WithinPageGift r = WithinPageGift (WithinPage => r)

-- Specialized version of Data.Reflection.give:
withPageContext :: forall r. WithinPageContext -> (WithinPage => r) -> r
withPageContext c k = Unsafe.unsafeCoerce (WithinPageGift k :: WithinPageGift r) c
{-# NOINLINE withPageContext #-}

-- A "handle" that closes over the page location
newtype WithinPageContext =
  WithinPageContext PageLocation

eval, evalBlock :: (WithinPage, JSON.FromJSON a, MonadIO m) => Text -> m a
-- | Evaluate a JavaScript __expression__ in the context of the current page.
-- The given text is automatically wrapped in a @return@ statement before being
-- evaluated in the browser.
--
-- If you try to call 'eval' outside of a call to 'runPage' or 'modifyPageIO',
--  you'll get a type error like the following:
--
-- > • No instance for WithinPage arising from a use of ‘eval’
-- > • In a stmt of a 'do' block: x <- eval @Int "1 + 1"
--
-- This means that you called it in some non-'Page' context, like in the
-- main function of your program.
eval expression =
  let WithinPageContext location = withinPageContext
  in liftIO (evaluateJs location (JsExpression expression))
  
-- | Evaluate a JavaScript __block__ in the context of the current page. Unlike
-- 'eval', the given text is /not/ automatically wrapped in a @return@
-- statement, which means that you can evaluate multi-line statements, but you
-- must provide your own @return@.
--
-- If you try to call 'evalBlock' outside of a call to 'runPage' or
--  'modifyPageIO', you'll get a type error like the following:
--
-- > • No instance for WithinPage arising from a use of ‘evalBlock’
-- > • In a stmt of a 'do' block: x <- evalBlock @Int "return 1;"
--
-- This means that you called it in some non-'Page' context, like in the
-- main function of your program.
evalBlock block =
  let WithinPageContext location = withinPageContext
  in liftIO (evaluateJs location (JsBlock block))
