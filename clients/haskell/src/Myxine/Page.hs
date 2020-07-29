{-# options_haddock not-home #-}

module Myxine.Page
  (
  -- * #Top# Creating Interactive Pages
  {-| To create an interactive page, we need to build a 'Page'. A 'Page' is a
      handle to a running page in the browser, providing a stateful typed
      mapping between the view and interactions in the browser page and the
      types and actions available within Haskell. To create a 'Page', we use
      'runPage'. The beginning of a typical @myxine-client@ app looks something
      like:

@
type Model = ...

do page <- 'runPage' location initialModel drawAndHandle
  ...
  finalModel <- 'waitPage' page
  ...
where
  location :: 'PageLocation'
  location     = 'pagePort' ... <> 'pagePath' ...  -- where to connect to the server

  initialModel :: model
  initialModel = ...  -- model

  drawAndHandle :: WithinPage => model -> ('PageContent', 'Handlers')
  drawAndHandle = ...  -- view and controller
@

      To describe the interactive behavior of the page, we need to define:

      * @location@: the 'pagePath' and 'pagePort' to connect to the Myxine
        server. Use 'mempty' to use the default port and the root path. See the
        section on [page locations](#Locations).

      * @initialModel@: the starting value for the model of the page, which can
        be any Haskell data type of your choice.

      * @drawAndHandle@: a pure function mapping from the current state of the
        page's model to a rendered HTML view of the page in its entirety, and
        the new set of 'Handlers' for page events, which describe how to react
        to events like mouse clicks, form inputs, and more. A handler can modify
        the model of the page, and perform arbitrary 'IO' actions (though of
        course it's better style to be as pure as you can). After each handler
        is invoked, the page is immediately re-rendered to the browser if the
        model has changed. The @drawAndHandle@ function will be called on every
        update to the model, so it's good to make it reasonably fast.

        The @drawAndHandle@ function is run in a 'WithinPage' context, which means
        that the 'eval' and 'evalBlock' functions may be used within handlers to
        evaluate JavaScript within the context of the current page.

      This library doesn't bind you into a specific approach for HTML
      generation: you can construct some 'PageContent' by generating any
      'Data.Text.Text'. However, a good approach to creating the @drawAndHandle@
      function is to use the DSL defined in 'Myxine.Reactive', which augments
      the [blaze-html](https://hackage.haskell.org/package/blaze-html) package
      with extra constructs for declaring scoped event handlers inline with page
      markup.

      See also the sections on [rendering page views](#Rendering), [handling
      events](#Handling) and [manipulating pages](#Manipulating).
  -}
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

  -- * #Handling# Handling Events
  {-| In order to react to user events in the browser, we need to specify what
      effect each event of interest should have on the model in our 'Page'. To
      do this, 'runPage' asks that we construct up-front a set of 'Handlers'
      describing this.

      'Handlers' is a 'Monoid': the 'mempty' 'Handlers' listens to no events
      (and therefore the only way for a page initialized this way to change is
      via 'modifyPage' and similar). Singleton 'Handlers' can be created using
      the 'on' function, and they can be joined together using their 'Monoid'
      instance.
  -}
  , Handlers, onEvent, TargetFact, tagIs, attrIs, window, Propagation(..)
  , module Myxine.Target

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
      system, and this interface reflects that. In between direct modifications
      of the model with 'modifyPage' and its friends, the model may change
      arbitrarily due to event handlers (or other threads) taking actions upon
      it. However, it's guaranteed that any single modification is atomic, and
      that sequences of modifications are not re-ordered (although there may be
      things that happen in between them).
  -}
  , modifyPage, modifyPageIO, setPage, getPage

  -- * #Rendering# Rendering Page Views
  {-| The Myxine server takes care of minimizing patches and re-draws in the
      browser. You merely need say how you want your model to be rendered, and
      let it take care of the rest.

      The @drawAndHandle@ function required as the last argument to 'runPage'
      returns the type @('PageContent', 'Handlers' model)@. As the library user,
      it's up to you how you want to render your model as 'Data.Text.Text'.
      Then, just wrap your desired @body@ contents with a call to 'pageBody'
      (and optionally combine this via 'Semigroup' with a call to 'pageTitle' to
      set the @title@), and return the resultant 'PageContent', pairing it with
      the handlers for any events you wish to listen to.

      Manually constructing 'PageContent' and 'Handlers' is not usually
      necessary; most of the time, the most succinct way to build a page is to
      use the DSL provided in 'Myxine.Reactive'.
  -}
  , PageContent, pageBody, pageTitle

  -- * #Evaluating# Evaluating Raw JavaScript
  {-| It occasionally might become necessary for you to directly evaluate some
      JavaScript within the context of the browser. The most frequent reason for
      this is to query the value of some object, such as the current contents of
      a text-box, or the current window dimensions. The 'evalInPage' function is
      precisely the escape hatch to enable this. Here's how you might get the
      current window width of the browser window:

@
do Right width <- evalInPage (JsExpression "window.innerWidth")
   print (width :: Int)
@
   -}
  , JavaScript(..), evalInPage, WithinPage, eval, evalBlock
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
import Myxine.Target (Target)
import Myxine.Handlers

-- | A handle to a running 'Page'. Create this using 'runPage', and wait for its
-- eventual result using 'waitPage'. In between, you can interact with it using
-- the rest of the functions in this module, such as 'modifyPage', 'stopPage',
-- etc.
data Page model
  = Page
    { pageActions     :: !(Chan (Maybe (model -> IO model)))
    , pageFinished    :: !(MVar (Either SomeException model))
    , pageLocation    :: !PageLocation }

-- | Run an interactive page, returning a handle 'Page' through which it can be
-- interacted further, via the functions in this module (e.g. 'waitPage',
-- 'modifyPage', etc.).
--
-- This function itself is non-blocking: it immediately kicks off threads to
-- start running the page. It will not throw exceptions by itself. All
-- exceptions thrown by page threads (such as issues with connecting to the
-- server) are deferred until a call to 'waitPage'.
--
-- __Important:__ Because the GHC runtime does not wait for all threads to
-- finish when ending the main thread, you probably need to use 'waitPage' to
-- make sure your program stays alive to keep processing events.
runPage :: forall model.
  PageLocation
    {- ^ The location of the 'Page' ('pagePort' and/or 'pagePath') -} ->
  model
    {- ^ The initial @model@ of the model for the 'Page' -} ->
  (WithinPage => model -> (PageContent, Handlers model))
    {- ^ A function to draw the @model@ as some rendered 'PageContent' and produce the
         set of handlers for events on that new view of the page. Note the
         presence of the 'WithinPage' context, which means the 'eval' and
         'evalBlock' functions are available to evaluate JavaScript in the
         context of the page within 'Handlers'.
    -} ->
  IO (Page model)
    {- ^ A 'Page' handle to permit further interaction with the running page -}
runPage pageLocation initialModel drawAndHandle =
  do pageActions   :: Chan (Maybe (model -> IO model))  <- newChan
     currentUpdate :: Chan (Either PageEvent model)     <- newChan
     frames        :: Chan PageContent                  <- newChan
     eventLists    :: Chan (Maybe EventList)            <- newChan
     pageFinished  :: MVar (Either SomeException model) <- newEmptyMVar

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
                 writeChan pageActions (Just (handle handlers event))
                 loop handlers
               Right model ->
                 do let (content, handlers') =
                          withPageContext
                            (WithinPageContext pageLocation)
                            (drawAndHandle model)
                    writeChan frames content
                    loop handlers'
       in catch @SomeException (loop mempty)
            (putMVar pageFinished . Left)

     -- Processes update actions to the model and sends to update thread:
     _modelThread <- forkIO $
       let loop model =
             readChan pageActions >>= \case
               Nothing -> putMVar pageFinished (Right model)
               Just action ->
                 do model' <- action model
                    writeChan currentUpdate (Right model')
                    loop model'
       in finally
            (catch @SomeException (loop initialModel)
              (putMVar pageFinished . Left))
            (traverse_ killThread [renderThread, pollThread, updateThread])

     -- Kick off the cycle by "updating" the intial model
     writeChan pageActions (Just pure)

     pure Page{..}

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
stopPage Page{pageActions} =
  writeChan pageActions Nothing

-- | Modify the model of the page with a pure function, and update the view in
-- the browser to reflect the new model.
--
-- This is non-blocking: it is not guaranteed that when this call returns, the
-- browser is now showing the new view. However, sequential calls to
-- 'modifyPage', 'modifyPageIO', 'setPage', 'getPage', 'evalInPage', and
-- 'stopPage' are guaranteed to be executed in the order they were issued.
modifyPage :: Page model -> (model -> model) -> IO ()
modifyPage page f = modifyPageIO page (pure . f)

-- | Modify the model of the page, potentially doing arbitrary other effects in
-- the 'IO' monad, then re-draw the page to the browser. Special cases include:
-- 'modifyPage' and 'setPage'.
--
-- This is non-blocking: it is not guaranteed that when this call returns, the
-- browser is now showing the new view. However, sequential calls to
-- 'modifyPage', 'modifyPageIO', 'setPage', 'getPage', 'evalInPage', and
-- 'stopPage' are guaranteed to be executed in the order they were issued.
modifyPageIO :: Page model -> (model -> IO model) -> IO ()
modifyPageIO Page{pageActions} action =
  writeChan pageActions (Just action)

-- | Set the model of the page to a particular value, and update the view in the
-- browser to reflect the new model.
--
-- This is non-blocking: it is not guaranteed that when this call returns, the
-- browser is now showing the new view. However, sequential calls to
-- 'modifyPage', 'modifyPageIO', 'setPage', 'getPage', 'evalInPage', and
-- 'stopPage' are guaranteed to be executed in the order they were issued.
setPage :: Page model -> model -> IO ()
setPage page = modifyPage page . const

-- | Get the current model of the page, blocking until it is retrieved.
--
-- Sequential calls to 'modifyPage', 'modifyPageIO', 'setPage', 'getPage',
-- 'evalInPage', and 'stopPage' are guaranteed to be executed in the order they
-- were issued.
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

-- | Evaluate some 'JavaScript' in the context of a running 'Page', blocking
-- until the result is returned.
--
-- Returns either a deserialized Haskell type, or throws a 'JsException'
-- containing a human-readable string describing any error that occurred.
-- Possible errors include:
--
-- * Any exception in the given JavaScript
--
-- * Invalid JSON response for the result type inferred (use 'JSON.Value' if you
--   don't know what shape of data you're waiting to receive).
--
-- Further caveats:
--
-- * JavaScript @undefined@ is translated to @null@ in the results
--
-- * 'JsBlock' inputs which don't explicitly return a value result in @null@
--
-- * Return types are limited to those which can be serialized via
--   [JSON.stringify](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify),
--   which does not work for cyclic objects (like @window@, @document@, and all
--   DOM nodes), and may fail to serialize some properties for other non-scalar
--   values. If you want to return a non-scalar value like a list or dictionary,
--   construct it explicitly yourself by copying from the fields of the object
--   you're interested in.
--
--   Keep in mind that this feature has sharp edges, and is usually
--   unnecessary. In particular:
--
--   * You're evaluating an arbitrary string as JavaScript, which means there
--   are no guarantees about type safety or purity.
--
--   * It is possible that you could break the Myxine server code running in
--   the page that makes it update properly, or hang the page by passing a
--   non-terminating piece of code.
--
--   * Any modifications you make to the DOM will be immediately overwritten on
--   the next re-draw of the page. Don't do this.
--
--   * If there are multiple browser windows pointed at the same page, and the
--   result of your query differs between them, it's nondeterministic which
--   result you get back.
--
-- Sequential calls to 'modifyPage', 'modifyPageIO', 'setPage', 'getPage',
-- 'evalInPage', and 'stopPage' are guaranteed to be executed in the order they
-- were issued.
evalInPage ::
  JSON.FromJSON a =>
  Page model {- ^ The 'Page' in which to evaluate the JavaScript -} ->
  JavaScript {- ^ The JavaScript to evaluate: either a 'JsExpression' or a 'JsBlock' -} ->
  IO a
evalInPage page@Page{pageLocation} js =
  do v <- newEmptyMVar
     modifyPageIO page \model ->
       do putMVar v =<< evaluateJs pageLocation js
          pure model
     takeMVar v

-- Borrowing a technique from Data.Reflection:

-- | The @WithinPage@ class, when it is present in a context, enables the use of
-- the 'eval' and 'evalBlock' functions, which evaluate JavaScript in the
-- context of the current page. Only in the body of a call to 'runPage' is there
-- a canonical current page to refer to, which means that these functions can
-- only be used there.
class WithinPage where
  -- | Acquire the JavaScript evaluation context from the ambient page. This is
  -- only possible within a call to 'runPage', which is where the 'WithinPage'
  -- context is present.
  --
  -- This function cannot be called directly; use 'eval' or 'evalBlock' to
  -- evaluate JavaScript within a page.
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
-- | Evaluate a JavaScript __expression__ in the context of the __current__
-- __page__. See the documentation for 'evalInPage' for further caveats.
--
-- This function can only be called within 'runPage', because it requires a
-- specific @'WithinPage'@ context to know where to run the JavaScript code.
eval expression =
  let WithinPageContext location = withinPageContext
  in liftIO (evaluateJs location (JsExpression expression))
  
-- | Evaluate a JavaScript __block__ in the context of the __current page__.
-- See the documentation for 'evalInPage' for further caveats.
--
-- This function can only be called within 'runPage', because it requires a
-- specific @'WithinPage'@ context to know where to run the JavaScript code.
evalBlock block =
  let WithinPageContext location = withinPageContext
  in liftIO (evaluateJs location (JsBlock block))
