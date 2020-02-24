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

do page <- 'runPage' location initialModel handlers draw
  ...
  finalModel <- 'waitPage' page
  ...
where
  location :: 'PageLocation'
  location     = 'pagePort' ... <> 'pagePath' ...  -- where to connect to the server

  initialModel :: Model
  initialModel = ...  -- model

  handlers :: 'Handlers'
  handlers     = ...  -- controller

  draw :: Model -> 'PageContent'
  draw         = ...  -- view
@

      To describe the interactive behavior of the page, we need to define:

      * @location@: the 'pagePath' and 'pagePort' to connect to the Myxine
        server. Use 'mempty' to use the default port and the root path. See the
        section on [page locations](#Locations).

      * @initialModel@: the starting value for the model of the page, which can
        be any Haskell data type of your choice.

      * @handlers@: the set of 'Handlers' for page events, which describe how to
        react to events like mouse clicks, form inputs, and more. A handler can
        modify the model of the page, and perform arbitrary 'IO' actions (though
        of course it's better style to be as pure as you can). After each
        handler is invoked, the page is immediately re-rendered to the browser
        if the model has changed. See the sections on [handling
        events](#Handling) and [manipulating pages](#Manipulating).

      * @draw@: a pure function mapping from the current state of the page's
        model to a rendered HTML view of the page in its entirety. This function
        will be called on every update to the model, so it's good to make it
        reasonably fast. This library takes an agnostic approach to HTML
        generation: it's up to you to create some 'PageContent' by generating
        some 'Data.Text.Text'. I recommend the
        [@blaze-html@](https://hackage.haskell.org/package/blaze-html) package
        for this purpose, but you can do this however you like. See the section
        on [rendering page views](#Rendering).
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
  , Handlers, on
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

      The @draw@ function required as the last argument to 'runPage' has the
      type @(model -> 'PageContent')@. As the library user, it's up to you how
      you want to render your model as 'Data.Text.Text'. Then, just wrap your
      desired @body@ contents with a call to 'pageBody' (and optionally
      combine this via 'Semigroup' with a call to 'pageTitle' to set the
      @title@), and return the resultant 'PageContent'.
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
  , JavaScript(..), evalInPage
  ) where

import Control.Monad
import Data.IORef
import Control.Exception (SomeException, throwIO)
import qualified Control.Exception as Exception
import qualified Data.Aeson as JSON
import Control.Concurrent

import Myxine.Direct
import Myxine.Target (Target, tag, attribute)
import Myxine.Handlers

-- | A handle to a running 'Page'. Create this using 'runPage', and wait for its
-- eventual result using 'waitPage'. In between, you can interact with it using
-- the rest of the functions in this module, such as 'modifyPage', 'stopPage',
-- etc.
data Page model
  = Page
    { pageActions     :: !(Chan (Maybe (model -> IO model)))
    , pageFinished    :: !(MVar (Either SomeException model))
    , pageLocation    :: !PageLocation
    , pageEventThread :: !ThreadId }

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
  Handlers model
    {- ^ The set of event 'Handlers' for events in the page -} ->
  (model -> PageContent)
    {- ^ A function to draw the @model@ as some rendered 'PageContent' (how you do this is up to you) -} ->
  IO (Page model)
    {- ^ A 'Page' handle to permit further interaction with the running page -}
runPage pageLocation initialModel handlers draw =
  do pageActions  <- newChan       -- channel for model-modifying actions
     pageFinished <- newEmptyMVar  -- signal for the page's shutdown

     -- Loop through all the events on the page, translating them to events
     pageEventThread <- forkIO $
       flip Exception.finally (writeChan pageActions Nothing) $  -- tell model thread to stop
         Exception.handle (putMVar pageFinished . Left) $  -- finished with exception
           Exception.handle @Exception.AsyncException (const (pure ())) $ -- don't track when the thread is killed
             do writeChan pageActions (Just redraw)
                withEvents pageLocation
                  (Just (handledEvents handlers))
                  \event properties targets ->
                    writeChan pageActions . Just $
                      redraw <=< handle handlers event properties targets

     -- Loop through all the actions, doing them
     _pageModelThread <- forkIO
       do model <- newIORef initialModel  -- current model of the page
          Exception.handle (putMVar pageFinished . Left) $
            let loop = readChan pageActions >>= \case
                  Nothing -> putMVar pageFinished . Right =<< readIORef model
                  Just action ->
                    do writeIORef model =<< action =<< readIORef model
                       loop
            in loop

     pure (Page { pageActions, pageLocation, pageFinished, pageEventThread })

  where
    redraw :: model -> IO model
    redraw model =
      do let pageContent = draw model
         sendUpdate pageLocation (Dynamic pageContent)
         pure model

-- | Wait for a 'Page' to finish executing and return its resultant @model@, or
-- re-throw any exception the page encountered.
--
-- This function may throw 'Req.HttpException' if it cannot connect to a running
-- instance of the server. Additionally, it will also re-throw any exception
-- that was raised by user code running within an event handler or
-- model-modifying action.
waitPage :: Page model -> IO model
waitPage Page{pageFinished, pageEventThread} =
  do result <- readMVar pageFinished
     killThread pageEventThread
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
-- Returns either a deserialized Haskell type, or a human-readable string
-- describing any error that occurred. Possible errors include:
--
-- * Any exception in the given JavaScript
--
-- * Absence of any browser window currently viewing the page (since there's no
--   way to evaluate JavaScript without a JavaScript engine)
--
-- * Evaluation timeout (default is 1000 milliseconds, but can be overridden in
--   the timeout parameter to this function
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
--   [@JSON.stringify@](https://developer.mozilla.org/docs/Web/JavaScript/Reference/Global_Objects/JSON/stringify),
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
  Maybe Int {- ^ An optional override for the default timeout of 1000 milliseconds -} ->
  JavaScript {- ^ The JavaScript to evaluate: either a 'JsExpression' or a 'JsBlock' -} ->
  IO (Either String a)
evalInPage page@Page{pageLocation} timeout js =
  do v <- newEmptyMVar
     modifyPageIO page \model ->
       do putMVar v =<< evaluateJs pageLocation timeout js
          pure model
     takeMVar v
