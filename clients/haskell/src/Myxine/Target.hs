{-# options_haddock not-home #-}

module Myxine.Target (Target, attribute, tag) where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import GHC.Generics

-- | A 'Target' is a description of a single element node in the browser. When
-- an event fires in the browser, Myxine tracks the path of nodes it touches,
-- from the most specific element all the way up to the root. Each event handler
-- is given access to this @['Target']@, ordered from most to least specific.
--
-- For any 'Target', you can query the value of any of an 'attribute', or you
-- can ask for the 'tag' of that element.
data Target = Target
  { tagName :: Text
  , attributes :: HashMap Text Text
  } deriving (Eq, Ord, Show, Generic, JSON.FromJSON)

-- | Get the value, if any, of some named attribute of a 'Target'.
attribute :: Text -> Target -> Maybe Text
attribute name Target{attributes} = HashMap.lookup name attributes
{-# INLINE attribute #-}

-- | Get the name of the HTML tag for this 'Target'. Note that unlike in the
-- browser itself, Myxine returns tag names in lower case, rather than upper.
tag :: Target -> Text
tag Target{tagName} = tagName
{-# INLINE tag #-}
