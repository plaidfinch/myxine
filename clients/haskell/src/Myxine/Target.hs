{-# language NamedFieldPuns, DeriveGeneric, DeriveAnyClass #-}

module Myxine.Target where

import Data.Text (Text)
import qualified Data.Aeson as JSON
import Data.HashMap.Lazy (HashMap)
import qualified Data.HashMap.Lazy as HashMap
import GHC.Generics

-- | A 'Target' is a description of a single element node in the browser. You
-- can query the value of any of an 'attribute', or you can ask for its
-- 'tagName'.
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
