{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass #-}

module Myxine.TH where

import Language.Haskell.TH
import Language.Haskell.TH.Lib
import Data.Traversable
import Data.Foldable
import qualified Data.HashSet as HashSet
import Data.HashSet (HashSet)
import qualified GHC.Generics as Generic
import qualified Data.Kind
import qualified Data.Aeson as JSON
import qualified Data.Aeson.TH as JSON
import qualified Data.Char as Char
import qualified Data.Text as Text
import Data.Text (Text)
import qualified Data.HashMap.Lazy as HashMap
import Data.HashMap.Lazy (HashMap)
import Data.List
import Data.GADT.Compare
import Data.ByteString.Lazy (ByteString)

-- Template Haskell to generate stuff

eventTypeName :: Name
eventTypeName = mkName "Event"

interfaceTypes :: HashMap String Name
interfaceTypes = HashMap.fromList
  [ ("f64", ''Double)
  , ("i64", ''Int)
  , ("String", ''Text)
  , ("bool", ''Bool)
  ]

data EnabledEvents name
  = EnabledEvents
  { events :: Events name
  , interfaces :: Interfaces name
  } deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

newtype Events name
  = Events (HashMap name (EventInfo name))
  deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

data EventInfo name
  = EventInfo
  { interface :: name
  , nameWords :: [Text]
  } deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

newtype Interfaces name
  = Interfaces (HashMap Text (Interface name))
  deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

data Interface name
  = Interface
  { inherits :: Maybe name
  , properties :: Properties name
  } deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

newtype Properties name
  = Properties (HashMap name name)
  deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

mkEventEnum :: [([String], Name)] -> Q [Dec]
mkEventEnum events = do
  cons <- for events \(conNameWords, indexName) -> do
    let conName = concatMap (onFirst Char.toUpper) conNameWords
    pure $ gadtC [mkName conName] []
      (appT (conT eventTypeName)
        (conT indexName))
  starArrowStar <- [t|Data.Kind.Type -> Data.Kind.Type|]
  pure <$> dataD (pure []) eventTypeName [] (Just starArrowStar) cons []

mkInterface :: String -> [(String, String)] -> Q [Dec]
mkInterface interface properties =
  let badFields =
        filter (not . (flip HashMap.member interfaceTypes) . snd) properties
  in if badFields == []
  then do
    fields <- sequence
      [ pure (mkName (avoidKeywordProp interface propName),
              Bang NoSourceUnpackedness SourceLazy,
              ConT (interfaceTypes HashMap.! propType))
      | (propName, propType) <- properties ]
    dec <- dataD (pure []) (mkName interface) [] Nothing
      [recC (mkName interface) (pure <$> fields)]
      [derivClause Nothing [[t|Eq|], [t|Ord|], [t|Show|], [t|Generic.Generic|], [t|JSON.FromJSON|]]]
    pure [dec]
  else do
    for_ badFields \(propName, propType) ->
      reportError $
        "Unrecognized type \"" <> propType <> "\" for event interface property \""
        <> propName <> "\" of interface \"" <> interface <> "\""
        <>": must be one of ["
        <> intercalate ", " (map show (HashMap.keys interfaceTypes))
        <> "]"
    pure []

mkEnumGEqInstance :: Name -> Q [Dec]
mkEnumGEqInstance name = do
  TyConI (DataD _ _ _ _ cons _) <- reify name
  true <- [|Just Refl|]
  false <- [|Nothing|]
  clauses <- for cons \(GadtC [con] _ _) ->
    pure (Clause [ConP con [], ConP con []] (NormalB true) [])
  let defaultClause = Clause [WildP, WildP] (NormalB false) []
  dec <- instanceD (pure []) [t|GEq $(conT name)|]
    [pure (FunD 'geq (clauses <> [defaultClause]))]
  pure [dec]

mkEnumGCompareInstance :: Name -> Q [Dec]
mkEnumGCompareInstance name = do
  TyConI (DataD _ _ _ _ cons _) <- reify name
  lt <- [|GLT|]
  eq <- [|GEQ|]
  gt <- [|GGT|]
  let clauses = flip concatMap (diagonalize cons)
        \(less, GadtC [con] _ _, greater) ->
          map (\(GadtC [l] _ _) -> Clause [ConP l [], ConP con []] (NormalB lt) []) less
          <> [Clause [ConP con [], ConP con []] (NormalB eq) []]
          <> map (\(GadtC [g] _ _) -> Clause [ConP g [], ConP con []] (NormalB gt) []) greater
  dec <- instanceD (pure []) [t|GCompare $(conT name)|]
    [pure (FunD 'gcompare clauses)]
  pure [dec]

mkDecodeProperties :: Name -> Q [Dec]
mkDecodeProperties function = do
  TyConI (DataD _ _ _ _ cons _) <- reify eventTypeName
  decode <- [|JSON.decode|]
  let event = pure (ConT  eventTypeName)
  let clauses = flip map cons \(GadtC [con] _ _) ->
        Clause [ConP con []] (NormalB decode) []
  sig <- SigD function <$> [t| forall d. $event d -> ByteString -> Maybe d|]
  let dec = FunD function clauses
  pure [sig, dec]

avoidKeywordProp :: String -> String -> String
avoidKeywordProp interface propName
  | HashSet.member propName keywords =
    onFirst Char.toLower (removeMatchingTail "Event" interface)
    <> onFirst Char.toUpper propName
  | otherwise = propName
  where
    removeMatchingTail m i =
      let reversed = reverse i
      in if m == reverse (take (length m) reversed)
      then reverse (drop (length m) reversed)
      else i

onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (c:cs) = f c : cs

diagonalize :: [a] -> [([a], a, [a])]
diagonalize [] = error "Diagonalize: empty list"
diagonalize (a : as) = go ([], a, as)
  where
    go :: ([a], a, [a]) -> [([a], a, [a])]
    go (l, c, []) = [(l, c, [])]
    go current@(l, c, r:rs) = current : go (c:l, r, rs)

-- All reserved keywords in Haskell, including all extensions
-- Source: https://github.com/ghc/ghc/blob/master/compiler/parser/Lexer.x#L875-L934
keywords :: HashSet String
keywords = HashSet.fromList
  ["as", "case", "class", "data", "default", "deriving", "do", "else", "hiding",
   "if", "import", "in", "infix", "infixl", "infixr", "instance", "let",
   "module", "newtype", "of", "qualified", "then", "type", "where", "forall",
   "mdo", "family", "role", "pattern", "static", "stock", "anyclass", "via",
   "group", "by", "using", "foreign", "export", "label", "dynamic", "safe",
   "interruptible", "unsafe", "stdcall", "ccall", "capi", "prim", "javascript",
   "unit", "dependency", "signature", "rec", "proc"]
