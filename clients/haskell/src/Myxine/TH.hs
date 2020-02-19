{-# language GADTs, DataKinds, DuplicateRecordFields, RankNTypes, StrictData,
    ScopedTypeVariables, BlockArguments, KindSignatures, TemplateHaskell,
    OverloadedStrings, DerivingStrategies, DerivingVia, StandaloneDeriving,
    DeriveGeneric, DeriveAnyClass, NamedFieldPuns, GeneralizedNewtypeDeriving,
    TupleSections, LambdaCase #-}
{-# options_ghc -Wno-incomplete-uni-patterns #-}

module Myxine.TH (mkEventsAndInterfaces) where

import qualified Data.Aeson           as JSON
import           Data.Bifunctor
import qualified Data.ByteString      as ByteString.Strict
import           Data.ByteString.Lazy (ByteString)
import qualified Data.Char            as Char
import           Data.Dependent.Map   (Some(..))
import           Data.Either
import           Data.Foldable
import           Data.GADT.Compare
import           Data.HashMap.Lazy    (HashMap)
import qualified Data.HashMap.Lazy    as HashMap
import           Data.HashSet         (HashSet)
import qualified Data.HashSet         as HashSet
import qualified Data.Kind
import           Data.List
import           Data.Ord
import           Data.Text            (Text)
import           Data.Traversable
import qualified GHC.Generics         as Generic
import           Language.Haskell.TH

eventTypeName, decodeEventPropertiesName, decodeSomeEventTypeName, encodeEventNameType :: Name
eventTypeName             = mkName "EventType"
decodeEventPropertiesName = mkName "decodeEventProperties"
decodeSomeEventTypeName   = mkName "decodeSomeEventType"
encodeEventNameType       = mkName "encodeEventType"

interfaceTypes :: HashMap String Name
interfaceTypes = HashMap.fromList
  [ ("f64", ''Double)
  , ("i64", ''Int)
  , ("String", ''Text)
  , ("bool", ''Bool)
  ]

mkEventsAndInterfaces :: ByteString.Strict.ByteString -> Q [Dec]
mkEventsAndInterfaces enabledEventsByteString =
  case JSON.eitherDecodeStrict' enabledEventsByteString of
    Right EnabledEvents{events, interfaces} -> do
      interfaceDecs <- mkInterfaces interfaces
      eventDecs <- mkEvents events
      pure $ interfaceDecs <> eventDecs
    Left err -> do
      reportError err
      pure []

data EnabledEvents
  = EnabledEvents
  { events :: Events
  , interfaces :: Interfaces
  } deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

newtype Events
  = Events (HashMap String EventInfo)
  deriving (Eq, Ord, Show)
  deriving newtype (JSON.FromJSON)

data EventInfo
  = EventInfo
  { interface :: String
  , nameWords :: [String]
  } deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

newtype Interfaces
  = Interfaces (HashMap String Interface)
  deriving (Eq, Ord, Show, Generic.Generic)
  deriving newtype (JSON.FromJSON)

data Interface
  = Interface
  { inherits :: Maybe String
  , properties :: Properties
  } deriving (Eq, Ord, Show, Generic.Generic, JSON.FromJSON)

newtype Properties
  = Properties (HashMap String String)
  deriving (Eq, Ord, Show, Generic.Generic)
  deriving newtype (Semigroup, Monoid, JSON.FromJSON)

allInterfaceProperties :: Interfaces -> String -> Either (Either (Maybe String) [String]) Properties
allInterfaceProperties (Interfaces interfaces) = go HashSet.empty []
  where
    go :: HashSet String -> [String] -> String -> Either (Either (Maybe String) [String]) Properties
    go seen seenList name
      | HashSet.member name seen = Left (Right (name : seenList))
      | otherwise = do
        Interface{inherits, properties} <-
          maybe (Left (Left (if length seenList <= 1 then Just name else Nothing)))
                Right
                (HashMap.lookup name interfaces)
        rest <- maybe (pure mempty) (go (HashSet.insert name seen) (name : seenList)) inherits
        pure (properties <> rest)

fillInterfaceProperties :: Interfaces -> Either [(String, Either (Maybe String) [String])] Interfaces
fillInterfaceProperties i@(Interfaces interfaces) =
  if bad == []
  then (Right good)
  else (Left bad)
  where
    good :: Interfaces
    bad :: [(String, Either (Maybe String) [String])]
    (bad, good) =
      second (Interfaces . HashMap.fromList)
      . partitionEithers
      . map (\(name, maybeInterface) ->
               either (Left . (name,)) (Right . (name,)) maybeInterface)
      $ results

    results :: [(String, Either (Either (Maybe String) [String]) Interface)]
    results = map (\(name, Interface{inherits}) ->
                      (name, (\properties -> Interface{inherits, properties})
                             <$> allInterfaceProperties i name))
              (HashMap.toList interfaces)

mkEvents :: Events -> Q [Dec]
mkEvents (Events events) = do
  cons <- for (sortBy (comparing (interface . snd) <> comparing fst) $
               HashMap.toList events)
    \(eventName, EventInfo{interface, nameWords}) -> do
      let conName = concatMap (onFirst Char.toUpper) nameWords
      (eventName,) <$>
       gadtC [mkName conName] []
        (appT (conT eventTypeName)
          (conT (mkName interface)))
  starArrowStar <- [t|Data.Kind.Type -> Data.Kind.Type|]
  dec <- dataD (pure []) eventTypeName [] (Just starArrowStar) (pure <$> map snd cons) []
  eqInstance   <- deriveEvent [t|Eq|]
  ordInstance  <- deriveEvent [t|Ord|]
  showInstance <- deriveEvent [t|Show|]
  geqInstance           <- mkEnumGEqInstance eventTypeName (map snd cons)
  gcompareInstance      <- mkEnumGCompareInstance eventTypeName (map snd cons)
  encodeEventType       <- mkEncodeEventType cons
  decodeSomeEventType   <- mkDecodeSomeEventType cons
  decodeEventProperties <- mkDecodeEventProperties (map snd cons)
  pure $ decodeSomeEventType <> decodeEventProperties <> encodeEventType <>
         [ dec
         , eqInstance
         , ordInstance
         , showInstance
         , geqInstance
         , gcompareInstance
         ]
  where
    deriveEvent typeclass =
      standaloneDerivD (pure []) [t|forall d. $typeclass ($(pure (ConT eventTypeName)) d)|]

mkInterfaces :: Interfaces -> Q [Dec]
mkInterfaces interfaces =
  case fillInterfaceProperties interfaces of
    Right (Interfaces filledInterfaces) ->
      concat <$> for (reverse . sortOn fst $ HashMap.toList filledInterfaces)
        \(name, interface) ->
          mkInterface name interface
    Left wrong -> do
      for_ wrong \(interface, err) ->
        case err of
          Left Nothing -> pure ()
          Left (Just directUnknown) ->
            reportError $ "Unknown interface \"" <> directUnknown
                          <> "\" inherited by \"" <> interface <> "\""
          Right cyclic ->
            reportError $ "Cycle in interface inheritance: "
                          <> intercalate " <: " (reverse cyclic)
      pure []

mkInterface :: String -> Interface -> Q [Dec]
mkInterface interfaceName Interface{properties = Properties properties} =
  let propertyList = HashMap.toList properties
      badFields =
        filter (not . (flip HashMap.member interfaceTypes) . snd) propertyList
  in if badFields == []
  then do
    fields <- sequence
      [ pure (mkName (avoidKeywordProp interfaceName propName),
              Bang NoSourceUnpackedness SourceStrict,
              ConT (interfaceTypes HashMap.! propType))
      | (propName, propType) <- propertyList ]
    dec <- dataD (pure []) (mkName interfaceName) [] Nothing
      [recC (mkName interfaceName) (pure <$> sort fields)]
      [derivClause Nothing [[t|Eq|], [t|Ord|], [t|Show|], [t|Generic.Generic|],
                            [t|JSON.FromJSON|], [t|JSON.ToJSON|]]]
    pure [dec]
  else do
    for_ badFields \(propName, propType) ->
      reportError $
        "Unrecognized type \"" <> propType <> "\" for event interface property \""
        <> propName <> "\" of interface \"" <> interfaceName <> "\""
        <>": must be one of ["
        <> intercalate ", " (map show (HashMap.keys interfaceTypes))
        <> "]"
    pure []

mkEnumGEqInstance :: Name -> [Con] -> Q Dec
mkEnumGEqInstance name cons = do
  true <- [|Just Refl|]
  false <- [|Nothing|]
  clauses <- for cons \(GadtC [con] _ _) ->
    pure (Clause [ConP con [], ConP con []] (NormalB true) [])
  let defaultClause = Clause [WildP, WildP] (NormalB false) []
  dec <- instanceD (pure []) [t|GEq $(conT name)|]
    [pure (FunD 'geq (clauses <> [defaultClause]))]
  pure dec

mkEnumGCompareInstance :: Name -> [Con] -> Q Dec
mkEnumGCompareInstance name cons = do
  arg1 <- newName "a"
  arg2 <- newName "b"
  cases <- for (diagonalize cons)
        \(less, GadtC [con] _ _, greater) ->
          match (conP con []) (normalB (caseE (varE arg2)
          (concat [ map (\(GadtC [l] _ _) -> match (conP l   []) (normalB [|GLT|]) []) less
                  ,                        [ match (conP con []) (normalB [|GEQ|]) [] ]
                  , map (\(GadtC [g] _ _) -> match (conP g   []) (normalB [|GGT|]) []) greater ]))) []
  dec <- instanceD (pure []) [t|GCompare $(conT name)|]
    [funD 'gcompare [clause [varP arg1, varP arg2]
                     (normalB (caseE (varE arg1) (pure <$> cases))) []]]
  pure dec

mkEncodeEventType :: [(String, Con)] -> Q [Dec]
mkEncodeEventType cons = do
  sig <- sigD encodeEventNameType [t|forall d. $(conT eventTypeName) d -> ByteString|]
  dec <- funD encodeEventNameType
    [ clause [conP con []] (normalB (litE (stringL string))) []
    | (string, GadtC [con] _ _) <- cons ]
  pure [sig, dec]

-- | Make the @decodeEventProperties@ function
mkDecodeEventProperties :: [Con] -> Q [Dec]
mkDecodeEventProperties cons = do
  let event = pure (ConT eventTypeName)
  let cases = flip map cons \(GadtC [con] _ _) ->
        match (conP con []) (normalB [|JSON.decode|]) []
  sig <- sigD decodeEventPropertiesName [t| forall d. $event d -> ByteString -> Maybe d|]
  arg <- newName "event"
  dec <- funD decodeEventPropertiesName
           [clause [varP arg] (normalB (caseE (varE arg) cases)) []]
  let prag = PragmaD (InlineP decodeEventPropertiesName Inline FunLike AllPhases)
  pure [sig, dec, prag]

mkDecodeSomeEventType :: [(String, Con)] -> Q [Dec]
mkDecodeSomeEventType cons = do
  allEvents <- newName "allEvents"
  let list =
        [ [|($(litE (stringL string)), Some $(conE con))|]
        | (string, GadtC [con] _ _) <- cons ]
  allEventsSig <- sigD allEvents [t|HashMap ByteString (Some $(conT eventTypeName))|]
  allEventsDec <- funD allEvents [clause [] (normalB [|HashMap.fromList $(listE list)|]) []]
  sig <- sigD decodeSomeEventTypeName
    [t| ByteString -> Maybe (Some $(conT eventTypeName))|]
  dec <- funD decodeSomeEventTypeName
    [clause [] (normalB [|flip HashMap.lookup $(varE allEvents)|])
      [pure allEventsSig, pure allEventsDec]]
  pure [sig, dec]

-- | Given an interface and a property for it, rename that property if necessary
-- to avoid clashing with reserved Haskell keywords
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

-- | Apply a function to the first element of a list
onFirst :: (a -> a) -> [a] -> [a]
onFirst _ [] = []
onFirst f (c:cs) = f c : cs

-- | Get all zipper positions into a list
diagonalize :: [a] -> [([a], a, [a])]
diagonalize [] = []
diagonalize (a : as) = go ([], a, as)
  where
    go :: ([a], a, [a]) -> [([a], a, [a])]
    go (l, c, []) = [(l, c, [])]
    go current@(l, c, r:rs) = current : go (c:l, r, rs)

-- | All reserved keywords in Haskell, including all extensions
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
