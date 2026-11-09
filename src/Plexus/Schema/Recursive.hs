-- | Shallow plugin schema for Plexus RPC
--
-- = Design
--
-- The schema is shallow: children are summaries (namespace, description, hash),
-- not full schemas. Full child schemas are fetched on demand when navigating.
--
-- This matches the coalgebraic design:
-- - Rust side: unfolds plugin structure on demand (anamorphism)
-- - Wire format: one layer at a time (shallow schema)
-- - Haskell side: folds/consumes structure (catamorphism over fetched data)
--
-- = The Functor (Conceptual)
--
-- @
-- F : Set → Set
-- F(X) = Namespace × Version × Description × Hash × [Method] × Maybe [X]
-- @
--
-- On the wire, X = ChildSummary (a reference). Resolution is lazy.
--
-- = Category Properties
--
-- The plugin system forms a free category:
-- - Objects: Schemas (identified by hash)
-- - Morphisms: Paths (sequences of child references)
-- - Identity: Empty path
-- - Composition: Path concatenation
module Plexus.Schema.Recursive
  ( -- * Core Types
    PluginSchema(..)
  , MethodSchema(..)
  , ChildSummary(..)
  , PluginHash
  , SchemaResult(..)
  , Cardinality(..)

    -- * Deprecation Metadata (IR-5)
  , DeprecationInfo(..)
  , ParamSchema(..)
  , MethodRole(..)
  , PeerInterface(..)
  , isSlotRole

    -- * The caller's view (AUTH-3 / AUTH-4)
  , View(..)
  , hasAuthColumns

    -- * Help metadata (SYNAPSE-13)
  , MethodExample(..)
  , paramValues
  , xPlexusValues

    -- * Queries
  , isParentActivation
  , isHubActivation    -- naming-alias: deprecated, removed by NAMING-18
  , isLeafActivation
  , pluginMethods
  , pluginChildren
  , childNamespaces

    -- * JSON Parsing
  , parsePluginSchema
  , parseSchemaResult
  ) where

import Control.Applicative ((<|>))

import Data.Aeson
import Data.Aeson.Types (Pair, Parser)
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Map.Strict (Map)
import GHC.Generics (Generic)

import Plexus.Auth (View(..))

-- ============================================================================
-- Deprecation Metadata (IR-5)
-- ============================================================================

-- | Structured deprecation metadata attached to activations, methods, and
--   parameter fields.
--
--   All three text fields use JSON snake_case on the wire
--   (@since@, @removed_in@, @message@). Producers emitted before IR-2
--   may omit the outer field entirely; consumers should use @.:?@ so a
--   missing value deserializes as 'Nothing' on the containing record.
data DeprecationInfo = DeprecationInfo
  { depSince     :: Text   -- ^ Version the surface became deprecated.
  , depRemovedIn :: Text   -- ^ Version at which the surface will be removed.
  , depMessage   :: Text   -- ^ Human-readable migration guidance.
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON DeprecationInfo where
  parseJSON = withObject "DeprecationInfo" $ \o -> DeprecationInfo
    <$> o .:  "since"
    <*> o .:  "removed_in"
    <*> o .:  "message"

instance ToJSON DeprecationInfo where
  toJSON DeprecationInfo{..} = object
    [ "since"      .= depSince
    , "removed_in" .= depRemovedIn
    , "message"    .= depMessage
    ]

-- | Structural role of a method in the activation graph (IR-2 / IR-3).
--
--   Mirrors the Rust @plexus_core::MethodRole@ enum. Tagged on the wire
--   with a @kind@ discriminator and @snake_case@ variant names:
--
--   @
--   {"kind": "rpc"}
--   {"kind": "static_child"}
--   {"kind": "static_child", "peer": true, "list_method": "names", "interface": {…}}
--   {"kind": "dynamic_child", "list_method": "planet_names", "search_method": null, "child": {…}}
--   @
--
--   A peer slot (NEST-7, @#[plexus_macros::peer]@) goes on the wire as a
--   @static_child@ marked @peer: true@, as plexus-core's @MethodRoleWire@
--   writes it; it is read as 'MethodRolePeer' and written back the same.
--
--   Defaults to 'MethodRoleRpc' when absent so pre-IR servers deserialize
--   cleanly (see 'MethodSchema.methodRole' parser).
data MethodRole
  = MethodRoleRpc
  | MethodRoleStaticChild
  | MethodRoleDynamicChild
      { listMethod   :: Maybe Text
      , searchMethod :: Maybe Text
      , childSchema  :: Maybe PluginSchema
        -- ^ NEST-5: the slot's type, for a typed dynamic child (@child@)
      }
  | MethodRolePeer
      -- ^ NEST-7: a slot of peers found by name at call time: the segment
      -- after the method is a peer's name (@dyn.peer.<name>.…@)
      { listMethod    :: Maybe Text
      , searchMethod  :: Maybe Text
      , peerInterface :: Maybe PeerInterface
        -- ^ a typed peer slot's interface (@interface@)
      }
  deriving stock (Show, Eq, Generic)

-- | NEST-7: a peer's shape, named and content-addressed — plexus-core's
--   @peer::Interface@: its name, the shape's hash, its IR (kept as the
--   JSON it came as), each method's own hash, and the schema it was read
--   from.
data PeerInterface = PeerInterface
  { interfaceName    :: Text
  , interfaceHash    :: Text
  , interfaceIr      :: Value
  , interfaceMethods :: Map Text Text
  , interfaceSchema  :: PluginSchema
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PeerInterface where
  parseJSON = withObject "PeerInterface" $ \o -> PeerInterface
    <$> o .:  "name"
    <*> o .:  "hash"
    <*> o .:? "ir" .!= Null
    <*> o .:? "methods" .!= mempty
    <*> o .:  "schema"

instance ToJSON PeerInterface where
  toJSON PeerInterface{..} = object
    [ "name"    .= interfaceName
    , "hash"    .= interfaceHash
    , "ir"      .= interfaceIr
    , "methods" .= interfaceMethods
    , "schema"  .= interfaceSchema
    ]

-- | Whether a role is a slot whose names are open: a dynamic child's keys
--   or a peer slot's peers (NEST-5, NEST-7).
isSlotRole :: MethodRole -> Bool
isSlotRole = \case
  MethodRoleDynamicChild{} -> True
  MethodRolePeer{}         -> True
  _                        -> False

instance FromJSON MethodRole where
  parseJSON = withObject "MethodRole" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "rpc"           -> pure MethodRoleRpc
      "static_child"  -> do
        peer <- o .:? "peer" .!= False
        if peer
          then MethodRolePeer
            <$> o .:? "list_method"
            <*> o .:? "search_method"
            -- an interface this reader cannot read leaves the slot untyped
            <*> (o .:? "interface" <|> pure Nothing)
          else pure MethodRoleStaticChild
      "dynamic_child" -> MethodRoleDynamicChild
        <$> o .:? "list_method"
        <*> o .:? "search_method"
        -- a slot type this reader cannot read leaves the slot untyped
        <*> (o .:? "child" <|> pure Nothing)
      other -> fail $ "Unknown MethodRole kind: " <> T.unpack other

-- | Encodes as plexus-core's @MethodRoleWire@: an absent optional field is
--   omitted, and a peer slot is a @static_child@ with @peer: true@.
instance ToJSON MethodRole where
  toJSON MethodRoleRpc         = object ["kind" .= ("rpc" :: Text)]
  toJSON MethodRoleStaticChild = object ["kind" .= ("static_child" :: Text)]
  toJSON (MethodRoleDynamicChild lm sm ch) =
    object $ ("kind" .= ("dynamic_child" :: Text)) : catMaybes
      [ ("list_method"   .=) <$> lm
      , ("search_method" .=) <$> sm
      , ("child"         .=) <$> ch
      ]
  toJSON (MethodRolePeer lm sm iface) =
    object $ ("kind" .= ("static_child" :: Text)) : ("peer" .= True) : catMaybes
      [ ("list_method"   .=) <$> lm
      , ("search_method" .=) <$> sm
      , ("interface"     .=) <$> iface
      ]

-- | Shallow parameter schema with optional per-field deprecation.
--
--   Not every producer emits this; synapse treats 'Nothing' as "no
--   per-field metadata available" and falls back to the raw
--   'methodParams' 'Value' for legacy rendering.
data ParamSchema = ParamSchema
  { paramName         :: Text
  , paramDescription  :: Maybe Text
  , paramRequired     :: Bool
  , paramDeprecation  :: Maybe DeprecationInfo
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ParamSchema where
  parseJSON = withObject "ParamSchema" $ \o -> ParamSchema
    <$> o .:  "name"
    <*> o .:? "description"
    <*> o .:? "required" .!= False
    <*> o .:? "deprecation"

instance ToJSON ParamSchema where
  toJSON ParamSchema{..} = object
    [ "name"        .= paramName
    , "description" .= paramDescription
    , "required"    .= paramRequired
    , "deprecation" .= paramDeprecation
    ]

-- ============================================================================
-- Core Types
-- ============================================================================

-- | Content hash for cache invalidation
type PluginHash = Text

-- | Summary of a child plugin (shallow - no methods or nested children)
--
-- This is a reference to a child, not the full schema. To get the full
-- schema, fetch it via @{path}.schema@ RPC call.
data ChildSummary = ChildSummary
  { csNamespace   :: Text
  , csDescription :: Text
  , csHash        :: PluginHash
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ChildSummary where
  parseJSON = withObject "ChildSummary" $ \o -> ChildSummary
    <$> o .: "namespace"
    -- AUTH-4: an anonymous view's children may be bare
    <*> o .:? "description" .!= ""
    <*> o .:? "hash" .!= ""

instance ToJSON ChildSummary where
  toJSON ChildSummary{..} = object
    [ "namespace"   .= csNamespace
    , "description" .= csDescription
    , "hash"        .= csHash
    ]

-- | Whether a method yields one item or many (plexus 0.7).
--
--   Mirrors the Rust @plexus_core::Cardinality@; on the wire @"one"@ /
--   @"many"@. Read from the method's signature: a plain return type is one
--   item, @impl Stream@ is many.
data Cardinality
  = CardinalityOne   -- ^ The method yields exactly one item.
  | CardinalityMany  -- ^ The method yields a stream of zero or more items.
  deriving stock (Show, Eq, Ord, Generic)

instance FromJSON Cardinality where
  parseJSON = withText "Cardinality" $ \case
    "one"  -> pure CardinalityOne
    "many" -> pure CardinalityMany
    other  -> fail $ "unknown cardinality: " <> T.unpack other

instance ToJSON Cardinality where
  toJSON CardinalityOne  = String "one"
  toJSON CardinalityMany = String "many"

-- | Schema for a single method (plexus 0.7)
--
--   A method's contract is its item type @T@ ('methodReturns'), its error
--   type @E@ when it can say no ('methodError'), its progress type @P@ when
--   it reports progress ('methodProgress'), and its 'methodCardinality'.
--   @returns@ and @cardinality@ are required on the wire: a schema that does
--   not state them is not a 0.7 schema, and is a decode error rather than a
--   guess.
data MethodSchema = MethodSchema
  { methodName            :: Text
  , methodDescription     :: Text
  , methodHash            :: PluginHash
  , methodParams          :: Maybe Value  -- ^ JSON Schema for params
  , methodReturns         :: Value        -- ^ JSON Schema for the item type @T@ — what one success carries (for @Result<T, E>@ or @Progress<T, P>@, @T@ alone)
  , methodError           :: Maybe Value  -- ^ JSON Schema for the error type @E@ (@-> Result<T, E>@); 'Nothing' when the method is infallible
  , methodProgress        :: Maybe Value  -- ^ JSON Schema for the progress type @P@ carried by progress frames; 'Nothing' when the method reports no progress
  , methodCardinality     :: Cardinality  -- ^ One item or many
  , methodHttpMethod      :: Text         -- ^ HTTP method of the method's REST endpoint (@"GET"@, @"POST"@, …); @"POST"@ when absent
  , methodDeprecation     :: Maybe DeprecationInfo  -- ^ Deprecation info, if any (IR-5)
  , methodParamSchemas    :: Maybe [ParamSchema]    -- ^ Optional structured param info with per-field deprecations (IR-5)
  , methodRole            :: MethodRole             -- ^ Structural role in the activation graph (IR-2 / IR-3); defaults to 'MethodRoleRpc'
  , methodLongDescription :: Maybe Text             -- ^ AUTH-3: the doc comment, beside the short description
  , methodGrant           :: Maybe Text             -- ^ AUTH-3: @<activation>.<method>@, e.g. @tickets.set_status@
  , methodTarget          :: Maybe Text             -- ^ AUTH-3: the param that locates what a call acts on
  , methodHeldOn          :: Maybe [Text]           -- ^ AUTH-4: where the caller holds the grant (its view only)
  , methodPublic          :: Bool                   -- ^ AUTH-3: callable anonymously (the auth activation's door)
  , methodGroup           :: Maybe Text             -- ^ SYNAPSE-13: the method's group in a listing (OpenAPI's tag)
  , methodExamples        :: [MethodExample]        -- ^ SYNAPSE-13: worked calls, each its params and a note
  , methodInput           :: Maybe Value            -- ^ SYNAPSE-CC-8 / BIDIR-6: the JSON Schema of what the client sends into a call after it starts (@input@); absent on every server until BIDIR-6
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodSchema where
  parseJSON = withObject "MethodSchema" $ \o -> MethodSchema
    <$> o .: "name"
    -- AUTH-4: an anonymous view may name a method and no more
    <*> o .:? "description" .!= ""
    <*> o .:? "hash" .!= ""
    <*> o .:? "params"
    <*> returnsOf o
    <*> o .:? "error"
    <*> o .:? "progress"
    <*> cardinalityOf o
    -- ASK-1: bidir is gone; an older schema's @bidirectional@,
    -- @request_type@ and @response_type@ are read past
    <*> o .:? "http_method"    .!= "POST"
    <*> o .:? "deprecation"
    <*> o .:? "param_schemas"
    <*> o .:? "role" .!= MethodRoleRpc
    <*> o .:? "long_description"
    <*> o .:? "grant"
    <*> o .:? "target"
    <*> o .:? "held_on"
    <*> o .:? "public" .!= False
    -- SYNAPSE-13: absent on an older service; an unreadable one is ignored
    <*> (o .:? "group" <|> pure Nothing)
    <*> (o .:? "examples" .!= [] <|> pure [])
    <*> o .:? "input"

-- | RETURNS-36: a method's cardinality — its @cardinality@ (plexus 0.8);
--   else, a method of the published 0.7.0 line, its own @streaming@ flag
--   (@true@ many, @false@ one), as plexus-core's child seam reads it; a
--   method that states neither is refused.
cardinalityOf :: Object -> Parser Cardinality
cardinalityOf o = case KM.lookup "cardinality" o of
  Just c -> parseJSON c
  Nothing -> case KM.lookup "streaming" o of
    Just (Bool True) -> pure CardinalityMany
    Just (Bool False) -> pure CardinalityOne
    _ -> fail "a method states neither cardinality (plexus 0.8) nor streaming (0.7.0)"

-- | RETURNS-36: a method's @returns@ — required of a 0.8 method (one that
--   states its cardinality); a 0.7.0 method's missing one is the empty
--   schema, @{}@.
returnsOf :: Object -> Parser Value
returnsOf o
  | KM.member "cardinality" o = o .: "returns"
  | otherwise = o .:? "returns" .!= Object KM.empty

-- | Encodes as the Rust @MethodSchema@ serializes: an absent optional field
--   is omitted, not @null@.
instance ToJSON MethodSchema where
  toJSON MethodSchema{..} = object $
    [ "name"           .= methodName
    , "description"    .= methodDescription
    , "hash"           .= methodHash
    , "returns"        .= methodReturns
    , "cardinality"    .= methodCardinality
    , "http_method"    .= methodHttpMethod
    , "role"           .= methodRole
    ] <> catMaybes
    [ "params"         .=? methodParams
    , "error"          .=? methodError
    , "progress"       .=? methodProgress
    , "deprecation"    .=? methodDeprecation
    , "param_schemas"  .=? methodParamSchemas
    , "long_description" .=? methodLongDescription
    , "grant"            .=? methodGrant
    , "target"           .=? methodTarget
    , "held_on"          .=? methodHeldOn
    , if methodPublic then Just ("public" .= True) else Nothing
    , "group"            .=? methodGroup
    , if null methodExamples then Nothing else Just ("examples" .= methodExamples)
    , "input"            .=? methodInput
    ]

-- | SYNAPSE-13: one worked call of a method — the params it sends and a
-- note saying what it does. A client renders it as a command line.
data MethodExample = MethodExample
  { exampleParams :: Value       -- ^ the params object, as the method takes it
  , exampleNote   :: Maybe Text  -- ^ what the call does, in a line
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodExample where
  parseJSON = withObject "MethodExample" $ \o -> MethodExample
    <$> o .:? "params" .!= Object mempty
    <*> o .:? "note"

instance ToJSON MethodExample where
  toJSON MethodExample{..} = object $
    ("params" .= exampleParams) : maybe [] (\n -> ["note" .= n]) exampleNote

-- | SYNAPSE-13: the JSON Schema extension key on a param's property that
-- lists the values it is meant to take — a hint, never enforced.
xPlexusValues :: Text
xPlexusValues = "x-plexus-values"

-- | SYNAPSE-13: the values a param's property says it is meant to take
-- (@x-plexus-values@), when it says them.
paramValues :: Value -> Maybe [Text]
paramValues (Object po) = case KM.lookup (K.fromText xPlexusValues) po of
  Just (Array vs) -> Just [t | String t <- foldr (:) [] vs]
  _ -> Nothing
paramValues _ = Nothing

-- | Shallow plugin schema (what we receive from {backend}.schema)
--
-- Children are summaries only - fetch full schema on-demand when navigating.
-- This is the wire format: one layer of observation at a time.
data PluginSchema = PluginSchema
  { psNamespace       :: Text
  , psVersion         :: Text
  , psDescription     :: Text
  , psLongDescription :: Maybe Text  -- ^ Extended description (no word limit)
  , psSelfHash        :: Maybe PluginHash  -- ^ Hash of this plugin's own methods, children aside (@self_hash@)
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]  -- ^ Nothing = leaf, Just = parent activation
  , psDeprecation     :: Maybe DeprecationInfo -- ^ Activation-level deprecation (IR-5)
  , psRequest         :: Maybe Value  -- ^ JSON Schema of the activation's request type (@request@): each
                                      --   property carries @x-plexus-source@ (cookie, header, query, peer);
                                      --   Nothing when the activation declares none (CONTEXT-5)
  , psRoles           :: Maybe (Map Text [Text]) -- ^ AUTH-3: the activation's default roles, each its grants
  , psView            :: Maybe View            -- ^ AUTH-4: which view this schema answers from
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PluginSchema where
  parseJSON = withObject "PluginSchema" $ \o -> PluginSchema
    <$> o .: "namespace"
    -- AUTH-4: an anonymous view's schema may omit what it does not show
    <*> o .:? "version" .!= ""
    <*> o .:? "description" .!= ""
    <*> o .:? "long_description"
    <*> o .:? "self_hash"
    <*> o .:? "hash" .!= ""
    <*> o .:? "methods" .!= []
    <*> o .:? "children"
    <*> o .:? "deprecation"
    <*> o .:? "request"
    <*> (o .:? "roles" <|> pure Nothing)
    <*> (o .:? "view" <|> pure Nothing)

-- | Encodes as the Rust @PluginSchema@ serializes: an absent optional field
--   is omitted, not @null@.
instance ToJSON PluginSchema where
  toJSON PluginSchema{..} = object $
    [ "namespace"        .= psNamespace
    , "version"          .= psVersion
    , "description"      .= psDescription
    , "hash"             .= psHash
    , "methods"          .= psMethods
    ] <> catMaybes
    [ "long_description" .=? psLongDescription
    , "self_hash"        .=? psSelfHash
    , "children"         .=? psChildren
    , "deprecation"      .=? psDeprecation
    , "request"          .=? psRequest
    , "roles"            .=? psRoles
    , "view"             .=? psView
    ]

-- | A key whose value is omitted when 'Nothing'.
(.=?) :: ToJSON v => Key -> Maybe v -> Maybe Pair
k .=? v = (k .=) <$> v
infixr 8 .=?

-- | Result of a schema query - can be either a full plugin or just a method
data SchemaResult
  = SchemaPlugin PluginSchema
  | SchemaMethod MethodSchema
  deriving stock (Show, Eq)

instance FromJSON SchemaResult where
  parseJSON v =
    -- Try PluginSchema first (has "namespace" field)
    (SchemaPlugin <$> parseJSON v) <|>
    -- Fall back to MethodSchema (has "name" field)
    (SchemaMethod <$> parseJSON v)

instance ToJSON SchemaResult where
  toJSON (SchemaPlugin p) = toJSON p
  toJSON (SchemaMethod m) = toJSON m

-- ============================================================================
-- Basic Queries
-- ============================================================================

-- | AUTH-4: does any method here say its grant or where it is held? Then
-- help shows those columns; otherwise it renders as before AUTH.
hasAuthColumns :: PluginSchema -> Bool
hasAuthColumns ps = any (\m -> maybe False (const True) (methodGrant m) || maybe False (const True) (methodHeldOn m)) (psMethods ps)

-- | Is this a parent activation (has children)?
isParentActivation :: PluginSchema -> Bool
isParentActivation = maybe False (not . null) . psChildren

-- | Is this a leaf activation (no children)?
isLeafActivation :: PluginSchema -> Bool
isLeafActivation = not . isParentActivation

-- | The old name of 'isParentActivation', kept for one release. -- naming-alias: removed by NAMING-18
{-# DEPRECATED isHubActivation "renamed isParentActivation — NAMING" #-} -- naming-alias
isHubActivation :: PluginSchema -> Bool -- naming-alias
isHubActivation = isParentActivation -- naming-alias

-- | Get methods (alias for psMethods)
pluginMethods :: PluginSchema -> [MethodSchema]
pluginMethods = psMethods

-- | Get child summaries (empty list if leaf)
pluginChildren :: PluginSchema -> [ChildSummary]
pluginChildren = fromMaybe [] . psChildren

-- | Get child namespace names
childNamespaces :: PluginSchema -> [Text]
childNamespaces = map csNamespace . pluginChildren

-- ============================================================================
-- JSON Parsing Helpers
-- ============================================================================

-- | Parse a PluginSchema from the schema event content
parsePluginSchema :: Value -> Either Text PluginSchema
parsePluginSchema val = case fromJSON val of
  Success schema -> Right schema
  Error err -> Left $ T.pack err

-- | Parse a SchemaResult (plugin or method) from schema event content
parseSchemaResult :: Value -> Either Text SchemaResult
parseSchemaResult val = case fromJSON val of
  Success result -> Right result
  Error err -> Left $ T.pack err
