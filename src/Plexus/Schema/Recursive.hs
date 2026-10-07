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

    -- * Queries
  , isHubActivation
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
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

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
--   {"kind": "dynamic_child", "list_method": "planet_names", "search_method": null}
--   @
--
--   Defaults to 'MethodRoleRpc' when absent so pre-IR servers deserialize
--   cleanly (see 'MethodSchema.methodRole' parser).
data MethodRole
  = MethodRoleRpc
  | MethodRoleStaticChild
  | MethodRoleDynamicChild
      { listMethod   :: Maybe Text
      , searchMethod :: Maybe Text
      }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodRole where
  parseJSON = withObject "MethodRole" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "rpc"           -> pure MethodRoleRpc
      "static_child"  -> pure MethodRoleStaticChild
      "dynamic_child" -> MethodRoleDynamicChild
        <$> o .:? "list_method"
        <*> o .:? "search_method"
      other -> fail $ "Unknown MethodRole kind: " <> T.unpack other

instance ToJSON MethodRole where
  toJSON MethodRoleRpc         = object ["kind" .= ("rpc" :: Text)]
  toJSON MethodRoleStaticChild = object ["kind" .= ("static_child" :: Text)]
  toJSON (MethodRoleDynamicChild lm sm) =
    object $ ("kind" .= ("dynamic_child" :: Text)) : catMaybes
      [ ("list_method"   .=) <$> lm
      , ("search_method" .=) <$> sm
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
    <*> o .: "description"
    <*> o .: "hash"

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
  , methodBidirectional   :: Bool         -- ^ True if method uses a bidirectional channel
  , methodHttpMethod      :: Text         -- ^ HTTP method of the method's REST endpoint (@"GET"@, @"POST"@, …); @"POST"@ when absent
  , methodRequestType     :: Maybe Value  -- ^ JSON Schema for the server→client request type (when bidirectional)
  , methodResponseType    :: Maybe Value  -- ^ JSON Schema for the client→server response type (when bidirectional)
  , methodDeprecation     :: Maybe DeprecationInfo  -- ^ Deprecation info, if any (IR-5)
  , methodParamSchemas    :: Maybe [ParamSchema]    -- ^ Optional structured param info with per-field deprecations (IR-5)
  , methodRole            :: MethodRole             -- ^ Structural role in the activation graph (IR-2 / IR-3); defaults to 'MethodRoleRpc'
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodSchema where
  parseJSON = withObject "MethodSchema" $ \o -> MethodSchema
    <$> o .: "name"
    <*> o .: "description"
    <*> o .: "hash"
    <*> o .:? "params"
    <*> o .: "returns"
    <*> o .:? "error"
    <*> o .:? "progress"
    <*> o .: "cardinality"
    <*> o .:? "bidirectional"  .!= False
    <*> o .:? "http_method"    .!= "POST"
    <*> o .:? "request_type"
    <*> o .:? "response_type"
    <*> o .:? "deprecation"
    <*> o .:? "param_schemas"
    <*> o .:? "role" .!= MethodRoleRpc

-- | Encodes as the Rust @MethodSchema@ serializes: an absent optional field
--   is omitted, not @null@.
instance ToJSON MethodSchema where
  toJSON MethodSchema{..} = object $
    [ "name"           .= methodName
    , "description"    .= methodDescription
    , "hash"           .= methodHash
    , "returns"        .= methodReturns
    , "cardinality"    .= methodCardinality
    , "bidirectional"  .= methodBidirectional
    , "http_method"    .= methodHttpMethod
    , "role"           .= methodRole
    ] <> catMaybes
    [ "params"         .=? methodParams
    , "error"          .=? methodError
    , "progress"       .=? methodProgress
    , "request_type"   .=? methodRequestType
    , "response_type"  .=? methodResponseType
    , "deprecation"    .=? methodDeprecation
    , "param_schemas"  .=? methodParamSchemas
    ]

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
  , psChildren        :: Maybe [ChildSummary]  -- ^ Nothing = leaf, Just = hub activation
  , psDeprecation     :: Maybe DeprecationInfo -- ^ Activation-level deprecation (IR-5)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PluginSchema where
  parseJSON = withObject "PluginSchema" $ \o -> PluginSchema
    <$> o .: "namespace"
    <*> o .: "version"
    <*> o .: "description"
    <*> o .:? "long_description"
    <*> o .:? "self_hash"
    <*> o .: "hash"
    <*> o .:? "methods" .!= []
    <*> o .:? "children"
    <*> o .:? "deprecation"

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

-- | Is this a hub activation (has children)?
isHubActivation :: PluginSchema -> Bool
isHubActivation = maybe False (not . null) . psChildren

-- | Is this a leaf activation (no children)?
isLeafActivation :: PluginSchema -> Bool
isLeafActivation = not . isHubActivation

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
