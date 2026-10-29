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

    -- * Deprecation Metadata (IR-5)
  , DeprecationInfo(..)
  , ParamSchema(..)
  , MethodRole(..)

    -- * The caller's view (AUTH-3 / AUTH-4)
  , View(..)
  , hasAuthColumns

    -- * Help metadata (SYNAPSE-13)
  , MethodExample(..)
  , paramValues
  , xPlexusValues

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
import Data.Aeson.Types (Parser)
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
    -- AUTH-4: an anonymous view's children may be bare
    <*> o .:? "description" .!= ""
    <*> o .:? "hash" .!= ""

instance ToJSON ChildSummary where
  toJSON ChildSummary{..} = object
    [ "namespace"   .= csNamespace
    , "description" .= csDescription
    , "hash"        .= csHash
    ]

-- | Schema for a single method
data MethodSchema = MethodSchema
  { methodName            :: Text
  , methodDescription     :: Text
  , methodHash            :: PluginHash
  , methodParams          :: Maybe Value  -- ^ JSON Schema for params
  , methodReturns         :: Maybe Value  -- ^ JSON Schema for return events
  , methodStreaming        :: Bool         -- ^ True if method streams multiple events
  , methodBidirectional   :: Bool         -- ^ True if method uses a bidirectional channel
  , methodRequestType     :: Maybe Value  -- ^ JSON Schema for the server→client request type (when bidirectional)
  , methodResponseType    :: Maybe Value  -- ^ JSON Schema for the client→server response type (when bidirectional)
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
    <*> o .:? "returns"
    <*> o .:? "streaming"      .!= False
    <*> o .:? "bidirectional"  .!= False
    <*> o .:? "request_type"
    <*> o .:? "response_type"
    <*> o .:? "deprecation"
    <*> o .:? "param_schemas"
    <*> o .:? "role" .!= MethodRoleRpc
    <*> o .:? "long_description"
    <*> o .:? "grant"
    <*> o .:? "target"
    <*> o .:? "held_on"
    <*> o .:? "public" .!= False
    -- SYNAPSE-13: absent on an older hub; an unreadable one is ignored
    <*> (o .:? "group" <|> pure Nothing)
    <*> (o .:? "examples" .!= [] <|> pure [])
    <*> o .:? "input"

instance ToJSON MethodSchema where
  toJSON MethodSchema{..} = object $
    [ "name"           .= methodName
    , "description"    .= methodDescription
    , "hash"           .= methodHash
    , "params"         .= methodParams
    , "returns"        .= methodReturns
    , "streaming"      .= methodStreaming
    , "bidirectional"  .= methodBidirectional
    , "request_type"   .= methodRequestType
    , "response_type"  .= methodResponseType
    , "deprecation"    .= methodDeprecation
    , "param_schemas"  .= methodParamSchemas
    , "role"           .= methodRole
    ] <> catMaybes
    [ ("long_description" .=) <$> methodLongDescription
    , ("grant"            .=) <$> methodGrant
    , ("target"           .=) <$> methodTarget
    , ("held_on"          .=) <$> methodHeldOn
    , if methodPublic then Just ("public" .= True) else Nothing
    , ("group"            .=) <$> methodGroup
    , if null methodExamples then Nothing else Just ("examples" .= methodExamples)
    , ("input"            .=) <$> methodInput
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
  , psHash            :: PluginHash
  , psMethods         :: [MethodSchema]
  , psChildren        :: Maybe [ChildSummary]  -- ^ Nothing = leaf, Just = hub activation
  , psDeprecation     :: Maybe DeprecationInfo -- ^ Activation-level deprecation (IR-5)
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
    <*> o .:? "hash" .!= ""
    <*> o .:? "methods" .!= []
    <*> o .:? "children"
    <*> o .:? "deprecation"
    <*> (o .:? "roles" <|> pure Nothing)
    <*> (o .:? "view" <|> pure Nothing)

instance ToJSON PluginSchema where
  toJSON PluginSchema{..} = object $
    [ "namespace"        .= psNamespace
    , "version"          .= psVersion
    , "description"      .= psDescription
    , "long_description" .= psLongDescription
    , "hash"             .= psHash
    , "methods"          .= psMethods
    , "children"         .= psChildren
    , "deprecation"      .= psDeprecation
    ] <> catMaybes
    [ ("roles" .=) <$> psRoles
    , ("view"  .=) <$> psView
    ]

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
