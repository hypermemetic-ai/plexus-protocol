-- | Schema types for dynamic CLI discovery
--
-- These types mirror the Substrate server's PlexusSchema and ActivationInfo,
-- allowing the CLI to discover available commands at runtime.
module Plexus.Schema
  ( -- * Schema Types
    PlexusSchema(..)
  , ActivationInfo(..)
  , PlexusSchemaEvent(..)
    -- * Enriched Schema Types (deprecated - use Full Schema)
  , EnrichedSchema(..)
  , SchemaProperty(..)
  , ActivationSchemaEvent(..)
    -- * Full Schema Types (from plexus_full_schema)
  , ActivationFullSchema(..)
  , MethodSchemaInfo(..)
  , FullSchemaEvent(..)
    -- * Hash Types
  , PlexusHash(..)
  , PlexusHashEvent(..)
    -- * Method Schema (parsed from enriched)
  , MethodSchema(..)
  , ParamSchema(..)
  , ParamType(..)
    -- * Parsing
  , parseMethodSchemas
  , parseMethodVariantByIndex
  , parseParamType
    -- * Stream Helpers
  , extractSchemaEvent
  , extractActivationSchemaEvent
  , extractFullSchemaEvent
  , extractHashEvent
  , extractRecursiveSchema
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import Data.Aeson.Types (Parser)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Plexus.RemoteError (renderRemoteError)
import Plexus.Types (PlexusStreamItem(..))
import qualified Plexus.Schema.Recursive as RecursiveSchema

-- ============================================================================
-- Core Schema Types (from {backend}.schema subscription)
-- ============================================================================

-- | Information about a single activation
data ActivationInfo = ActivationInfo
  { activationNamespace   :: Text   -- ^ e.g., "arbor", "cone", "health"
  , activationVersion     :: Text   -- ^ Semantic version e.g., "1.0.0"
  , activationDescription :: Text   -- ^ Human-readable description
  , activationMethods     :: [Text] -- ^ Method names e.g., ["tree_create", "tree_get"]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ActivationInfo where
  parseJSON = withObject "ActivationInfo" $ \o ->
    ActivationInfo
      <$> o .: "namespace"
      <*> o .: "version"
      <*> o .: "description"
      <*> o .: "methods"

instance ToJSON ActivationInfo where
  toJSON ActivationInfo{..} = object
    [ "namespace"   .= activationNamespace
    , "version"     .= activationVersion
    , "description" .= activationDescription
    , "methods"     .= activationMethods
    ]

-- | Top-level schema returned by {backend}.schema subscription
data PlexusSchema = PlexusSchema
  { schemaActivations  :: [ActivationInfo]
  , schemaTotalMethods :: Int
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusSchema where
  parseJSON = withObject "PlexusSchema" $ \o ->
    PlexusSchema
      <$> o .: "activations"
      <*> o .: "total_methods"

instance ToJSON PlexusSchema where
  toJSON PlexusSchema{..} = object
    [ "activations"   .= schemaActivations
    , "total_methods" .= schemaTotalMethods
    ]

-- | Event wrapper for plexus schema stream
data PlexusSchemaEvent
  = SchemaData PlexusSchema
  | SchemaError Text
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusSchemaEvent where
  parseJSON = withObject "PlexusSchemaEvent" $ \o -> do
    mTyp <- o .:? "type"
    case mTyp :: Maybe Text of
      Just "error" -> SchemaError <$> o .: "message"
      _ -> do
        -- Schema data comes directly without type wrapper
        mActivations <- o .:? "activations" :: Parser (Maybe Value)
        case mActivations of
          Just _ -> SchemaData <$> parseJSON (Object o)
          Nothing -> fail "PlexusSchemaEvent: expected 'activations' field"

-- ============================================================================
-- Enriched Schema Types (from plexus_activation_schema)
-- ============================================================================

-- ============================================================================
-- $ref Resolution Helpers
-- ============================================================================

-- | Resolve $ref references in a schema using $defs
resolveSchemaRefs :: EnrichedSchema -> EnrichedSchema
resolveSchemaRefs schema = case schemaRef schema of
  Just ref | Just defs <- schemaDefs schema ->
    -- Resolve the reference
    case lookupRef ref defs of
      Just resolved ->
        -- Merge resolved schema with local description
        let merged = mergeSchema schema resolved
        -- Continue resolving recursively
        in resolveSchemaRefs merged { schemaRef = Nothing, schemaDefs = schemaDefs schema }
      Nothing -> schema  -- Reference not found, keep as-is
  _ ->
    -- No ref to resolve, but recursively resolve properties and oneOf
    -- Propagate $defs to children so they can resolve their refs
    let defs = schemaDefs schema
    in schema
      { schemaProperties = resolvePropertiesRefs defs <$> schemaProperties schema
      , schemaOneOf = map (propagateDefsAndResolve defs) <$> schemaOneOf schema
      }
  where
    -- Propagate parent $defs to child schema and resolve
    propagateDefsAndResolve :: Maybe (Map Text Value) -> EnrichedSchema -> EnrichedSchema
    propagateDefsAndResolve parentDefs child =
      let childWithDefs = case (parentDefs, schemaDefs child) of
            (Just pDefs, Nothing) -> child { schemaDefs = Just pDefs }
            _ -> child  -- Child already has defs or parent has none
      in resolveSchemaRefs childWithDefs

-- | Look up a $ref path in definitions
-- e.g., "#/$defs/ConeIdentifier" -> look up "ConeIdentifier" in defs
lookupRef :: Text -> Map Text Value -> Maybe EnrichedSchema
lookupRef ref defs
  | T.isPrefixOf "#/$defs/" ref =
      let defName = T.drop 8 ref  -- Drop "#/$defs/"
      in case Map.lookup defName defs of
        Just val -> case fromJSON val of
          Success schema -> Just schema
          Error _ -> Nothing
        Nothing -> Nothing
  | otherwise = Nothing

-- | Merge a schema with a resolved reference, preserving local description
mergeSchema :: EnrichedSchema -> EnrichedSchema -> EnrichedSchema
mergeSchema local resolved = resolved
  { schemaDescription = schemaDescription local <|> schemaDescription resolved
  }

-- | Resolve $refs in schema properties
resolvePropertiesRefs :: Maybe (Map Text Value) -> Map Text SchemaProperty -> Map Text SchemaProperty
resolvePropertiesRefs defs props = Map.map (resolvePropertyRefs defs) props

-- | Resolve $ref in a single property
resolvePropertyRefs :: Maybe (Map Text Value) -> SchemaProperty -> SchemaProperty
resolvePropertyRefs maybeDefs prop = case (propRef prop, maybeDefs) of
  (Just ref, Just defs) | T.isPrefixOf "#/$defs/" ref ->
    let defName = T.drop 8 ref
    in case Map.lookup defName defs of
      Just val -> case fromJSON val of
        Success resolved ->
          -- Merge resolved with local description, then recursively resolve the result
          let merged = resolved { propDescription = propDescription prop <|> propDescription resolved }
          in resolvePropertyRefs maybeDefs merged { propRef = Nothing }
        Error _ -> prop  -- Parse error, keep original
      Nothing -> prop  -- Ref not found, keep original
  _ ->
    -- No ref to resolve, but recursively resolve nested properties
    prop
      { propProperties = resolvePropertiesRefs maybeDefs <$> propProperties prop
      , propOneOf = map (resolvePropertyRefs maybeDefs) <$> propOneOf prop
      , propItems = resolvePropertyRefs maybeDefs <$> propItems prop
      }

-- ============================================================================
-- Enriched Schema Types (from plexus_activation_schema)
-- ============================================================================

-- | Enriched JSON Schema from Substrate server
-- Mirrors the Rust Schema type from plexus/schema.rs
data EnrichedSchema = EnrichedSchema
  { schemaTitle       :: Maybe Text
  , schemaDescription :: Maybe Text
  , schemaType        :: Maybe Value         -- ^ "object", "string", etc.
  , schemaProperties  :: Maybe (Map Text SchemaProperty)
  , schemaRequired    :: Maybe [Text]
  , schemaOneOf       :: Maybe [EnrichedSchema]  -- ^ Method variants
  , schemaDefs        :: Maybe (Map Text Value)  -- ^ Schema definitions for $ref resolution
  , schemaRef         :: Maybe Text              -- ^ Reference to definition (e.g., "#/$defs/ConeIdentifier")
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON EnrichedSchema where
  parseJSON val@(Object o) = do
    -- Parse base schema
    base <- EnrichedSchema
      <$> o .:? "title"
      <*> o .:? "description"
      <*> o .:? "type"
      <*> o .:? "properties"
      <*> o .:? "required"
      <*> o .:? "oneOf"
      <*> o .:? "$defs"
      <*> o .:? "$ref"
    -- Apply $ref resolution if needed
    pure $ resolveSchemaRefs base
  parseJSON _ = fail "EnrichedSchema must be an object"

instance ToJSON EnrichedSchema where
  toJSON EnrichedSchema{..} = object $ catMaybes
    [ ("title" .=) <$> schemaTitle
    , ("description" .=) <$> schemaDescription
    , ("type" .=) <$> schemaType
    , ("properties" .=) <$> schemaProperties
    , ("required" .=) <$> schemaRequired
    , ("oneOf" .=) <$> schemaOneOf
    , ("$defs" .=) <$> schemaDefs
    , ("$ref" .=) <$> schemaRef
    ]

-- | Schema property definition
data SchemaProperty = SchemaProperty
  { propType        :: Maybe Value       -- ^ "string", "integer", ["string", "null"]
  , propDescription :: Maybe Text
  , propFormat      :: Maybe Text        -- ^ "uuid", "date-time", etc.
  , propItems       :: Maybe SchemaProperty  -- ^ For array types
  , propDefault     :: Maybe Value
  , propEnum        :: Maybe [Value]
  , propProperties  :: Maybe (Map Text SchemaProperty)  -- ^ Nested object
  , propRequired    :: Maybe [Text]      -- ^ Required fields for object types
  , propOneOf       :: Maybe [SchemaProperty]  -- ^ OneOf variants (for enums)
  , propRef         :: Maybe Text        -- ^ Reference to definition
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON SchemaProperty where
  parseJSON = withObject "SchemaProperty" $ \o ->
    SchemaProperty
      <$> o .:? "type"
      <*> o .:? "description"
      <*> o .:? "format"
      <*> o .:? "items"
      <*> o .:? "default"
      <*> o .:? "enum"
      <*> o .:? "properties"
      <*> o .:? "required"
      <*> o .:? "oneOf"
      <*> o .:? "$ref"

instance ToJSON SchemaProperty where
  toJSON SchemaProperty{..} = object $ catMaybes
    [ ("type" .=) <$> propType
    , ("description" .=) <$> propDescription
    , ("format" .=) <$> propFormat
    , ("items" .=) <$> propItems
    , ("default" .=) <$> propDefault
    , ("enum" .=) <$> propEnum
    , ("properties" .=) <$> propProperties
    , ("required" .=) <$> propRequired
    , ("oneOf" .=) <$> propOneOf
    , ("$ref" .=) <$> propRef
    ]

-- | Event wrapper for activation schema stream
data ActivationSchemaEvent
  = ActivationSchemaData EnrichedSchema
  | ActivationSchemaError Text
  deriving stock (Show, Eq, Generic)

instance FromJSON ActivationSchemaEvent where
  parseJSON val = case val of
    Object o -> do
      mErr <- o .:? "error"
      case mErr of
        Just err -> pure $ ActivationSchemaError err
        Nothing  -> ActivationSchemaData <$> parseJSON val
    _ -> fail "ActivationSchemaEvent: expected object"

-- ============================================================================
-- Full Schema Types (from plexus_full_schema)
-- ============================================================================

-- | Schema info for a single method
data MethodSchemaInfo = MethodSchemaInfo
  { methodInfoName        :: Text
  , methodInfoDescription :: Text
  , methodInfoParams      :: Maybe Value  -- ^ JSON Schema for params
  , methodInfoReturns     :: Maybe Value  -- ^ JSON Schema for return/stream events
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON MethodSchemaInfo where
  parseJSON = withObject "MethodSchemaInfo" $ \o ->
    MethodSchemaInfo
      <$> o .: "name"
      <*> o .: "description"
      <*> o .:? "params"
      <*> o .:? "returns"

instance ToJSON MethodSchemaInfo where
  toJSON MethodSchemaInfo{..} = object $ catMaybes
    [ Just ("name" .= methodInfoName)
    , Just ("description" .= methodInfoDescription)
    , ("params" .=) <$> methodInfoParams
    , ("returns" .=) <$> methodInfoReturns
    ]

-- | Full schema for an activation
data ActivationFullSchema = ActivationFullSchema
  { fullSchemaNamespace   :: Text
  , fullSchemaVersion     :: Text
  , fullSchemaDescription :: Text
  , fullSchemaMethods     :: [MethodSchemaInfo]
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON ActivationFullSchema where
  parseJSON = withObject "ActivationFullSchema" $ \o ->
    ActivationFullSchema
      <$> o .: "namespace"
      <*> o .: "version"
      <*> o .: "description"
      <*> o .: "methods"

instance ToJSON ActivationFullSchema where
  toJSON ActivationFullSchema{..} = object
    [ "namespace"   .= fullSchemaNamespace
    , "version"     .= fullSchemaVersion
    , "description" .= fullSchemaDescription
    , "methods"     .= fullSchemaMethods
    ]

-- | Event wrapper for full schema stream
data FullSchemaEvent
  = FullSchemaData ActivationFullSchema
  | FullSchemaError Text
  deriving stock (Show, Eq, Generic)

instance FromJSON FullSchemaEvent where
  parseJSON val = case val of
    Object o -> do
      mErr <- o .:? "error"
      case mErr of
        Just err -> pure $ FullSchemaError err
        Nothing  -> FullSchemaData <$> parseJSON val
    _ -> fail "FullSchemaEvent: expected object"

-- ============================================================================
-- Hash Types (from plexus_hash)
-- ============================================================================

-- | Plexus hash for cache invalidation
data PlexusHash = PlexusHash
  { plexusHash :: Text  -- ^ Hash of all activations (namespace:version:methods)
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusHash where
  parseJSON = withObject "PlexusHash" $ \o ->
    PlexusHash <$> o .: "hash"

instance ToJSON PlexusHash where
  toJSON PlexusHash{..} = object
    [ "hash" .= plexusHash ]

-- | Event wrapper for plexus hash stream
data PlexusHashEvent
  = HashData PlexusHash
  | HashError Text
  deriving stock (Show, Eq, Generic)

instance FromJSON PlexusHashEvent where
  parseJSON val = case val of
    Object o -> do
      mErr <- o .:? "error"
      case mErr of
        Just err -> pure $ HashError err
        Nothing  -> HashData <$> parseJSON val
    _ -> fail "PlexusHashEvent: expected object"

-- ============================================================================
-- Method Schema Types (parsed from EnrichedSchema)
-- ============================================================================

-- | Parameter type enumeration
data ParamType
  = ParamString
  | ParamInteger
  | ParamNumber
  | ParamBoolean
  | ParamObject
  | ParamArray ParamType
  | ParamNullable ParamType
  deriving stock (Show, Eq, Generic)

-- | Schema for a single parameter
data ParamSchema = ParamSchema
  { paramName     :: Text
  , paramType     :: ParamType
  , paramFormat   :: Maybe Text   -- ^ e.g., "uuid"
  , paramRequired :: Bool
  , paramDesc     :: Maybe Text
  , paramDefault  :: Maybe Value
  }
  deriving stock (Show, Eq, Generic)

-- | Schema for a method with full param details
data MethodSchema = MethodSchema
  { methodName        :: Text
  , methodDescription :: Maybe Text
  , methodParams      :: [ParamSchema]
  }
  deriving stock (Show, Eq, Generic)

-- | Parse ParamType from JSON Schema type field
parseParamType :: Value -> ParamType
parseParamType (String "string")  = ParamString
parseParamType (String "integer") = ParamInteger
parseParamType (String "number")  = ParamNumber
parseParamType (String "boolean") = ParamBoolean
parseParamType (String "object")  = ParamObject
parseParamType (Array types)      =
  -- Handle nullable types like ["string", "null"]
  let typeList = foldr (:) [] types
  in case filter (/= String "null") typeList of
    [t] -> ParamNullable (parseParamType t)
    _   -> ParamObject  -- fallback
parseParamType _                  = ParamObject  -- fallback

-- | Parse an EnrichedSchema into a list of MethodSchemas
-- The enriched schema has oneOf containing method variants
parseMethodSchemas :: EnrichedSchema -> [MethodSchema]
parseMethodSchemas schema = case schemaOneOf schema of
  Just variants -> mapMaybe parseMethodVariant variants
  Nothing -> []

-- | Parse a single method variant from the oneOf array
parseMethodVariant :: EnrichedSchema -> Maybe MethodSchema
parseMethodVariant variant = do
  props <- schemaProperties variant
  -- Get method name from "method" property's const/enum (may not exist)
  let methodProp = Map.lookup "method" props
      mMethodName = methodProp >>= extractMethodName
  -- Get params from "params" property
  let params = case Map.lookup "params" props of
        Just paramsProp -> extractParams paramsProp (propRequired paramsProp)
        Nothing -> []
  -- Return schema even without method name (we'll use index-based lookup)
  pure MethodSchema
    { methodName = fromMaybe "" mMethodName
    , methodDescription = schemaDescription variant
    , methodParams = params
    }

-- | Parse a method variant by index (for when method names aren't in schema)
parseMethodVariantByIndex :: EnrichedSchema -> Int -> Maybe MethodSchema
parseMethodVariantByIndex schema idx = case schemaOneOf schema of
  Just variants | idx < length variants -> parseMethodVariant (variants !! idx)
  _ -> Nothing

-- | Extract method name from the method property
extractMethodName :: SchemaProperty -> Maybe Text
extractMethodName prop = case propEnum prop of
  Just (String name : _) -> Just name
  _ -> Nothing

-- | Extract parameters from the params property
extractParams :: SchemaProperty -> Maybe [Text] -> [ParamSchema]
extractParams paramsProp mRequired =
  case propProperties paramsProp of
    Just props -> map (toParamSchema required) (Map.toList props)
    Nothing -> []
  where
    required = fromMaybe [] mRequired

-- | Convert a property to ParamSchema
toParamSchema :: [Text] -> (Text, SchemaProperty) -> ParamSchema
toParamSchema required (name, prop) = ParamSchema
  { paramName = name
  , paramType = maybe ParamObject parseParamType (propType prop)
  , paramFormat = propFormat prop
  , paramRequired = name `elem` required
  , paramDesc = propDescription prop
  , paramDefault = propDefault prop
  }

-- ============================================================================
-- Stream Helpers
-- ============================================================================

-- | Extract PlexusSchemaEvent from a stream item
-- The {backend}.schema subscription uses content_type "plexus.schema"
extractSchemaEvent :: PlexusStreamItem -> Maybe PlexusSchemaEvent
extractSchemaEvent (StreamData _ _ contentType dat)
  | contentType == "plexus.schema" =
      case fromJSON dat of
        Success evt -> Just evt
        Error _     -> Nothing
  | otherwise = Nothing
extractSchemaEvent _ = Nothing

-- | Extract ActivationSchemaEvent from a stream item
-- The plexus_activation_schema subscription uses content_type "plexus.activation_schema"
extractActivationSchemaEvent :: PlexusStreamItem -> Maybe ActivationSchemaEvent
extractActivationSchemaEvent (StreamData _ _ contentType dat)
  | contentType == "plexus.activation_schema" =
      case fromJSON dat of
        Success schema -> Just (ActivationSchemaData schema)
        Error _        -> Nothing
  | otherwise = Nothing
extractActivationSchemaEvent (StreamError _ _ err _) = Just (ActivationSchemaError (renderRemoteError err))
extractActivationSchemaEvent _ = Nothing

-- | Extract FullSchemaEvent from a stream item
-- The plexus_full_schema subscription uses content_type "plexus.full_schema"
extractFullSchemaEvent :: PlexusStreamItem -> Maybe FullSchemaEvent
extractFullSchemaEvent (StreamData _ _ contentType dat)
  | contentType == "plexus.full_schema" =
      case fromJSON dat of
        Success schema -> Just (FullSchemaData schema)
        Error _        -> Nothing
  | otherwise = Nothing
extractFullSchemaEvent (StreamError _ _ err _) = Just (FullSchemaError (renderRemoteError err))
extractFullSchemaEvent _ = Nothing

-- | Extract PlexusHashEvent from a stream item
-- The plexus_hash subscription uses content_type "plexus.hash"
extractHashEvent :: PlexusStreamItem -> Maybe PlexusHashEvent
extractHashEvent (StreamData _ _ contentType dat)
  | contentType == "plexus.hash" =
      case fromJSON dat of
        Success hash -> Just (HashData hash)
        Error _      -> Nothing
  | otherwise = Nothing
extractHashEvent (StreamError _ _ err _) = Just (HashError (renderRemoteError err))
extractHashEvent _ = Nothing

-- | Extract recursive plugin schema from stream item
-- The {backend}.schema subscription uses content_type "plexus.schema"
-- and returns the full recursive schema structure
extractRecursiveSchema :: PlexusStreamItem -> Maybe (Either Text RecursiveSchema.PluginSchema)
extractRecursiveSchema (StreamData _ _ contentType dat)
  | contentType == "plexus.schema" = Just $ RecursiveSchema.parsePluginSchema dat
  | otherwise = Nothing
extractRecursiveSchema (StreamError _ _ err _) = Just (Left (renderRemoteError err))
extractRecursiveSchema _ = Nothing
