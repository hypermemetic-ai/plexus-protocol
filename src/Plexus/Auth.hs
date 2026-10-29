-- | AUTH-4 / AUTH-2 / AUTH-5: what a client parses of a service's auth.
--
-- Every field is optional on the wire: a service without a gate (every service
-- before AUTH, and any service that leaves login off) answers none of it, and
-- everything here parses its absence as 'Nothing' or empty.
--
-- * '_info' gains @view@ (@anonymous@ | @caller@), the view's @hash@, and,
--   to an anonymous caller, an @auth@ block naming the methods it may call
--   and the RFC 9728 resource metadata URL ('BackendInfo').
-- * A refusal is an error item with @error_kind: "forbidden"@ naming the
--   grant it needed, the scope it was asked on, the roles that list the
--   grant and where the caller holds it ('Refusal').
module Plexus.Auth
  ( -- * The view (AUTH-4)
    View(..)
  , viewText
  , BackendInfo(..)
  , AuthInfo(..)
  , parseBackendInfo

    -- * Refusals (AUTH-2)
  , Refusal(..)
  , parseRefusal
  , refusalFromObject
  ) where

import Control.Applicative ((<|>))
import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Parser, parseMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- ============================================================================
-- The view
-- ============================================================================

-- | Which part of a service the caller sees: an anonymous caller only how to
-- sign in; a signed-in caller what it can use.
data View = Anonymous | Caller
  deriving stock (Show, Eq, Ord, Generic)

viewText :: View -> Text
viewText Anonymous = "anonymous"
viewText Caller = "caller"

-- | @"anonymous"@ / @"caller"@, or an object naming it as @kind@ (a
-- tolerance: a service may carry the view's hash beside it).
instance FromJSON View where
  parseJSON = \case
    String s -> fromName s
    Object o -> (o .: "kind" <|> o .: "view") >>= fromName
    v -> fail ("View: expected a string, got " <> show v)
    where
      fromName s = case T.toLower s of
        "anonymous" -> pure Anonymous
        "caller" -> pure Caller
        other -> fail ("View: unknown view " <> T.unpack other)

instance ToJSON View where
  toJSON = String . viewText

-- | The anonymous view's door: the methods it may call and where an HTTP
-- client reads how to get a token.
data AuthInfo = AuthInfo
  { authMethods          :: [Text]      -- ^ e.g. @["auth.providers","auth.login","auth.signup"]@
  , authResourceMetadata :: Maybe Text  -- ^ RFC 9728 @/.well-known/oauth-protected-resource@
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON AuthInfo where
  parseJSON = withObject "AuthInfo" $ \o -> AuthInfo
    <$> o .:? "methods" .!= []
    <*> o .:? "resource_metadata"

instance ToJSON AuthInfo where
  toJSON AuthInfo{..} = object $
    ["methods" .= authMethods]
    <> maybe [] (\u -> ["resource_metadata" .= u]) authResourceMetadata

-- | What @_info@ answers. Only @backend@ is required.
data BackendInfo = BackendInfo
  { infoBackend  :: Text
  , infoView     :: Maybe View   -- ^ Nothing: a service with no gate (today's)
  , infoViewHash :: Maybe Text   -- ^ the view's own hash (@view_hash@, else @hash@, else the view object's @hash@)
  , infoAuth     :: Maybe AuthInfo
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON BackendInfo where
  parseJSON = withObject "BackendInfo" $ \o -> do
    backend <- o .: "backend"
    -- a view that does not parse is no view, never a failed _info
    view <- optionalLenient (o .:? "view")
    viewObjHash <- case KM.lookup "view" o of
      Just (Object vo) -> optionalLenient (vo .:? "hash")
      _ -> pure Nothing
    vhash <- optionalLenient (o .:? "view_hash")
    hash <- optionalLenient (o .:? "hash")
    auth <- optionalLenient (o .:? "auth")
    pure BackendInfo
      { infoBackend = backend
      , infoView = view
      , infoViewHash = vhash <|> viewObjHash <|> hash
      , infoAuth = auth
      }

instance ToJSON BackendInfo where
  toJSON BackendInfo{..} = object $
    ["backend" .= infoBackend]
    <> maybe [] (\v -> ["view" .= v]) infoView
    <> maybe [] (\h -> ["view_hash" .= h]) infoViewHash
    <> maybe [] (\a -> ["auth" .= a]) infoAuth

-- | Parse @_info@'s content.
parseBackendInfo :: Value -> Maybe BackendInfo
parseBackendInfo = parseMaybe parseJSON

-- | A field that may be present but malformed: Nothing rather than failing.
optionalLenient :: Parser (Maybe a) -> Parser (Maybe a)
optionalLenient p = p <|> pure Nothing

-- ============================================================================
-- Refusals
-- ============================================================================

-- | A typed refusal: @error_kind: "forbidden"@. Every field but the
-- message is optional; lists absent are empty.
data Refusal = Refusal
  { refusalGrant   :: Maybe Text   -- ^ @tickets.set_status@
  , refusalScope   :: Maybe Text   -- ^ where it was asked: @IDENTITY@
  , refusalRoles   :: [Text]       -- ^ the roles that list the grant
  , refusalHeldOn  :: [Text]       -- ^ where the caller holds it
  , refusalCeiling :: [Text]       -- ^ the session's ceiling, when the service says it
  , refusalMessage :: Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON Refusal where
  parseJSON = withObject "Refusal" $ \o -> do
    kind <- o .:? "error_kind"
    case kind :: Maybe Text of
      Just k | T.toLower k == "forbidden" -> refusalFields o
      _ -> fail "Refusal: error_kind is not forbidden"

instance ToJSON Refusal where
  toJSON Refusal{..} = object $
    [ "error_kind" .= ("forbidden" :: Text)
    , "message" .= refusalMessage
    , "roles" .= refusalRoles
    , "held_on" .= refusalHeldOn
    ]
    <> maybe [] (\g -> ["grant" .= g]) refusalGrant
    <> maybe [] (\s -> ["scope" .= s]) refusalScope
    <> (if null refusalCeiling then [] else ["ceiling" .= refusalCeiling])

refusalFields :: Object -> Parser Refusal
refusalFields o = do
  grant <- optionalLenient (o .:? "grant")
  scope <- optionalLenient (o .:? "scope")
  roles <- textList o "roles"
  held <- textList o "held_on"
  ceiling <- textList o "ceiling"
  msg <- optionalLenient (o .:? "message")
  pure Refusal
    { refusalGrant = grant
    , refusalScope = scope
    , refusalRoles = roles
    , refusalHeldOn = held
    , refusalCeiling = ceiling
    , refusalMessage = maybe (defaultMessage grant scope) id msg
    }
  where
    defaultMessage g s = case (g, s) of
      (Just g', Just s') -> "needs " <> g' <> " on " <> s'
      (Just g', Nothing) -> "needs " <> g'
      _ -> "forbidden"

-- | A list of strings, a single string, or a list whose elements are
-- objects naming a @scope@ / @role@ / @name@ — anything else is empty.
textList :: Object -> Key -> Parser [Text]
textList o k = pure $ case KM.lookup k o of
  Just (Array xs) -> concatMap one (foldr (:) [] xs)
  Just (String s) -> [s]
  _ -> []
  where
    one = \case
      String s -> [s]
      Object e
        | Just (String s) <- KM.lookup "scope" e -> [s]
        | Just (String s) <- KM.lookup "role" e -> [s]
        | Just (String s) <- KM.lookup "name" e -> [s]
      _ -> []

-- | A refusal from an error item's (or an in-band error event's) fields.
refusalFromObject :: Object -> Maybe Refusal
refusalFromObject = parseMaybe parseJSON . Object

-- | A refusal from any JSON value: an object with @error_kind: forbidden@.
parseRefusal :: Value -> Maybe Refusal
parseRefusal = parseMaybe parseJSON
