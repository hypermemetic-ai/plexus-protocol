{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- | ASK-1: a method that needs its caller's yes, or a value only its
-- planning can name, finishes its call with a proposal — an ordinary data
-- item whose content is @{"type":"needs_input", …}@ — and is called again
-- with the same params, each field's value under its own name, and
-- @"confirm": token@. Two ordinary calls; nothing the server asks mid-call
-- (bidir, spec/08's @request@ item and @{ns}.respond@, is gone).
--
-- @
-- ← data  content_type "tickets.archive"
--         content {"type":"needs_input","summary":"archive MENGER-4 …","effects":[…],
--                  "fields":[{"name","options"?,"default"?,"help"?}],"token":"v1.…","expires":"2026-…Z"}
-- ← done
-- → tickets.archive { filter: "label stale", confirm: "v1.…" }
-- @
--
-- The token is opaque here: a client sends it back as it came. A client
-- only ever re-sends the call it made — never a method the server names.
module Plexus.NeedsInput
  ( NeedsInput(..)
  , NeedsInputField(..)
  , ConfirmToken
  , needsInputOf
  , needsInputOfContent
  , answerParams
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Aeson.Key as K
import Data.Text (Text)
import GHC.Generics (Generic)

import Plexus.Types (PlexusStreamItem(..))

-- | The standard type of a method's @confirm@ parameter: a string, opaque
-- to every client.
type ConfirmToken = Text

-- | One value the answer call carries: a parameter the method already
-- declares (its type is that parameter's, from the schema); the proposal
-- adds only the choices, a default and a line of help.
data NeedsInputField = NeedsInputField
  { nifName    :: Text
  , nifOptions :: Maybe [Value]
  , nifDefault :: Maybe Value
  , nifHelp    :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NeedsInputField where
  parseJSON = withObject "NeedsInputField" $ \o -> NeedsInputField
    <$> o .: "name"
    <*> o .:? "options"
    <*> o .:? "default"
    <*> o .:? "help"

instance ToJSON NeedsInputField where
  toJSON NeedsInputField{..} = object $
    [ "name" .= nifName ]
    ++ [ "options" .= v | Just v <- [nifOptions] ]
    ++ [ "default" .= v | Just v <- [nifDefault] ]
    ++ [ "help" .= v | Just v <- [nifHelp] ]

-- | A proposal: what the call would do, the fields it needs, the token
-- that confirms it and when that token expires (RFC 3339). A confirmation
-- is a proposal with no fields.
data NeedsInput = NeedsInput
  { niSummary :: Text
  , niEffects :: [Value]
  , niFields  :: [NeedsInputField]
  , niToken   :: ConfirmToken
  , niExpires :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

instance FromJSON NeedsInput where
  parseJSON = withObject "NeedsInput" $ \o -> do
    t <- o .: "type"
    if t /= ("needs_input" :: Text) then fail "not a needs_input item" else NeedsInput
      <$> o .: "summary"
      <*> o .:? "effects" .!= []
      <*> o .:? "fields" .!= []
      <*> o .: "token"
      <*> o .:? "expires"

instance ToJSON NeedsInput where
  toJSON NeedsInput{..} = object $
    [ "type" .= ("needs_input" :: Text)
    , "summary" .= niSummary
    , "effects" .= niEffects
    , "fields" .= niFields
    , "token" .= niToken
    ] ++ [ "expires" .= e | Just e <- [niExpires] ]

-- | The proposal a stream item carries: a data item whose content is
-- @{"type":"needs_input", …}@ with a summary and a token.
needsInputOf :: PlexusStreamItem -> Maybe NeedsInput
needsInputOf = \case
  StreamData _ _ _ content -> needsInputOfContent content
  _ -> Nothing

-- | The proposal a data item's content is, if it is one.
needsInputOfContent :: Value -> Maybe NeedsInput
needsInputOfContent v@(Object o)
  | KM.lookup "type" o == Just (String "needs_input") = case fromJSON v of
      Success ni -> Just ni
      Error _ -> Nothing
needsInputOfContent _ = Nothing

-- | The answer call's params: the call's own params, each field's value
-- under its own name, and @confirm@. A params value that is not an object
-- (no params) becomes one.
answerParams :: Value -> [(Text, Value)] -> ConfirmToken -> Value
answerParams params values token = Object $
  KM.insert "confirm" (String token) $
    foldr (\(k, v) -> KM.insert (K.fromText k) v) base values
  where
    base = case params of
      Object o -> o
      _ -> KM.empty
