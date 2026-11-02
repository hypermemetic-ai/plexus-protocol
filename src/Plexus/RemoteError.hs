-- | The typed, exhaustive vocabulary of call failures (plexus 0.7).
--
-- A call fails — as opposed to an activation saying no, which is an @err@
-- item and therefore data — when the service could not or would not run it: no
-- such activation or method, bad params, no credentials, not allowed, or the
-- method itself blew up. The error frame carries exactly one 'RemoteError'
-- ('Plexus.Types.StreamError'), and so does the @data@ of a JSON-RPC-level
-- rejection ('Plexus.Types.errData').
--
-- Mirrors @plexus_client::RemoteError@ (RETURNS-4), which is canonical.
--
-- = Wire shape
--
-- @kind@-tagged, @snake_case@. 'ParamProblem' and 'DenyReason' are flattened
-- into their variant, under @problem@ and @reason@:
--
-- @
-- {"kind": "activation_not_found", "activation": "tickets"}
-- {"kind": "method_not_found", "activation": "tickets", "method": "frob"}
-- {"kind": "invalid_params", "param": "id", "problem": "missing"}
-- {"kind": "invalid_params", "param": "id", "problem": "malformed", "detail": "expected a string"}
-- {"kind": "unauthenticated"}
-- {"kind": "forbidden", "reason": "missing_scope", "scope": "tickets:write"}
-- {"kind": "forbidden", "reason": "tenant_boundary"}
-- {"kind": "forbidden", "reason": "missing_grant", "grant": "tickets.set_status", "scope": "IDENTITY",
--  "roles": ["editor", "admin"], "held_on": ["DOCS"]}
-- {"kind": "handle_not_supported", "activation": "tickets"}
-- {"kind": "execution", "message": "database is locked"}
-- {"kind": "too_many_calls", "max": 8}
-- {"kind": "child_unavailable", "child": "echo", "reason": "the connection has closed"}
-- {"kind": "child_timeout", "child": "echo", "timeout_ms": 5000}
-- @
--
-- @param@ is omitted when the failure is not about one parameter, and
-- decodes as 'Nothing' when absent or @null@.
--
-- = Exhaustive on purpose
--
-- No catch-all constructor: a @kind@, @problem@ or @reason@ this library does
-- not know is a decode error, so a new failure kind fails every receiver's
-- build until it handles it.
--
-- The constructor names 'ActivationNotFound', 'MethodNotFound' and
-- 'InvalidParams' are also used by 'Plexus.Types.GuidanceErrorType', so
-- 'Plexus.Types' does not re-export them; import this module (or "Plexus").
module Plexus.RemoteError
  ( RemoteError(..)
  , ParamProblem(..)
  , DenyReason(..)
  , renderRemoteError
  , remoteErrorOfFields
  ) where

import Data.Aeson
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types (Pair, Parser)
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

-- | Why a call failed — what an error frame carries, and the @data@ of a
-- JSON-RPC-level rejection.
data RemoteError
  = ActivationNotFound Text
    -- ^ The service has no activation by this name (@activation@).
  | MethodNotFound Text Text
    -- ^ The activation (@activation@) exists but has no method by this name
    -- (@method@).
  | InvalidParams (Maybe Text) ParamProblem
    -- ^ The call's params did not fit the method: the parameter at fault
    -- (@param@), when the failure is about one, and what is wrong with it.
  | Unauthenticated
    -- ^ The method requires credentials and the call carried none that
    -- verified.
  | Forbidden DenyReason
    -- ^ The caller is authenticated but not allowed this call.
  | HandleNotSupported Text
    -- ^ The activation (@activation@) does not resolve handles.
  | Execution Text
    -- ^ The method ran and failed for a reason that is not the activation
    -- saying no (that is an @err@ item): a panic, a lost dependency
    -- (@message@).
  | TooManyCalls Integer (Maybe Text)
    -- ^ spec/05: too many calls open at once (@max@), on a child when the
    -- limit is a child's (@child@, omitted otherwise). RETURNS-36.
  | ChildUnavailable Text Text
    -- ^ spec/12: the child the call was routed to (@child@) cannot be
    -- reached (@reason@). RETURNS-36.
  | ChildTimeout Text Integer
    -- ^ spec/12: the child (@child@) did not open the call in the service's
    -- time (@timeout_ms@). RETURNS-36.
  deriving stock (Show, Eq, Ord, Generic)

-- | What is wrong with a call's params. Flattened into 'InvalidParams' under
-- the @problem@ key.
data ParamProblem
  = Missing
    -- ^ A required parameter was not supplied.
  | Malformed Text
    -- ^ A parameter was supplied but did not decode; the text (@detail@)
    -- says how.
  deriving stock (Show, Eq, Ord, Generic)

-- | Why an authenticated caller was refused. Flattened into 'Forbidden'
-- under the @reason@ key. Each carries at most the single fact the caller
-- already failed.
data DenyReason
  = MissingScope Text
    -- ^ The caller's scopes do not include this one (@scope@) — the first
    -- unmet one.
  | TenantBoundary
    -- ^ The call reaches across the caller's tenant boundary.
  | NotAccepted
    -- ^ The call was not accepted in the caller's action context.
  | MissingGrant Text Text [Text] [Text]
    -- ^ AUTH-2: the grant gate's refusal — the caller does not hold the
    -- method's grant (@grant@, @<activation>.<method>@) on the scope it was
    -- needed on (@scope@, @*@ for a method without a target); the roles that
    -- list it (@roles@, admin last) and where the caller does hold it
    -- (@held_on@). Mirrors main's @AuthzDenyReason::MissingGrant@.
  deriving stock (Show, Eq, Ord, Generic)

-- ============================================================================
-- JSON
-- ============================================================================

instance ToJSON RemoteError where
  toJSON = object . remoteErrorPairs

instance FromJSON RemoteError where
  parseJSON = withObject "RemoteError" $ \o -> do
    kind <- o .: "kind" :: Parser Text
    case kind of
      "activation_not_found" -> ActivationNotFound <$> o .: "activation"
      "method_not_found"     -> MethodNotFound <$> o .: "activation" <*> o .: "method"
      "invalid_params"       -> InvalidParams <$> o .:? "param" <*> parseParamProblem o
      "unauthenticated"      -> pure Unauthenticated
      "forbidden"            -> Forbidden <$> parseDenyReason o
      "handle_not_supported" -> HandleNotSupported <$> o .: "activation"
      "execution"            -> Execution <$> o .: "message"
      "too_many_calls"       -> TooManyCalls <$> o .: "max" <*> o .:? "child"
      "child_unavailable"    -> ChildUnavailable <$> o .: "child" <*> o .: "reason"
      "child_timeout"        -> ChildTimeout <$> o .: "child" <*> o .: "timeout_ms"
      other -> fail $ "unknown RemoteError kind: " <> T.unpack other

-- | Standalone, a 'ParamProblem' is its flattened fields:
-- @{"problem": "missing"}@ or @{"problem": "malformed", "detail": …}@.
instance ToJSON ParamProblem where
  toJSON = object . paramProblemPairs

instance FromJSON ParamProblem where
  parseJSON = withObject "ParamProblem" parseParamProblem

-- | Standalone, a 'DenyReason' is its flattened fields:
-- @{"reason": "missing_scope", "scope": …}@, @{"reason": "tenant_boundary"}@.
instance ToJSON DenyReason where
  toJSON = object . denyReasonPairs

instance FromJSON DenyReason where
  parseJSON = withObject "DenyReason" parseDenyReason

kindPair :: Text -> Pair
kindPair k = "kind" .= k

remoteErrorPairs :: RemoteError -> [Pair]
remoteErrorPairs = \case
  ActivationNotFound a ->
    [kindPair "activation_not_found", "activation" .= a]
  MethodNotFound a m ->
    [kindPair "method_not_found", "activation" .= a, "method" .= m]
  InvalidParams mParam problem ->
    kindPair "invalid_params"
      : maybe [] (\p -> ["param" .= p]) mParam
     <> paramProblemPairs problem
  Unauthenticated ->
    [kindPair "unauthenticated"]
  Forbidden reason ->
    kindPair "forbidden" : denyReasonPairs reason
  HandleNotSupported a ->
    [kindPair "handle_not_supported", "activation" .= a]
  Execution msg ->
    [kindPair "execution", "message" .= msg]
  TooManyCalls mx child ->
    kindPair "too_many_calls" : "max" .= mx : maybe [] (\c -> ["child" .= c]) child
  ChildUnavailable child reason ->
    [kindPair "child_unavailable", "child" .= child, "reason" .= reason]
  ChildTimeout child ms ->
    [kindPair "child_timeout", "child" .= child, "timeout_ms" .= ms]

paramProblemPairs :: ParamProblem -> [Pair]
paramProblemPairs = \case
  Missing          -> ["problem" .= ("missing" :: Text)]
  Malformed detail -> ["problem" .= ("malformed" :: Text), "detail" .= detail]

denyReasonPairs :: DenyReason -> [Pair]
denyReasonPairs = \case
  MissingScope scope -> ["reason" .= ("missing_scope" :: Text), "scope" .= scope]
  TenantBoundary     -> ["reason" .= ("tenant_boundary" :: Text)]
  NotAccepted        -> ["reason" .= ("not_accepted" :: Text)]
  MissingGrant g sc roles held ->
    [ "reason" .= ("missing_grant" :: Text), "grant" .= g, "scope" .= sc
    , "roles" .= roles, "held_on" .= held ]

parseParamProblem :: Object -> Parser ParamProblem
parseParamProblem o = do
  problem <- o .: "problem" :: Parser Text
  case problem of
    "missing"   -> pure Missing
    "malformed" -> Malformed <$> o .: "detail"
    other -> fail $ "unknown ParamProblem: " <> T.unpack other

parseDenyReason :: Object -> Parser DenyReason
parseDenyReason o = do
  reason <- o .: "reason" :: Parser Text
  case reason of
    "missing_scope"   -> MissingScope <$> o .: "scope"
    "tenant_boundary" -> pure TenantBoundary
    "not_accepted"    -> pure NotAccepted
    "missing_grant"   -> MissingGrant <$> o .: "grant" <*> o .: "scope"
                           <*> o .:? "roles" .!= [] <*> o .:? "held_on" .!= []
    other -> fail $ "unknown DenyReason: " <> T.unpack other

-- ============================================================================
-- Rendering
-- ============================================================================

-- | One line, for a human — what a CLI prints. The same line as the Rust
-- @Display@ (and the @message@ of a JSON-RPC rejection); line breaks in
-- server-supplied text are folded to spaces so it stays one line.
renderRemoteError :: RemoteError -> Text
renderRemoteError = \case
  ActivationNotFound a ->
    "activation not found: " <> oneLine a
  MethodNotFound a m ->
    "method not found: " <> oneLine a <> "." <> oneLine m
  InvalidParams mParam problem -> "invalid params: " <> case (problem, mParam) of
    (Missing, Just p)        -> "missing `" <> oneLine p <> "`"
    (Missing, Nothing)       -> "a required parameter is missing"
    (Malformed d, Just p)    -> "malformed `" <> oneLine p <> "`: " <> oneLine d
    (Malformed d, Nothing)   -> "malformed: " <> oneLine d
  Unauthenticated ->
    "unauthenticated: credentials required"
  Forbidden reason -> "forbidden: " <> case reason of
    MissingScope s -> "missing required scope `" <> oneLine s <> "`"
    TenantBoundary -> "tenant boundary"
    NotAccepted    -> "call not accepted"
    MissingGrant g "*" _ _ -> "needs `" <> oneLine g <> "`"
    MissingGrant g sc _ _  -> "needs `" <> oneLine g <> "` on `" <> oneLine sc <> "`"
  HandleNotSupported a ->
    "handle resolution not supported by activation: " <> oneLine a
  Execution msg ->
    "execution failed: " <> oneLine msg
  TooManyCalls mx child ->
    "too many calls open" <> maybe "" (\c -> " on child " <> oneLine c) child
      <> " (at most " <> T.pack (show mx) <> "): close one, or call again later"
  ChildUnavailable child reason ->
    "child " <> oneLine child <> " is unavailable: " <> oneLine reason
  ChildTimeout child ms ->
    "child " <> oneLine child <> " did not open the call in " <> T.pack (show ms) <> " ms"

oneLine :: Text -> Text
oneLine = T.map (\c -> if c == '\n' || c == '\r' then ' ' else c)

-- ============================================================================
-- A 0.7.0 frame's fields
-- ============================================================================

-- | RETURNS-36: the 'RemoteError' a frame of the published 0.7.0 line says
-- with spec/05's fields alone (no typed @error@): by @error_kind@ when there
-- is one, else by @code@; what neither names is the method failing
-- (@execution@, its @message@). The same reading as plexus-core's child
-- seam (@remote_child/peer.rs@, @remote_error_of@).
remoteErrorOfFields :: Object -> RemoteError
remoteErrorOfFields o = case maybe code id (str "error_kind") of
  k | k `elem` ["invalid_params", "-32602"] -> InvalidParams Nothing (Malformed message)
    | k `elem` ["unauthenticated", "-32001"] -> Unauthenticated
    | k `elem` ["forbidden", "-32003"] -> Forbidden $ case str "reason" of
        Just "missing_scope" -> MissingScope (txt "scope")
        Just "tenant_boundary" -> TenantBoundary
        Just "not_accepted" -> NotAccepted
        _ -> MissingGrant (txt "grant") (txt "scope") (strings "roles") (strings "held_on")
  "too_many_calls" -> TooManyCalls (int "max") (str "child")
  "child_unavailable" -> ChildUnavailable (txt "child") (txt "reason")
  "child_timeout" -> ChildTimeout (txt "child") (int "timeout_ms")
  _ -> Execution message
  where
    str k = case KM.lookup k o of { Just (String t) -> Just t; _ -> Nothing }
    txt k = maybe "" id (str k)
    message = txt "message"
    code = case KM.lookup "code" o of
      Just (String c) -> c
      Just n@(Number _) | Success i <- fromJSON n -> T.pack (show (i :: Integer))
      _ -> ""
    int k = case KM.lookup k o of
      Just n@(Number _) | Success i <- fromJSON n -> i
      _ -> 0
    strings k = case KM.lookup k o of
      Just (Array a) -> [ t | String t <- foldr (:) [] a ]
      _ -> []
