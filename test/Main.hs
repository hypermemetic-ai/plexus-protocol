-- | plexus-protocol 0.6 against plexus 0.7.
--
-- The golden half reads RETURNS-7's conformance fixture by sibling path
-- (@../plexus-macros/fixtures/conformance/expected/@) and checks that every
-- schema, wire frame and error frame there parses into the Haskell types and
-- re-encodes to the same JSON (compared as parsed values, so key order does
-- not count; frame @timestamp@ normalized to 0 — the fixture README's
-- comparison rules). Its hashes are real since RETURNS-10: every frame's
-- @plexus_hash@ is the schema's @hash@.
--
-- The rest pins the shapes the fixture does not exercise: every
-- 'RemoteError' variant against RETURNS-4's wire literals
-- (@plexus-client/tests/remote_error_wire_tests.rs@), the JSON-RPC
-- rejection's @data@, and the decode errors that replace 0.6's silent
-- defaults.
module Main (main) where

import Control.Monad (forM_, unless)
import Data.Aeson
import Data.Aeson.Types (parseEither)
import qualified Data.Aeson.Key as Key
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BS8
import qualified Data.ByteString.Lazy as LBS
import Data.Foldable (toList)
import Data.List (isInfixOf, sort)
import qualified Data.Set as Set
import Data.Text (Text)
import qualified Data.Text as T
import System.Directory (doesDirectoryExist, listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Hspec

import Plexus.Auth (Refusal (..), View (..))
import Plexus.RemoteError
import Plexus.Schema.Recursive
import Plexus.Types hiding (GuidanceErrorType (..))

-- ============================================================================
-- The fixture
-- ============================================================================

fixtureDir :: FilePath
fixtureDir = ".." </> "plexus-macros" </> "fixtures" </> "conformance" </> "expected"

readJson :: FilePath -> IO Value
readJson path = do
  bytes <- LBS.readFile path
  either (fail . ((path <> ": ") <>)) pure (eitherDecode bytes)

-- | One JSON value per non-empty line.
readJsonl :: FilePath -> IO [Value]
readJsonl path = do
  bytes <- BS.readFile path
  let ls = filter (not . BS.null) (BS8.lines bytes)
  sequence
    [ either (\e -> fail (path <> ":" <> show n <> ": " <> e)) pure (eitherDecodeStrict l)
    | (n, l) <- zip [1 :: Int ..] ls
    ]

-- | The wire files @calls.json@ names, and the @errors.jsonl@ lines it names.
data Calls = Calls
  { callWireFiles  :: Set.Set FilePath
  , callErrorLines :: [Int]
  }

readCalls :: IO Calls
readCalls = do
  v <- readJson (fixtureDir </> "calls.json")
  calls <- either fail pure $ parseEither (withArray "calls" (mapM callFrames . toList)) v
  pure Calls
    { callWireFiles  = Set.fromList [f | (f, Nothing) <- calls]
    , callErrorLines = sort [n | ("errors.jsonl", Just n) <- calls]
    }
  where
    callFrames = withObject "call" $ \o -> (,) <$> o .: "frames" <*> o .:? "line"

-- ============================================================================
-- Comparing (the fixture README's rules)
-- ============================================================================

-- | A frame's @metadata.timestamp@, set to 0.
normalizeFrame :: Value -> Value
normalizeFrame (Object o) = Object $ case KM.lookup "metadata" o of
  Just m  -> KM.insert "metadata" (zeroTimestamp m) o
  Nothing -> o
  where
    zeroTimestamp (Object m) | KM.member "timestamp" m = Object (KM.insert "timestamp" (Number 0) m)
    zeroTimestamp m = m
normalizeFrame v = v

-- | @expected `matches` actual@: equal as parsed JSON.
matches :: Value -> Value -> Bool
matches (Object e) (Object a) =
  sort (KM.keys e) == sort (KM.keys a)
    && and [sub ve (KM.lookup k a) | (k, ve) <- KM.toList e]
  where
    sub ve (Just va) = matches ve va
    sub _ Nothing = False
matches (Array e) (Array a) = length e == length a && and (zipWith matches (toList e) (toList a))
matches e a = e == a

-- | Parse @v@ as an @a@, re-encode it, and require the result to match @v@.
roundTrips :: forall a. (FromJSON a, ToJSON a) => (Value -> Value) -> Value -> IO a
roundTrips norm v = case parseEither parseJSON v of
  Left err -> expectationFailure ("does not parse: " <> err <> "\n  " <> encodeStr v) >> fail "unreachable"
  Right (x :: a) -> do
    let back = toJSON x
    unless (norm v `matches` norm back) $
      expectationFailure $ "re-encodes differently\n  expected: " <> encodeStr v <> "\n  actual:   " <> encodeStr back
    pure x

encodeStr :: Value -> String
encodeStr = BS8.unpack . LBS.toStrict . encode

decodes :: FromJSON a => Value -> Either String a
decodes = parseEither parseJSON

field :: Text -> Value -> Maybe Value
field k (Object o) = KM.lookup (Key.fromText k) o
field _ _ = Nothing

title :: Value -> Maybe Value
title = field "title"

-- | 16 lowercase hex characters: every Plexus hash's form (RETURNS-10).
isHash16 :: Text -> Bool
isHash16 h = T.length h == 16 && T.all (`elem` ("0123456789abcdef" :: String)) h

-- | The fixture activation's hash, as @schema.json@ pins it.
fixtureHash :: IO Text
fixtureHash = do
  v <- readJson (fixtureDir </> "schema.json")
  either fail (pure . psHash) (decodes @PluginSchema v)

-- ============================================================================
-- Tests
-- ============================================================================

main :: IO ()
main = do
  present <- doesDirectoryExist fixtureDir
  unless present $
    fail $ "conformance fixture not found at " <> fixtureDir
      <> " (plexus-macros on returns/0.7 must be this repository's sibling)"
  hspec spec

spec :: Spec
spec = do
  schemaSpec
  wireSpec
  errorsSpec
  remoteErrorSpec
  rejectionSpec
  strictnessSpec
  mergedSpec

schemaSpec :: Spec
schemaSpec = describe "expected/schema.json" $ do
  let load = readJson (fixtureDir </> "schema.json")

  it "parses as a PluginSchema and re-encodes to the same JSON" $ do
    v <- load
    schema <- roundTrips @PluginSchema id v
    psNamespace schema `shouldBe` "conformance"
    psVersion schema `shouldBe` "0.7.0"
    fmap isHash16 (psSelfHash schema) `shouldBe` Just True
    isHash16 (psHash schema) `shouldBe` True
    forM_ (psMethods schema) $ \m -> (methodName m, isHash16 (methodHash m)) `shouldBe` (methodName m, True)
    psChildren schema `shouldBe` Nothing
    map methodName (psMethods schema) `shouldBe` ["ping", "lookup", "count", "scan", "load", "warm", "schema"]

  it "carries an activation's request schema as psRequest (CONTEXT-5) and omits it when absent" $ do
    v <- load
    plain <- either fail pure (decodes @PluginSchema v)
    psRequest plain `shouldBe` Nothing
    let req = object
          [ "type" .= ("object" :: Text)
          , "properties" .= object
              [ "auth_token" .= object [ "type" .= ("string" :: Text), "x-plexus-source" .= object [ "from" .= ("cookie" :: Text), "name" .= ("access_token" :: Text) ] ] ]
          ]
        withReq = case v of
          Object o -> Object (KM.insert "request" req o)
          other    -> other
    schema <- roundTrips @PluginSchema id withReq
    psRequest schema `shouldBe` Just req
    toJSON plain `shouldBe` v

  it "parses as a SchemaResult (a plugin) and re-encodes to the same JSON" $ do
    v <- load
    result <- roundTrips @SchemaResult id v
    case result of
      SchemaPlugin _ -> pure ()
      SchemaMethod _ -> expectationFailure "a plugin schema parsed as a method"

  it "each method alone parses as a MethodSchema and as a SchemaResult (a method)" $ do
    v <- load
    methods <- maybe (fail "no methods") (pure . toList) $ case field "methods" v of
      Just (Array ms) -> Just ms
      _ -> Nothing
    length methods `shouldBe` 7
    forM_ methods $ \m -> do
      _ <- roundTrips @MethodSchema id m
      result <- roundTrips @SchemaResult id m
      case result of
        SchemaMethod _ -> pure ()
        SchemaPlugin _ -> expectationFailure "a method schema parsed as a plugin"

  it "reads each method's returns, error, progress and cardinality" $ do
    schema <- load >>= either fail pure . decodes @PluginSchema
    let shape m =
          ( methodName m
          , title (methodReturns m)
          , title =<< methodError m
          , title =<< methodProgress m
          , methodCardinality m
          )
    map shape (psMethods schema) `shouldBe`
      [ ("ping",   Just "Pong",    Nothing,            Nothing,         CardinalityOne)
      , ("lookup", Just "Thing",   Just "LookupError", Nothing,         CardinalityOne)
      , ("count",  Just "Tick",    Nothing,            Nothing,         CardinalityMany)
      , ("scan",   Just "Row",     Just "ScanError",   Nothing,         CardinalityMany)
      , ("load",   Just "Loaded",  Just "LoadError",   Just "LoadStep", CardinalityMany)
      , ("warm",   Just "Warmed",  Nothing,            Just "Note",     CardinalityMany)
      , ("schema", Just "SchemaResult", Nothing,      Nothing,         CardinalityOne)  -- the macro-generated method (RETURNS-26: a PluginSchema, or one MethodSchema)
      ]
    forM_ (psMethods schema) $ \m -> do
      methodHttpMethod m `shouldBe` "POST"
      methodRole m `shouldBe` MethodRoleRpc
      methodBidirectional m `shouldBe` False

wireSpec :: Spec
wireSpec = describe "expected/wire/*.jsonl" $ do
  let wireDir = fixtureDir </> "wire"
      frames f = readJsonl (wireDir </> f)
      parsed f = frames f >>= mapM (either fail pure . decodes @PlexusStreamItem)

  it "holds exactly the wire files calls.json names" $ do
    files <- filter ((== ".jsonl") . takeExtension) <$> listDirectory wireDir
    calls <- readCalls
    Set.fromList (map ("wire" </>) files) `shouldBe` callWireFiles calls

  it "every frame of every call parses and re-encodes to the same JSON" $ do
    hash <- fixtureHash
    files <- sort . filter ((== ".jsonl") . takeExtension) <$> listDirectory wireDir
    length files `shouldBe` 7
    forM_ files $ \f -> do
      vs <- frames f
      items <- mapM (roundTrips @PlexusStreamItem normalizeFrame) vs
      -- each call ends in its one done
      [() | StreamDone{} <- items] `shouldBe` [()]
      case last items of
        StreamDone{} -> pure ()
        other -> expectationFailure (f <> " does not end in done: " <> show other)
      forM_ items $ \item -> do
        itemProvenance item `shouldBe` Provenance ["conformance"]
        itemPlexusHash item `shouldBe` hash

  it "load: two typed progress frames (LoadStep), then its {ok} item" $ do
    items <- parsed "load.jsonl"
    [ (ct, c) | StreamProgress _ _ ct c <- items ] `shouldBe`
      [ ("conformance.load", object ["type" .= ("batch" :: Text), "done" .= (1 :: Int), "of" .= (2 :: Int)])
      , ("conformance.load", object ["type" .= ("batch" :: Text), "done" .= (2 :: Int), "of" .= (2 :: Int)])
      ]
    [ (ct, c) | StreamData _ _ ct c <- items ] `shouldBe`
      [ ("conformance.load", object ["ok" .= object ["items" .= (2 :: Int)]]) ]

  it "warm: two Note progress frames, the second with a percentage" $ do
    items <- parsed "warm.jsonl"
    [ c | StreamProgress _ _ "conformance.warm" c <- items ] `shouldBe`
      [ object ["message" .= ("cache cold" :: Text)]
      , object ["message" .= ("cache warming" :: Text), "percentage" .= (50.0 :: Double)]
      ]

  it "scan: an {err} item between two {ok} items, the stream carrying on" $ do
    items <- parsed "scan.jsonl"
    [ c | StreamData _ _ "conformance.scan" c <- items ] `shouldBe`
      [ object ["ok" .= object ["key" .= ("a1" :: Text)]]
      , object ["err" .= object ["type" .= ("unreadable" :: Text), "key" .= ("a2" :: Text)]]
      , object ["ok" .= object ["key" .= ("a3" :: Text)]]
      ]

  it "plain-T methods carry their item unwrapped" $ do
    ping <- parsed "ping.jsonl"
    [ c | StreamData _ _ _ c <- ping ] `shouldBe` [object ["message" .= ("pong" :: Text)]]
    count <- parsed "count.jsonl"
    [ c | StreamData _ _ _ c <- count ] `shouldBe` [object ["n" .= n] | n <- [1 :: Int, 2, 3]]

errorsSpec :: Spec
errorsSpec = describe "expected/errors.jsonl" $ do
  let load = readJsonl (fixtureDir </> "errors.jsonl")

  it "holds exactly the lines calls.json names" $ do
    vs <- load
    calls <- readCalls
    callErrorLines calls `shouldBe` [1 .. length vs]

  it "every error frame parses and re-encodes to the same JSON" $ do
    hash <- fixtureHash
    vs <- load
    items <- mapM (roundTrips @PlexusStreamItem normalizeFrame) vs
    length items `shouldBe` 3
    forM_ items $ \item -> itemPlexusHash item `shouldBe` hash

  it "carries a typed RemoteError, not recoverable" $ do
    items <- load >>= mapM (either fail pure . decodes @PlexusStreamItem)
    [ (e, r) | StreamError _ _ e r <- items ] `shouldBe`
      [ (MethodNotFound "conformance" "frob", False)
      , (InvalidParams (Just "id") Missing, False)
      , (InvalidParams (Just "to") (Malformed "invalid type: string \"three\", expected u32"), False)
      ]
    [ renderRemoteError e | StreamError _ _ e _ <- items ] `shouldBe`
      [ "method not found: conformance.frob"
      , "invalid params: missing `id`"
      , "invalid params: malformed `to`: invalid type: string \"three\", expected u32"
      ]

-- | RETURNS-4's wire literals, variant by variant.
wireLiterals :: [(RemoteError, Value)]
wireLiterals =
  [ ( ActivationNotFound "tickets"
    , object ["kind" .= s "activation_not_found", "activation" .= s "tickets"] )
  , ( MethodNotFound "tickets" "frob"
    , object ["kind" .= s "method_not_found", "activation" .= s "tickets", "method" .= s "frob"] )
  , ( InvalidParams (Just "id") Missing
    , object ["kind" .= s "invalid_params", "param" .= s "id", "problem" .= s "missing"] )
  , ( InvalidParams (Just "limit") (Malformed "invalid type: string \"ten\", expected u32")
    , object [ "kind" .= s "invalid_params", "param" .= s "limit", "problem" .= s "malformed"
             , "detail" .= s "invalid type: string \"ten\", expected u32" ] )
  , ( InvalidParams Nothing (Malformed "params must be an object")
    , object ["kind" .= s "invalid_params", "problem" .= s "malformed", "detail" .= s "params must be an object"] )
  , ( InvalidParams Nothing Missing
    , object ["kind" .= s "invalid_params", "problem" .= s "missing"] )
  , ( Unauthenticated
    , object ["kind" .= s "unauthenticated"] )
  , ( Forbidden (MissingScope "tickets:write")
    , object ["kind" .= s "forbidden", "reason" .= s "missing_scope", "scope" .= s "tickets:write"] )
  , ( Forbidden TenantBoundary
    , object ["kind" .= s "forbidden", "reason" .= s "tenant_boundary"] )
  , ( Forbidden NotAccepted
    , object ["kind" .= s "forbidden", "reason" .= s "not_accepted"] )
  , ( HandleNotSupported "tickets"
    , object ["kind" .= s "handle_not_supported", "activation" .= s "tickets"] )
  , ( Execution "database is locked"
    , object ["kind" .= s "execution", "message" .= s "database is locked"] )
    -- main's refusals in the vocabulary (RETURNS-36; plexus-client's remote_error.rs)
  , ( Forbidden (MissingGrant "tickets.create" "*" ["editor", "admin"] [])
    , object [ "kind" .= s "forbidden", "reason" .= s "missing_grant", "grant" .= s "tickets.create", "scope" .= s "*"
             , "roles" .= [s "editor", s "admin"], "held_on" .= ([] :: [Text]) ] )
  , ( TooManyCalls 8 Nothing
    , object ["kind" .= s "too_many_calls", "max" .= (8 :: Int)] )
  , ( TooManyCalls 4 (Just "echo")
    , object ["kind" .= s "too_many_calls", "max" .= (4 :: Int), "child" .= s "echo"] )
  , ( ChildUnavailable "echo" "the connection has closed"
    , object ["kind" .= s "child_unavailable", "child" .= s "echo", "reason" .= s "the connection has closed"] )
  , ( ChildTimeout "echo" 5000
    , object ["kind" .= s "child_timeout", "child" .= s "echo", "timeout_ms" .= (5000 :: Int)] )
  ]

s :: Text -> Text
s = id

remoteErrorSpec :: Spec
remoteErrorSpec = describe "RemoteError (RETURNS-4 wire literals)" $ do
  forM_ wireLiterals $ \(err, wire) ->
    it ("round-trips " <> encodeStr wire) $ do
      toJSON err `shouldBe` wire
      decodes wire `shouldBe` Right err
      eitherDecode (encode wire) `shouldBe` Right err

  it "an invalid_params param of null decodes as Nothing" $
    decodes (object ["kind" .= s "invalid_params", "param" .= Null, "problem" .= s "missing"])
      `shouldBe` Right (InvalidParams Nothing Missing)

  it "a kind, reason or problem it does not know is a decode error, naming it" $
    forM_
      [ (object ["kind" .= s "rate_limited"], "rate_limited")
      , (object ["kind" .= s "forbidden", "reason" .= s "too_tall"], "too_tall")
      , (object ["kind" .= s "invalid_params", "param" .= s "id", "problem" .= s "smelly"], "smelly")
      ] $ \(wire, name) -> case decodes @RemoteError wire of
        Right e -> expectationFailure ("decoded " <> show e <> " from " <> encodeStr wire)
        Left msg -> msg `shouldSatisfy` (name `isInfixOf`)

  it "a required field that is absent is a decode error" $
    forM_
      [ object ["kind" .= s "method_not_found", "activation" .= s "tickets"]
      , object ["kind" .= s "invalid_params", "param" .= s "id"]
      , object ["kind" .= s "invalid_params", "problem" .= s "malformed"]
      , object ["kind" .= s "forbidden"]
      , object ["kind" .= s "forbidden", "reason" .= s "missing_scope"]
      , object ["activation" .= s "tickets"]
      ] $ \wire -> decodes @RemoteError wire `shouldSatisfy` isLeft

  it "renders the same one line as the Rust Display" $
    map (renderRemoteError . fst) wireLiterals `shouldBe`
      [ "activation not found: tickets"
      , "method not found: tickets.frob"
      , "invalid params: missing `id`"
      , "invalid params: malformed `limit`: invalid type: string \"ten\", expected u32"
      , "invalid params: malformed: params must be an object"
      , "invalid params: a required parameter is missing"
      , "unauthenticated: credentials required"
      , "forbidden: missing required scope `tickets:write`"
      , "forbidden: tenant boundary"
      , "forbidden: call not accepted"
      , "handle resolution not supported by activation: tickets"
      , "execution failed: database is locked"
      , "forbidden: needs `tickets.create`"
      , "too many calls open (at most 8): close one, or call again later"
      , "too many calls open on child echo (at most 4): close one, or call again later"
      , "child echo is unavailable: the connection has closed"
      , "child echo did not open the call in 5000 ms"
      ]

  it "folds line breaks in server text so the line stays one line" $
    renderRemoteError (Execution "panicked at src/lib.rs:1:1:\nboom\r\nagain")
      `shouldBe` "execution failed: panicked at src/lib.rs:1:1: boom  again"

rejectionSpec :: Spec
rejectionSpec = describe "JSON-RPC rejection" $ do
  let rejection = object
        [ "code" .= (-32601 :: Int)
        , "message" .= s "method not found: tickets.frob"
        , "data" .= object ["kind" .= s "method_not_found", "activation" .= s "tickets", "method" .= s "frob"]
        ]

  it "reads data as a RemoteError and re-encodes to the same JSON" $ do
    e <- roundTrips @RpcError id rejection
    e `shouldBe` RpcErrorObj (-32601) "method not found: tickets.frob" (Just (MethodNotFound "tickets" "frob"))

  it "a response carrying it is an RpcError" $
    decodes (object ["jsonrpc" .= s "2.0", "id" .= (7 :: Int), "error" .= rejection])
      `shouldBe` Right (RpcError (RequestId 7) (RpcErrorObj (-32601) "method not found: tickets.frob" (Just (MethodNotFound "tickets" "frob"))))

  it "a rejection without data (not a Plexus call failure) has no RemoteError" $ do
    e <- roundTrips @RpcError id (object ["code" .= (-32601 :: Int), "message" .= s "Method not found"])
    errData e `shouldBe` Nothing
    decodes (object ["code" .= (-32602 :: Int), "message" .= s "Method 'x' not found", "data" .= Null])
      `shouldBe` Right (RpcErrorObj (-32602) "Method 'x' not found" Nothing)

  it "jsonrpsee's own param rejection (string data) has no RemoteError" $
    decodes (object ["code" .= (-32602 :: Int), "message" .= s "Invalid params", "data" .= s "missing field `method`"])
      `shouldBe` Right (RpcErrorObj (-32602) "Invalid params" Nothing)

  it "a kind-tagged data of an unknown kind is a decode error" $
    decodes @RpcError (object ["code" .= (-32000 :: Int), "message" .= s "x", "data" .= object ["kind" .= s "rate_limited"]])
      `shouldSatisfy` isLeft

strictnessSpec :: Spec
strictnessSpec = describe "0.7 shapes are required, not defaulted" $ do
  let method extra = object $
        [ "name" .= s "get", "description" .= s "d", "hash" .= s "h" ] <> extra
      returns = "returns" .= object []
      returns' = "returns" .= object ["type" .= s "string"]

  it "Cardinality is \"one\" or \"many\"" $ do
    toJSON CardinalityOne `shouldBe` String "one"
    toJSON CardinalityMany `shouldBe` String "many"
    decodes (String "one") `shouldBe` Right CardinalityOne
    decodes (String "many") `shouldBe` Right CardinalityMany
    decodes @Cardinality (String "some") `shouldSatisfy` isLeft

  it "a method with neither cardinality nor streaming does not parse" $
    decodes @MethodSchema (method [returns]) `shouldSatisfy` isLeft

  it "a method without returns does not parse" $
    decodes @MethodSchema (method ["cardinality" .= s "one"]) `shouldSatisfy` isLeft

  it "a minimal 0.7 method parses with the Rust defaults" $ do
    m <- either fail pure $ decodes @MethodSchema (method [returns, "cardinality" .= s "many"])
    methodCardinality m `shouldBe` CardinalityMany
    methodError m `shouldBe` Nothing
    methodProgress m `shouldBe` Nothing
    methodHttpMethod m `shouldBe` "POST"
    methodRole m `shouldBe` MethodRoleRpc
    toJSON m `shouldBe` method
      [ returns, "cardinality" .= s "many", "bidirectional" .= False
      , "http_method" .= s "POST", "role" .= object ["kind" .= s "rpc"] ]

  let meta = "metadata" .= object
        ["provenance" .= [s "tickets"], "plexus_hash" .= s "h", "timestamp" .= (0 :: Int)]

  -- RETURNS-36 (the coordinator's decision): synapse 4.0 reads the
  -- published 0.7.0 line too, as plexus-core's child seam does
  -- (remote_child/peer.rs) — from what the peer itself declared, never
  -- guessing; a 0.8 schema is read strictly, as above.
  it "a 0.7.0 method: its cardinality is its streaming flag, its missing returns {}" $ do
    many <- either fail pure $ decodes @MethodSchema (method ["streaming" .= True])
    methodCardinality many `shouldBe` CardinalityMany
    methodReturns many `shouldBe` object []
    one <- either fail pure $ decodes @MethodSchema (method [returns', "streaming" .= False])
    methodCardinality one `shouldBe` CardinalityOne
    methodReturns one `shouldBe` object ["type" .= s "string"]
    -- written back in 0.8's shape
    toJSON many `shouldBe` method
      [ returns, "cardinality" .= s "many", "bidirectional" .= False
      , "http_method" .= s "POST", "role" .= object ["kind" .= s "rpc"] ]

  it "a 0.8 method's cardinality wins over a streaming flag beside it" $ do
    m <- either fail pure $ decodes @MethodSchema (method [returns, "cardinality" .= s "one", "streaming" .= True])
    methodCardinality m `shouldBe` CardinalityOne

  it "a 0.7.0 progress frame (message, percentage) reads as the default progress type, Note" $ do
    item <- either fail pure $ decodes @PlexusStreamItem
      (object ["type" .= s "progress", meta, "message" .= s "half", "percentage" .= (50 :: Int)])
    case item of
      StreamProgress _ _ _ c -> c `shouldBe` object ["message" .= s "half", "percentage" .= (50 :: Int)]
      other -> expectationFailure (show other)

  it "a 0.7.0 error frame (message, code) reads typed from its fields, and keeps them" $ do
    let wire = object ["type" .= s "error", meta, "message" .= s "boom", "code" .= s "-32000", "recoverable" .= False]
    item <- either fail pure $ decodes @PlexusStreamItem wire
    itemError item `shouldBe` Execution "boom"
    toJSON item `shouldBe` object
      [ "type" .= s "error", meta, "message" .= s "boom", "code" .= s "-32000", "recoverable" .= False
      , "error" .= object ["kind" .= s "execution", "message" .= s "boom"] ]

  it "a 0.7.0 error frame: error_kind first, else code, as peer.rs reads it" $ do
    let frame extra = object (["type" .= s "error", meta, "recoverable" .= False] <> extra)
        typed extra = either error itemError (decodes @PlexusStreamItem (frame extra))
    typed [ "message" .= s "needs tickets.set_status on IDENTITY", "code" .= s "-32003", "error_kind" .= s "forbidden"
          , "grant" .= s "tickets.set_status", "scope" .= s "IDENTITY"
          , "roles" .= [s "editor", s "admin"], "held_on" .= [s "DOCS"] ]
      `shouldBe` Forbidden (MissingGrant "tickets.set_status" "IDENTITY" ["editor", "admin"] ["DOCS"])
    typed ["message" .= s "x", "code" .= s "-32003"] `shouldBe` Forbidden (MissingGrant "" "" [] [])
    typed ["message" .= s "x", "error_kind" .= s "forbidden", "reason" .= s "missing_scope", "scope" .= s "t:w"]
      `shouldBe` Forbidden (MissingScope "t:w")
    typed ["message" .= s "bad", "code" .= s "-32602"] `shouldBe` InvalidParams Nothing (Malformed "bad")
    typed ["message" .= s "bad", "error_kind" .= s "invalid_params"] `shouldBe` InvalidParams Nothing (Malformed "bad")
    typed ["message" .= s "sign in", "code" .= (-32001 :: Int)] `shouldBe` Unauthenticated
    typed ["message" .= s "busy", "error_kind" .= s "too_many_calls", "max" .= (8 :: Int)] `shouldBe` TooManyCalls 8 Nothing
    typed ["error_kind" .= s "child_unavailable", "child" .= s "echo", "reason" .= s "closed"] `shouldBe` ChildUnavailable "echo" "closed"
    typed ["error_kind" .= s "child_timeout", "child" .= s "echo", "timeout_ms" .= (5000 :: Int)] `shouldBe` ChildTimeout "echo" 5000
    -- spec/05: -32601 is an activation or method not found, its message naming which
    typed ["message" .= s "Method not found: tickets.nosuch", "code" .= s "-32601"] `shouldBe` MethodNotFound "tickets" "nosuch"
    typed ["message" .= s "Activation not found: nosuch", "code" .= s "-32601"] `shouldBe` ActivationNotFound "nosuch"
    typed ["message" .= s "Method not found: tickets.inputs.nosuch", "code" .= s "-32601"] `shouldBe` MethodNotFound "tickets.inputs" "nosuch"
    itemRefusal (either error id (decodes @PlexusStreamItem (frame
      [ "message" .= s "needs x", "error_kind" .= s "forbidden", "grant" .= s "tickets.get", "scope" .= s "*" ])))
      `shouldSatisfy` maybe False ((== Just "tickets.get") . refusalGrant)

  it "an error frame must say whether it is recoverable" $
    decodes @PlexusStreamItem
      (object ["type" .= s "error", meta, "error" .= object ["kind" .= s "unauthenticated"]])
      `shouldSatisfy` isLeft

isLeft :: Either a b -> Bool
isLeft = either (const True) (const False)

-- | RETURNS-36: what main brought beside 0.7 — AUTH-2's refusal on a typed
-- error frame, AUTH-3/4's schema fields, SYNAPSE-13's and SYNAPSE-CC-8's.
mergedSpec :: Spec
mergedSpec = describe "0.7 merged with main's auth (RETURNS-36)" $ do
  let meta = "metadata" .= object
        ["provenance" .= [s "tickets"], "plexus_hash" .= s "h", "timestamp" .= (0 :: Int)]
      grantWire = object
        [ "kind" .= s "forbidden", "reason" .= s "missing_grant", "grant" .= s "tickets.set_status"
        , "scope" .= s "IDENTITY", "roles" .= [s "editor", s "admin"], "held_on" .= [s "DOCS"] ]
      grantErr = Forbidden (MissingGrant "tickets.set_status" "IDENTITY" ["editor", "admin"] ["DOCS"])
      refusal = Refusal
        { refusalGrant = Just "tickets.set_status", refusalScope = Just "IDENTITY"
        , refusalRoles = ["editor", "admin"], refusalHeldOn = ["DOCS"], refusalCeiling = []
        , refusalMessage = "needs tickets.set_status on IDENTITY" }

  it "a missing grant is a forbidden RemoteError, and round-trips" $ do
    decodes grantWire `shouldBe` Right grantErr
    toJSON grantErr `shouldBe` grantWire
    -- the Rust Display's line (plexus-client's remote_error.rs)
    renderRemoteError grantErr `shouldBe` "forbidden: needs `tickets.set_status` on `IDENTITY`"
    renderRemoteError (Forbidden (MissingGrant "auth.role_set" "*" [] []))
      `shouldBe` "forbidden: needs `auth.role_set`"

  it "an error frame whose typed error is a missing grant names its refusal" $ do
    item <- roundTrips @PlexusStreamItem id
      (object ["type" .= s "error", meta, "recoverable" .= False, "error" .= grantWire
              , "message" .= s "needs tickets.set_status on IDENTITY"])
    itemError item `shouldBe` grantErr
    itemErrorRefusal item `shouldBe` Nothing
    itemRefusal item `shouldBe` Just refusal

  it "AUTH-2's refusal fields beside the typed error are kept and re-encoded" $ do
    let wire = object
          [ "type" .= s "error", meta, "recoverable" .= False, "error" .= grantWire
          , "message" .= s "needs tickets.set_status on IDENTITY", "code" .= s "-32003"
          , "error_kind" .= s "forbidden", "grant" .= s "tickets.set_status", "scope" .= s "IDENTITY"
          , "roles" .= [s "editor", s "admin"], "held_on" .= [s "DOCS"] ]
    item <- roundTrips @PlexusStreamItem id wire
    itemErrorRefusal item `shouldBe` Just refusal
    itemRefusal item `shouldBe` Just refusal

  it "main's message, code and error_kind beside the typed error are kept as sent" $ do
    item <- roundTrips @PlexusStreamItem id $ object
      [ "type" .= s "error", meta, "recoverable" .= False
      , "error" .= object ["kind" .= s "invalid_params", "param" .= s "id", "problem" .= s "missing"]
      , "message" .= s "Invalid params: missing field: id", "code" .= s "-32602", "error_kind" .= s "invalid_params" ]
    itemError item `shouldBe` InvalidParams (Just "id") Missing
    itemRefusal item `shouldBe` Nothing

  -- the review (RETURNS-36 #1): the frames plexus-core 0.8 writes
  -- (PlexusStreamItem::from_plexus_error: message, code, the typed error and
  -- spec/05's fields), and the server's message as callers read it
  it "a signed-out session's frame, as plexus-core 0.8 writes it: unauthenticated, the server's message kept" $ do
    item <- roundTrips @PlexusStreamItem id $ object
      [ "type" .= s "error", meta, "recoverable" .= False
      , "message" .= s "Authentication required: you have signed out: sign in again", "code" .= s "-32001"
      , "error" .= object ["kind" .= s "unauthenticated"], "error_kind" .= s "unauthenticated" ]
    itemError item `shouldBe` Unauthenticated
    itemErrorMessage item `shouldBe` Just "Authentication required: you have signed out: sign in again"

  it "a missing param's frame, as plexus-core 0.8 writes it" $ do
    item <- roundTrips @PlexusStreamItem id $ object
      [ "type" .= s "error", meta, "recoverable" .= False
      , "message" .= s "Invalid params: missing field: id", "code" .= s "-32602"
      , "error" .= object ["kind" .= s "invalid_params", "param" .= s "id", "problem" .= s "missing"]
      , "error_kind" .= s "invalid_params" ]
    itemError item `shouldBe` InvalidParams (Just "id") Missing
    itemErrorMessage item `shouldBe` Just "Invalid params: missing field: id"

  it "a 0.7.0 frame's message is the server's too" $ do
    item <- either fail pure $ decodes @PlexusStreamItem
      (object ["type" .= s "error", meta, "message" .= s "your sign-in has expired: sign in again", "code" .= s "-32001", "recoverable" .= False])
    itemError item `shouldBe` Unauthenticated
    itemErrorMessage item `shouldBe` Just "your sign-in has expired: sign in again"

  -- review §4: an error frame written here carries message, so plexus-core
  -- (whose frame requires it) reads it
  it "an error frame built here is written with its message" $ do
    let item = StreamError "h" (Provenance ["tickets"]) (Execution "boom") False
    field "message" (toJSON item) `shouldBe` Just (String "execution failed: boom")
    itemErrorMessage item `shouldBe` Nothing
    fmap itemError (decodes @PlexusStreamItem (toJSON item)) `shouldBe` Right (Execution "boom")

  it "any other typed error has no refusal" $ do
    item <- either fail pure $ decodes @PlexusStreamItem
      (object ["type" .= s "error", meta, "recoverable" .= False, "error" .= object ["kind" .= s "unauthenticated"]])
    itemRefusal item `shouldBe` Nothing

  it "a method keeps main's auth, group, examples and input beside 0.7's contract" $ do
    let wire = object
          [ "name" .= s "set_status", "description" .= s "d", "hash" .= s "h"
          , "returns" .= object [], "error" .= object ["title" .= s "SetStatusError"]
          , "cardinality" .= s "one", "bidirectional" .= False, "http_method" .= s "POST"
          , "role" .= object ["kind" .= s "rpc"]
          , "long_description" .= s "long", "grant" .= s "tickets.set_status", "target" .= s "id"
          , "held_on" .= [s "*"], "public" .= True, "group" .= s "write"
          , "examples" .= [object ["params" .= object ["id" .= s "A-1"], "note" .= s "one"]]
          , "input" .= object ["type" .= s "string"] ]
    m <- roundTrips @MethodSchema id wire
    methodGrant m `shouldBe` Just "tickets.set_status"
    methodGroup m `shouldBe` Just "write"
    methodCardinality m `shouldBe` CardinalityOne

  it "a plugin keeps main's roles and view beside 0.7's self_hash and CONTEXT-5's request" $ do
    let wire = object
          [ "namespace" .= s "tickets", "version" .= s "1", "description" .= s "d", "hash" .= s "h"
          , "self_hash" .= s "sh", "methods" .= ([] :: [Value])
          , "request" .= object ["type" .= s "object"]
          , "roles" .= object ["viewer" .= [s "tickets.get"]], "view" .= s "caller" ]
    p <- roundTrips @PluginSchema id wire
    psView p `shouldBe` Just Caller
    psSelfHash p `shouldBe` Just "sh"

  it "an anonymous view's plugin may omit its hash, version and description (AUTH-4)" $ do
    p <- either fail pure $ decodes @PluginSchema
      (object ["namespace" .= s "menger", "methods" .= ([] :: [Value]), "view" .= s "anonymous"])
    psHash p `shouldBe` ""
    psView p `shouldBe` Just Anonymous
