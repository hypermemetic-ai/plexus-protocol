-- | BIDIR-18: a call's input half, as plexus-wire spec/10 draws it.
--
-- A method that declares @input@ beside its @params@ takes a stream in
-- beside its stream out. The client sends
--
-- @
-- → {"jsonrpc":"2.0","method":"<ns>.input","params":{"subscription":S,"seq":N,"items":[…]}}
-- → [0x01][S: u64 BE][N: u64 BE][payload]          (a binary frame, one item: BIDIR-17)
-- → {"jsonrpc":"2.0","method":"<ns>.end","params":{"subscription":S,"seq":FRAMES}}
-- @
--
-- under the credit the callee grants with its output items @credit {n}@ (items),
-- and is told what it consumed with @ack {seq}@ (frames) and when it stopped
-- reading with @input_closed {reason, ack?}@. Those three are the
-- transport's, never a 'Plexus.Types.PlexusStreamItem': 'inputSignal' reads
-- them off a notification's @result@ before anything parses it as an item.
--
-- There is no resume (spec/10): a call dies with its socket, and nothing
-- here sends a frame twice.
module Plexus.Input
  ( -- * What the callee says of the input half
    InputSignal(..)
  , inputSignal
  , inputSignalValue

    -- * Where a call's input goes
  , InputPort(..)
  , InputHalf(..)

    -- * Frames
  , inputFrame
  , endFrame
  , binaryFrame
  , binaryHeader
  , subscriptionValue
  , subscriptionWord
  , inputNamespace

    -- * An item schema
  , ItemKind(..)
  , itemKind
  ) where

import Data.Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as LBS
import Data.Scientific (toBoundedInteger)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Word (Word64)

import Plexus.Types (SubscriptionId(..))

-- | One of the input half's output items (spec/10).
data InputSignal
  = InputCredit Integer
    -- ^ @credit {n}@: the callee takes @n@ more items
  | InputAck Integer
    -- ^ @ack {seq}@: every frame through @seq@ is consumed
  | InputClosed Text (Maybe Integer)
    -- ^ @input_closed {reason, ack?}@: the callee reads no more —
    -- @method_ended@, or the @error_kind@ of the refusal that follows
  deriving stock (Show, Eq)

-- | The signal a notification's @result@ is, if it is one.
inputSignal :: Value -> Maybe InputSignal
inputSignal = \case
  Object o -> case KM.lookup "type" o of
    Just (String "credit") -> InputCredit <$> int "n" o
    Just (String "ack") -> InputAck <$> int "seq" o
    Just (String "input_closed") -> Just $ InputClosed
      (case KM.lookup "reason" o of { Just (String r) -> r; _ -> "" })
      (int "ack" o)
    _ -> Nothing
  _ -> Nothing
  where
    int k o = case KM.lookup k o of
      Just (Number n) -> toInteger <$> (toBoundedInteger n :: Maybe Int)
      _ -> Nothing

-- | The signal as the callee writes it: 'inputSignal' reads it back.
inputSignalValue :: InputSignal -> Value
inputSignalValue = \case
  InputCredit n -> object ["type" .= ("credit" :: Text), "n" .= n]
  InputAck sq -> object ["type" .= ("ack" :: Text), "seq" .= sq]
  InputClosed reason ack -> object $
    ["type" .= ("input_closed" :: Text), "reason" .= reason] ++ [ "ack" .= a | Just a <- [ack] ]

-- | A call's input, once its subscription is known: each frame built and
-- sent on the connection that holds the call (spec/10: only that socket
-- may send it). The caller numbers the frames; a send that fails throws
-- as the connection's loss does.
data InputPort = InputPort
  { ipItems  :: Word64 -> [Value] -> IO ()        -- ^ @<ns>.input {subscription, seq, items}@
  , ipBinary :: Word64 -> BS.ByteString -> IO ()  -- ^ a binary frame of one item
  , ipEnd    :: Word64 -> IO ()                   -- ^ @<ns>.end {subscription, seq}@
  }

-- | What a call with an input half hands its route: where the port goes
-- when the subscription is known (before the call is said to be taken),
-- and where the callee's signals go as they come.
data InputHalf = InputHalf
  { ihPort   :: InputPort -> IO ()
  , ihSignal :: InputSignal -> IO ()
  }

-- | The subscription id as the server gave it (a number stays a number).
subscriptionValue :: SubscriptionId -> Value
subscriptionValue (SubscriptionId raw) = case decodeStrict (TE.encodeUtf8 raw) of
  Just n@(Number _) -> n
  _ -> String raw

-- | The subscription id as a binary frame carries it: an unsigned 64-bit
-- integer, or Nothing for an id that is not one.
subscriptionWord :: SubscriptionId -> Maybe Word64
subscriptionWord sid = case subscriptionValue sid of
  Number n -> toBoundedInteger n :: Maybe Word64
  _ -> Nothing

-- | The namespace a call's frames are addressed to: @menger@ for a call
-- made as @menger.call@.
inputNamespace :: Text -> Text
inputNamespace method = maybe method id (T.stripSuffix ".call" method)

-- | @{"jsonrpc":"2.0","method":"<ns>.input","params":{"subscription":S,"seq":N,"items":[…]}}@
inputFrame :: Text -> SubscriptionId -> Word64 -> [Value] -> LBS.ByteString
inputFrame ns sid sq items = encode $ object
  [ "jsonrpc" .= ("2.0" :: Text), "method" .= (ns <> ".input")
  , "params" .= object ["subscription" .= subscriptionValue sid, "seq" .= sq, "items" .= items] ]

-- | @{"jsonrpc":"2.0","method":"<ns>.end","params":{"subscription":S,"seq":FRAMES}}@
endFrame :: Text -> SubscriptionId -> Word64 -> LBS.ByteString
endFrame ns sid sq = encode $ object
  [ "jsonrpc" .= ("2.0" :: Text), "method" .= (ns <> ".end")
  , "params" .= object ["subscription" .= subscriptionValue sid, "seq" .= sq] ]

-- | BIDIR-17: @[0x01][subscription: u64 BE][seq: u64 BE]@ — the 17 bytes
-- before a binary frame's payload.
binaryHeader :: Word64 -> Word64 -> BS.ByteString
binaryHeader sub sq = LBS.toStrict $ B.toLazyByteString $
  B.word8 0x01 <> B.word64BE sub <> B.word64BE sq

-- | A whole binary input frame.
binaryFrame :: Word64 -> Word64 -> BS.ByteString -> BS.ByteString
binaryFrame sub sq payload = binaryHeader sub sq <> payload

-- | How a method's input items are written, from the item's schema.
data ItemKind
  = ItemBytes   -- ^ @"x-plexus-binary": true@ (@Input<Bytes>@): raw bytes, binary frames
  | ItemString  -- ^ a string: a line is an item
  | ItemJson    -- ^ anything else: one JSON value
  deriving stock (Show, Eq)

-- | The item schema's kind. A @$ref@ is followed into the schema's own
-- @$defs@ / @definitions@ once.
itemKind :: Value -> ItemKind
itemKind v = case v of
  Object o
    | KM.lookup "x-plexus-binary" o == Just (Bool True) -> ItemBytes
    | KM.lookup "type" o == Just (String "string") -> ItemString
    | Just (String ref) <- KM.lookup "$ref" o
    , Just target <- deref o ref -> itemKind target
  _ -> ItemJson
  where
    deref o ref = do
      name <- T.stripPrefix "#/$defs/" ref <> T.stripPrefix "#/definitions/" ref
      defs <- case (KM.lookup "$defs" o, KM.lookup "definitions" o) of
        (Just (Object d), _) -> Just d
        (_, Just (Object d)) -> Just d
        _ -> Nothing
      t <- KM.lookup (K.fromText name) defs
      -- a $ref to another $ref is not followed again
      case t of
        Object to | KM.member "$ref" to -> Nothing
        _ -> Just t

