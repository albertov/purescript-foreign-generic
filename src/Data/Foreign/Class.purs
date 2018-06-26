module Data.Foreign.Class where

import Prelude

import Control.Monad.Except (mapExcept, throwError)
import Data.Array ((..), zipWith, length, index, null)
import Data.Bifunctor (lmap)
import Data.Const (Const(..))
import Data.Foreign (F, Foreign, ForeignError(ErrorAtIndex, ForeignError), isNull, isUndefined, readArray, readBoolean, readChar, readInt, readNumber, readString, toForeign)
import Data.Foreign.Internal (readStrMap)
import Data.Foreign.NullOrUndefined (NullOrUndefined(..), readNullOrUndefined, undefined)
import Data.List.NonEmpty as NEL
import Data.Maybe (Maybe, maybe)
import Data.StrMap as StrMap
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))

-- | The `Decode` class is used to generate decoding functions
-- | of the form `Foreign -> F a` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericDecode` and `genericDecodeJSON` functions
-- | to decode your foreign/JSON-encoded data.
class Decode a where
  decode :: Foreign -> F a

instance foreignDecode :: Decode Foreign where
  decode = pure

instance stringDecode :: Decode String where
  decode = readString

instance charDecode :: Decode Char where
  decode = readChar

instance booleanDecode :: Decode Boolean where
  decode = readBoolean

instance numberDecode :: Decode Number where
  decode = readNumber

instance intDecode :: Decode Int where
  decode = readInt

instance arrayDecode :: Decode a => Decode (Array a) where
  decode = readArray >=> readElements where
    readElements :: Array Foreign -> F (Array a)
    readElements arr = sequence (zipWith readElement (0 .. length arr) arr)

    readElement :: Int -> Foreign -> F a
    readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance strMapDecode :: (Decode v) => Decode (StrMap.StrMap v) where
  decode = sequence <<< StrMap.mapWithKey (\_ -> decode) <=< readStrMap

instance decodeTuple :: (Decode a, Decode b) => Decode (Tuple a b) where
  decode v = readArray v >>= \ arr -> do
    a <- maybe (noIndexError 0) (readElement 0) (arr `index` 0)
    b <- maybe (noIndexError 1) (readElement 1) (arr `index` 1)
    pure (Tuple a b)
    where
      noIndexError :: forall x. Int -> F x
      noIndexError i = throwError $ NEL.singleton $
                       ForeignError ("Expected an element at index " <> show i)
      readElement :: forall x. Decode x => Int -> Foreign -> F x
      readElement i value = mapExcept (lmap (map (ErrorAtIndex i))) (decode value)

instance decodeUnit :: Decode Unit where
  decode x = readArray x >>= \arr ->
              if null arr
              then pure unit
              else throwError $ NEL.singleton $
                   ForeignError "Expected an empty array"

instance constDecode :: Decode a => Decode (Const a b) where
  decode = map Const <<< decode

-- | The `Encode` class is used to generate encoding functions
-- | of the form `a -> Foreign` using `generics-rep` deriving.
-- |
-- | First, derive `Generic` for your data:
-- |
-- | ```purescript
-- | import Data.Generic.Rep
-- |
-- | data MyType = MyType ...
-- |
-- | derive instance genericMyType :: Generic MyType _
-- | ```
-- |
-- | You can then use the `genericEncode` and `genericEncodeJSON` functions
-- | to encode your data as JSON.
class Encode a where
  encode :: a -> Foreign

instance foreignEncode :: Encode Foreign where
  encode = id

instance stringEncode :: Encode String where
  encode = toForeign

instance charEncode :: Encode Char where
  encode = toForeign

instance booleanEncode :: Encode Boolean where
  encode = toForeign

instance numberEncode :: Encode Number where
  encode = toForeign

instance intEncode :: Encode Int where
  encode = toForeign

instance arrayEncode :: Encode a => Encode (Array a) where
  encode = toForeign <<< map encode

instance decodeNullOrUndefined :: Decode a => Decode (NullOrUndefined a) where
  decode = readNullOrUndefined decode

instance encodeNullOrUndefined :: Encode a => Encode (NullOrUndefined a) where
  encode (NullOrUndefined a) = maybe undefined encode a

instance decodeMaybe :: Decode a => Decode (Maybe a) where
  decode v = do NullOrUndefined v' <- readNullOrUndefined decode v
                pure v'

instance encodeMaybe :: Encode a => Encode (Maybe a) where
  encode = maybe undefined encode

instance strMapEncode :: Encode v => Encode (StrMap.StrMap v) where 
  encode = toForeign <<< StrMap.mapWithKey (\_ -> encode)


instance encodeTuple :: (Encode a, Encode b) => Encode (Tuple a b) where
  encode (Tuple a b) = toForeign [encode a, encode b]

instance encodeUnit :: Encode Unit where
  encode _ = toForeign []

instance encodeConst :: Encode a => Encode (Const a b) where
  encode (Const a) = encode a
