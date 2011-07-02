module Purecoin.WordArray
       ( Word8s, fromList, fromAscii, fromByteString, toByteString, Purecoin.WordArray.length
       ) where

import Data.Word (Word8)
import Data.Array.Unboxed (UArray, listArray, elems, bounds)
import qualified Data.ByteString as BS
import Purecoin.Utils (showHexByteStringLE)

newtype Word8s = Word8s { unWord8s :: UArray Int Word8 } deriving (Eq)

instance Show Word8s where
  show = showHexByteStringLE . toByteString

fromList :: [Word8] -> Word8s
fromList l = Word8s $ listArray (1, (Prelude.length l)) l

fromAscii :: [Char] -> Word8s
fromAscii = fromList . map (toEnum . fromEnum)

fromByteString :: BS.ByteString -> Word8s
fromByteString = fromList . BS.unpack

toByteString :: Word8s -> BS.ByteString
toByteString = BS.pack . elems . unWord8s

length :: Word8s -> Integer
length = toInteger . snd . bounds . unWord8s