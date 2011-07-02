module Purecoin.Core.DataTypes
       ( Difficulty, target, fromTarget
       , Hash, hash0, hashBS
       ) where

import Data.Word (Word32, Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.List (unfoldr)
import Data.Monoid (mempty)
import Data.Serialize (Serialize, get, getWord32le, put, putWord32le, encode)
import Control.Applicative ((<$>),(<*>))
import Purecoin.Digest.SHA256 (Hash256, sha256)
import Purecoin.Utils (showHexByteStringLE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Hashable as H

-- for historical reasons the 9th bit of the difficulty encoding must always be 0.
-- note: that even though one target may have multiple represenatations in this compact form,
-- comparison that blocks have the correct target is done on the compact representation.
newtype Difficulty = Difficulty Word32 deriving Eq

instance Serialize Difficulty where
  get = Difficulty <$> getWord32le

  put (Difficulty x) = putWord32le x

-- perhaps this code should be extended to support negative numbers; though they are not used
-- in bitcoin at the moment.
target :: Difficulty -> Integer
target (Difficulty x) = shiftL man exp
  where
    exp :: Int
    exp = 8*(fromIntegral (shiftR x 24) - 3)
    man = toInteger (x .&. 0x00ffffff)

-- 0 <= y ==> target (fromTarget y) <= y
fromTarget :: Integer -> Difficulty
fromTarget y | y <= 0     = Difficulty 0
             | 0xff < exp = Difficulty 0xff7fffff
             | otherwise  = Difficulty (shiftL exp 24 .|. man)
 where
   octets :: [Word8]
   octets = (unfoldr f y)
    where
     f 0 = Nothing
     f z = Just (fromIntegral z, shiftR z 8)
   len = length octets + if signedIntegerNonsenseForBitcoin then 1 else 0
   -- call to last is safe because y is positive hence octets is too.
   signedIntegerNonsenseForBitcoin = (last octets .&. 0x80 == 0x80)
   exp :: Word32
   exp = fromIntegral len
   man :: Word32
   man = fromInteger (shiftR y (8*(fromIntegral exp - 3)))

-- the Ord instance makes it useful as a key for a Map
newtype Hash = Hash Hash256 deriving (Eq, Ord)

instance Serialize Hash where
  get = Hash <$> get

  put (Hash h) = put h

-- The standard way of displaying a bitcoin hash is in little endian format.
instance Show Hash where
  show = showHexByteStringLE . encode

instance H.Hashable Hash where
  hash (Hash h) = H.hash h

-- Like all crap C programs, the 0 value is copted to have a sepearate special meaning.
hash0 :: Hash
hash0 = Hash mempty

-- For some reason in bitcoin hashing is done by two separate rounds of sha256.
-- It makes hashing slower and shortens the effectiveness of the hash by close a little less than a bit.
-- I do not know what is gained by this.
hashBS :: BS.ByteString -> Hash
hashBS = Hash . round . encode . round
 where
   round = sha256 . BSL.fromChunks . (:[])
