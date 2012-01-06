module Purecoin.Core.Hash
       ( Hash, hash0, hashBS, hash, merkleHash
       , Hash160, hash160BS, sha256BS, ripemd160BS
       ) where

import Data.Monoid (mempty, mappend)
import Control.Applicative ((<$>))
import Data.ByteString (ByteString)
import Data.ByteString.Lazy (fromChunks)
import qualified Data.Hashable as H
import Data.NEList (NEList(..))
import Purecoin.Digest.SHA256 (Hash256, sha256)
import Purecoin.Digest.RIPEMD (Hash160, ripemd160)
import Purecoin.Core.Serialize (Serialize, get, put, encode)
import Purecoin.Utils (showHexByteStringLE)

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
hashBS :: ByteString -> Hash
hashBS = Hash . sha256BS . encode . sha256BS

hash :: (Serialize a) => a -> Hash
hash = hashBS . encode

merkleHash :: NEList Hash -> Hash
merkleHash (NENil x) = x
merkleHash l = merkleHash (go l)
 where
  merkle :: Hash -> Hash -> Hash
  h1 `merkle` h2 = hashBS $ encode h1 `mappend` encode h2
  go :: NEList Hash -> NEList Hash
  go (NENil x) = NENil (x `merkle` x)
  go (NECons x (NENil y)) = NENil (x `merkle` y)
  go (NECons x (NECons y ys)) = NECons (x `merkle` y) (go ys)

sha256BS :: ByteString -> Hash256
sha256BS = sha256 . fromChunks . (:[])

ripemd160BS :: ByteString -> Hash160
ripemd160BS = ripemd160 . fromChunks . (:[])

hash160BS :: ByteString -> Hash160
hash160BS = ripemd160BS . encode . sha256BS