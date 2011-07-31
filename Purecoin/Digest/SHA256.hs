{-# OPTIONS_GHC -funbox-strict-fields #-}
-- Implements SHA-256 as defined in FIPS 180-2
-- <http://csrc.nist.gov/publications/fips/fips180-2/fips180-2withchangenotice.pdf>.
module Purecoin.Digest.SHA256 (sha256, sha256Ascii, Hash256) where

import Data.Word
import Data.Bits
import Data.List
import Data.Monoid
import Data.Serialize
import Control.Monad (ap, replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Hashable as H
import Purecoin.Utils (showHexByteStringBE)

ch, maj :: Word32 -> Word32 -> Word32 -> Word32
ch x y z = (x .&. y) `xor` (complement x .&. z)
maj x y z = (x .&. y) `xor` (x .&. z) `xor` (y .&. z)

bigSigma0, bigSigma1, smallSigma0, smallSigma1 :: Word32 -> Word32
bigSigma0 x = rotateR x 2 `xor` rotateR x 13 `xor` rotateR x 22
bigSigma1 x = rotateR x 6 `xor` rotateR x 11 `xor` rotateR x 25
smallSigma0 x = rotateR x 7 `xor` rotateR x 18 `xor` shiftR x 3
smallSigma1 x = rotateR x 17 `xor` rotateR x 19 `xor` shiftR x 10
ks = [0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4, 0xab1c5ed5
     ,0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe, 0x9bdc06a7, 0xc19bf174
     ,0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f, 0x4a7484aa, 0x5cb0a9dc, 0x76f988da
     ,0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7, 0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967
     ,0x27b70a85, 0x2e1b2138, 0x4d2c6dfc, 0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85
     ,0xa2bfe8a1, 0xa81a664b, 0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070
     ,0x19a4c116, 0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3
     ,0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7, 0xc67178f2]

pad :: BSL.ByteString -> BSL.ByteString
pad = BSL.fromChunks . go 0 . BSL.toChunks
 where
   go n (bs:bss) = let n' = n + BS.length bs in n' `seq` bs:(go n' bss)
   go n [] = [BS.singleton 0x80, BS.replicate zeros 0, encode (8*n)]
    where
      zeros = fromIntegral ( (-(n + 9)) `mod` 64)

data Hash256 = Hash256 {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32 deriving (Eq, Ord)

instance H.Hashable Hash256 where
  hash (Hash256 a b c d e f g h) = fromIntegral h

instance Show Hash256 where
 showsPrec _ (Hash256 a b c d e f g h) x =
   (showHexBits a) ++ " " ++
   (showHexBits b) ++ " " ++
   (showHexBits c) ++ " " ++
   (showHexBits d) ++ " " ++
   (showHexBits e) ++ " " ++
   (showHexBits f) ++ " " ++
   (showHexBits g) ++ " " ++
   (showHexBits h) ++ x
  where
   showHexBits = showHexByteStringBE . encode

instance Serialize Hash256 where
  get = Hash256 `fmap` getWord32be `ap` getWord32be `ap` getWord32be `ap` getWord32be
                `ap` getWord32be `ap` getWord32be `ap` getWord32be `ap` getWord32be
  put (Hash256 a b c d e f g h) = mapM_ putWord32be [a,b,c,d,e,f,g,h]

instance Serialize Buffer where
  get = Buffer `fmap` get `ap` get
  put (Buffer l r) = put [l,r]

instance Monoid Hash256 where
  mempty = Hash256 0 0 0 0 0 0 0 0
  (Hash256 x0 x1 x2 x3 x4 x5 x6 x7) `mappend` (Hash256 y0 y1 y2 y3 y4 y5 y6 y7) =
    Hash256 (x0+y0) (x1+y1) (x2+y2) (x3+y3) (x4+y4) (x5+y5) (x6+y6) (x7+y7)

data Buffer = Buffer {-# UNPACK #-} !Hash256
                     {-# UNPACK #-} !Hash256

shaStep :: Hash256 -> Buffer -> Hash256
shaStep h m = go ks m h `mappend` h
 where
  go [] _ h  = h
  go (k:ks) (Buffer (Hash256 a0 a1 a2 a3 a4 a5 a6 a7) (Hash256 a8 a9 aa ab ac ad ae af)) h =
    go ks (Buffer (Hash256 a1 a2 a3 a4 a5 a6 a7 a8) (Hash256 a9 aa ab ac ad ae af ag)) h'
   where
    h' = mkStep3 k a0 h
    ag = smallSigma ae a9 a1 a0
    smallSigma a b c d = smallSigma1 a + b + smallSigma0 c + d
    mkStep3 k w (Hash256 a b c d e f g h) = Hash256 (t1+t2) a b c (d+t1) e f g
     where
      t1 = h + bigSigma1 e + ch e f g + k + w
      t2 = bigSigma0 a + maj a b c

-----------------------------------------------------------------------------
-- | Due to the limitations of 'padding', 'sha' currently requires that the
-- bitSize of @a@ divide the bitSize of @w@
-----------------------------------------------------------------------------
sha :: Hash256 -> BSL.ByteString -> Hash256
sha h bs  | BSL.null bs = h
          | otherwise = sha (shaStep h buf) tl
     where
       Right buf = decode (BS.concat (BSL.toChunks hd))
       (hd,tl) = BSL.splitAt 64 bs

-----------------------------------------------------------------------------
-- | 'sha256' currently requires that the bitSize of @a@ divide 32
-----------------------------------------------------------------------------
sha256 :: BSL.ByteString -> Hash256
sha256 = sha
    (Hash256 0x6a09e667 0xbb67ae85 0x3c6ef372 0xa54ff53a 0x510e527f 0x9b05688c 0x1f83d9ab 0x5be0cd19)
  . pad

-----------------------------------------------------------------------------
-- ** Hashing Strings
-- | @shaXXXAscii@ assumes that all characters of the strings are
-- ISO-latin-1 characters.  ie. each characters fits in one octet.
-----------------------------------------------------------------------------
sha256Ascii :: String -> Hash256
sha256Ascii = sha256 . BSC.pack
