{-# OPTIONS_GHC -funbox-strict-fields #-}

module Purecoin.Digest.RIPEMD (ripemd160, ripemd160Ascii, Hash160) where

import Prelude hiding (pi)
import Data.Word
import Data.Bits hiding (rotate)
import Data.List
import Data.Monoid
import Data.Serialize
import Control.Monad (ap, replicateM)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Array.Unboxed as UA
import Data.Array.Unboxed (UArray, listArray, elems)
import Purecoin.Utils (showHexByteStringLE)

pad :: BSL.ByteString -> BSL.ByteString
pad = BSL.fromChunks . go 0 . BSL.toChunks
 where
   go n (bs:bss) = let n' = n + fromIntegral (BS.length bs) in n' `seq` bs:(go n' bss)
   go n [] = [BS.singleton 0x80, BS.replicate zeros 0, runPut (putWord64le (8*n))]
    where
      zeros = fromIntegral ( (-(n + 9)) `mod` 64)

data Hash160 = Hash160 {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32
                       {-# UNPACK #-} !Word32 deriving (Eq, Ord)

instance Show Hash160 where
 showsPrec _ (Hash160 a b c d e) x =
   (showHexBits a) ++ " " ++
   (showHexBits b) ++ " " ++
   (showHexBits c) ++ " " ++
   (showHexBits d) ++ " " ++
   (showHexBits e) ++ x
  where
   showHexBits = showHexByteStringLE . encode

instance Serialize Hash160 where
  get = Hash160 `fmap` getWord32le `ap` getWord32le `ap` getWord32le `ap` getWord32le `ap` getWord32le
  put (Hash160 a b c d e) = mapM_ putWord32le [a,b,c,d,e]

instance Monoid Hash160 where
  mempty = Hash160 0 0 0 0 0
  Hash160 a1 b1 c1 d1 e1 `mappend` Hash160 a2 b2 c2 d2 e2 = Hash160 (a1+a2) (b1+b2) (c1+c2) (d1+d2) (e1+e2)

newtype Buffer = Buffer (UArray Int Word32) -- 16 Word32

instance Serialize Buffer where
  get = (Buffer . listArray (0, 15)) `fmap` replicateM 16 getWord32le
  put (Buffer arr) = mapM_ putWord32le (elems arr)

(!) :: Buffer -> Int -> Word32
(Buffer arr) ! i = arr UA.! i

pi, rho :: Int -> Int
pi  i = (9*i + 5) `rem` 16
rho i = [7,4,13,1,10,6,15,3,12,0,9,5,2,14,11,8]!!i

type Stuff = (Word32 -> Word32 -> Word32 -> Word32, Word32, Int, Int)

consts :: [(Stuff,Stuff)]
consts = concat . transpose $ [zip (zip4 fs  ks  (r j)  (zipWith (!!) s (r j)))
                                   (zip4 f's k's (r' j) (zipWith (!!) s (r' j))) | j <- [0..15]]
 where
  f1 x y z = x `xor` y `xor` z
  f2 x y z = (x .&. y) .|. (complement x .&. z)
  f3 x y z = (x .|. complement y) `xor` z
  f4 x y z = (x .&. z) .|. (y .&. complement z)
  f5 x y z = x `xor` (y .|. complement z)
  fs  = [f1,f2,f3,f4,f5]
  f's = [f5,f4,f3,f2,f1]
  ks  = [0,0x5a827999,0x6ed9eba1,0x8f1bbcdc,0xa953fd4e]
  k's = [0x50a28be6,0x5c4dd124,0x6d703ef3,0x7a6d76e9,0]
  r = iterate rho
  r' = r . pi
  s  = [[11,14,15,12,5,8,7,9,11,13,14,15,6,7,9,8]       
       ,[12,13,11,15,6,9,9,7,12,15,11,13,7,8,7,7]
       ,[13,15,14,11,7,7,6,8,13,14,13,12,5,5,6,9]
       ,[14,11,12,14,8,6,5,5,15,12,15,14,9,9,8,6]
       ,[15,12,13,13,9,5,8,6,14,11,12,11,8,6,5,5]]

rotate :: Hash160 -> Hash160
rotate (Hash160 a b c d e) = Hash160 b c d e a

ripemdStep :: Hash160 -> Buffer -> Hash160
ripemdStep h m = ((rotate . rotate) endl) `mappend` (rotate h)
       `mappend` ((rotate . rotate . rotate) endr)
 where
  sideStep (f,k,r,s) (Hash160 a b c d e) = Hash160 e t b (rotateL c 10) d
    where
      t = rotateL (a + f b c d + m!r + k) s + e
  step (h1,h2) (l,r) = (sideStep l h1, sideStep r h2)
  (endl, endr) = foldl' step (h,h) consts

-----------------------------------------------------------------------------
-- | Due to the limitations of 'padding', 'ripemd160' currently requires that the
-- bitSize of @a@ divide the bitSize of @w@
-----------------------------------------------------------------------------
ripemd :: Hash160 -> BSL.ByteString -> Hash160
ripemd h bs | BSL.null bs = h 
            | otherwise   = ripemd (ripemdStep h buf) tl
  where
    Right buf = decode (BS.concat (BSL.toChunks hd))
    (hd,tl) = BSL.splitAt 64 bs

-----------------------------------------------------------------------------
-- | 'ripemd160' currently requires that the bitSize of @a@ divide 32
-----------------------------------------------------------------------------
ripemd160 :: BSL.ByteString -> Hash160
ripemd160 = ripemd (Hash160 0x67452301 0xefcdab89 0x98badcfe 0x10325476 0xc3d2e1f0)
          . pad
  
ripemd160Ascii :: String -> Hash160
ripemd160Ascii = ripemd160 . BSC.pack
