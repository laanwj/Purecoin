module Purecoin.Core.Signature
       ( HashKind(..)
       , CoinSignature, csSig, csHashKind, csAnyoneCanPay, putHashType
       ) where

import Data.Word (Word8)
import Data.Bits ((.&.), testBit)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString, pack)
import Purecoin.Core.Serialize ( Serialize, runPut
                               , get, getWord8
                               , put, putWord8, putWord32le
                               )
import Purecoin.Crypto.EcDsaSecp256k1

data HashKind = SIGHASH_ALL
              | SIGHASH_NONE
              | SIGHASH_SINGLE
              deriving Show

data CoinSignature = CoinSignature { csSig :: Signature
                                   , csHashType_ :: Word8
                                   } deriving Show

csAnyoneCanPay :: CoinSignature -> Bool
csAnyoneCanPay cs = testBit (csHashType_ cs) 7

csHashKind :: CoinSignature -> HashKind
csHashKind cs = go (csHashType_ cs .&. 0x1f)
 where
   go 1 = SIGHASH_ALL
   go 2 = SIGHASH_NONE
   go 3 = SIGHASH_SINGLE
   go _ = SIGHASH_ALL    -- I feel that this is an error in the protocolSubVersion

instance Serialize CoinSignature where
  get = CoinSignature <$> get <*> getWord8
  put (CoinSignature sg ht) = put sg >> putWord8 ht

putHashType :: CoinSignature -> ByteString
putHashType = runPut . putWord32le . fromIntegral . csHashType_