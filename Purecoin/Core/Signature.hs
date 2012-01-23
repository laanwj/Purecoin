module Purecoin.Core.Signature
       ( HashKind(..)
       , HashTypeView(..)
       , HashType, hashTypeView, hashTypeCode
       , CoinSignature(..)
       ) where

import Data.Word (Word8)
import Data.Bits ((.&.), testBit)
import Control.Applicative ((<$>), (<*>))
import Data.ByteString (ByteString)
import Purecoin.Core.Serialize ( Serialize, runPut
                               , get, getWord8
                               , put, putWord8, putWord32le
                               )
import Purecoin.Crypto.EcDsaSecp256k1 (Signature)

data HashKind = SIGHASH_ALL
              | SIGHASH_NONE
              | SIGHASH_SINGLE
              deriving Show

data HashTypeView = HashTypeView { htvHashKind :: HashKind 
                                 , htvAnyoneCanPay :: Bool
                                 }

newtype HashType = HashType Word8

hashTypeView :: HashType -> HashTypeView
hashTypeView (HashType w) = HashTypeView (go (w .&. 0x1f)) (testBit w 7)
 where
   go 1 = SIGHASH_ALL
   go 2 = SIGHASH_NONE
   go 3 = SIGHASH_SINGLE
   go _ = SIGHASH_ALL    -- I feel that this is an error in the protocolSubVersion

hashTypeCode :: HashType -> ByteString
hashTypeCode (HashType w) = runPut . putWord32le . fromIntegral $ w

data CoinSignature = CoinSignature { csSig :: Signature
                                   , csHashType :: HashType
                                   }

instance Serialize CoinSignature where
  get = CoinSignature <$> get <*> (HashType <$> getWord8)
  put (CoinSignature sg (HashType ht)) = put sg >> putWord8 ht