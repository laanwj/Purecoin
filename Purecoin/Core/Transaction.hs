module Purecoin.Core.Transaction
       ( MakeHash, getTxInputs
       ) where

import Data.Bits (testBit, (.&.))
import Data.Word (Word8)
import Data.Maybe (fromMaybe)
import Control.Applicative ((<$>))
import Data.NEList (NEList(..), (<|), appendNE, toList)
import Data.ByteString (append)
import Purecoin.Utils (integerByteStringBE)
import Purecoin.Core.Serialize (encode, runPut, put, putWord32le)
import Purecoin.Core.Script (OP, opsScript)
import Purecoin.Core.Hash (Hash, hashBS)
import Purecoin.Core.DataTypes ( TxInput(..)
                               , GeneralizedTx(..), Tx
                               , nullOutput
                               )
import Purecoin.Core.Signature ( HashKind(..), CoinSignature
                               , csHashKind, csAnyoneCanPay, csHashType)

type MakeHash = [[OP]] -> CoinSignature -> Integer

getTxInputs :: Tx -> [(TxInput, MakeHash)]
getTxInputs tx = map result . selections . toList . txIn $ tx
  where
   selections :: [a] -> [([a],a,[a])]
   selections [] = []
   selections (x:xs) = ([],x,xs):(map (\(l,m,r) -> (x:l,m,r)) (selections xs))
   result (l, m, r) = (m, makeHash l m r)
   setScript s txi = txi{txiScript = opsScript (NENil s)}
   makeHash l m r script sig = fromMaybe 1 $ do
     tx <- newTx
     return . integerByteStringBE . encode . hashBS $ encode tx `append` csHashType sig
    where
      newTx = do out <- newOut (csHashKind sig)
                 return Tx{ txVersion = txVersion tx
                          , txIn = newIn
                          , txOut = out
                          , txLock = txLock tx
                          }
      newIn | csAnyoneCanPay sig = NENil m'
            | otherwise          = l' `appendNE` (m' <| r')
       where
        l' = map clear l
        m' = setScript (concat script) m
        r' = map clear r
      clear = clearSequence (csHashKind sig) . setScript []
      clearSequence SIGHASH_ALL txi = txi
      clearSequence _           txi = txi{txiSequence = 0}
      newOut SIGHASH_ALL    = return . toList . txOut $ tx
      newOut SIGHASH_NONE   = return []
      newOut SIGHASH_SINGLE = zipOutput l (toList . txOut $ tx)
       where
        zipOutput _      []       = fail "getTxInputs: SIGHASH_SINGLE out of range"
        zipOutput []     (out:_)  = return [out]
        zipOutput (_:ls) (_:outs) = (nullOutput:) <$> zipOutput ls outs