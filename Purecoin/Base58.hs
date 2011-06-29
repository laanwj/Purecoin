module Purecoin.Base58 (decodeBase58, encodeBase58) where

import Data.List (elemIndex)
import Data.Maybe (fromJust)
import Numeric (readInt, showIntAtBase)

base58Code = "123456789ABCDEFGHJKLMNPQRSTUVWXYZabcdefghijkmnopqrstuvwxyz"

-- The use of fromJust here is safe because of (`elem` base58Code).
-- The real problem is that the interface to readInt is poor.
decodeBase58 :: ReadS Integer
decodeBase58 = readInt (fromIntegral . length $ base58Code) 
                       (`elem` base58Code)
                       (fromJust . (`elemIndex` base58Code))
                      
encodeBase58 :: Integer -> ShowS
encodeBase58 = showIntAtBase (fromIntegral . length $ base58Code) 
                             (base58Code!!)