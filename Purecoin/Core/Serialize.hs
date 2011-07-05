module Purecoin.Core.Serialize
       ( module Data.Serialize
       , getVarInteger, getVarByteString, getList, getNEList
       , putVarInteger, putVarByteString, putList, putNEList
       ) where

import Data.Serialize
import Data.Foldable (toList)
import Data.NEList (NEList, listToNEList)
import Control.Monad (replicateM)
import qualified Data.ByteString as BS

getVarInteger :: Get Integer
getVarInteger = go =<< getWord8
 where
  go 0xff = fromIntegral `fmap` getWord64le
  go 0xfe = fromIntegral `fmap` getWord32le
  go 0xfd = fromIntegral `fmap` getWord16le
  go x = return (fromIntegral x)

putVarInteger :: Integer -> Put
putVarInteger x | x < 0 = error "PutVarInteger: Negative"
                | x < 0xfd = putWord8 (fromIntegral x)
                | x <= 0xffff = putWord8 0xfd >> putWord16le (fromIntegral x)
                | x <= 0xffffffff = putWord8 0xfe >> putWord32le (fromIntegral x)
                | x <= 0xffffffffffffffff = putWord8 0xff >> putWord64le (fromIntegral x)
                | otherwise = error "PutVarInteger: Too Large"

getVarByteString :: Get BS.ByteString
getVarByteString = do l <- getVarInteger
                      l' <- if l < toInteger (maxBound :: Int) then return (fromInteger l)
                                                               else fail "getVarByteString too long"
                      getByteString l'

putVarByteString :: BS.ByteString -> Put
putVarByteString s = putVarInteger (toInteger (BS.length s)) >> putByteString s

getList :: Serialize a => Get [a]
getList = do len <- getVarInteger
             replicateM (fromInteger len) get

putList :: Serialize a => [a] -> Put
putList xs = putVarInteger (toInteger (length xs)) >> mapM_ put xs

getNEList :: Serialize a => Get (NEList a)
getNEList = do l <- getList
               maybe (fail "get (NEList): empty list") return (listToNEList l)

putNEList :: Serialize a => (NEList a) -> Put
putNEList = putList . toList