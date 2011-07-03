module Purecoin.Utils (showHexByteStringBE, showHexByteStringLE, integerByteStringLE) where

import Data.Word (Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.), bitSize)
import Data.Char (intToDigit)
import qualified Data.ByteString as BS

showHexByteStringBE :: BS.ByteString -> String
showHexByteStringBE = concatMap showOctet . BS.unpack

showHexByteStringLE :: BS.ByteString -> String
showHexByteStringLE = concatMap showOctet . reverse . BS.unpack

showOctet :: Word8 -> String
showOctet w = [wordToDigit (shiftR w 4), wordToDigit (0x0f .&. w)]
 where
   wordToDigit = intToDigit . fromIntegral

integerByteStringLE :: BS.ByteString -> Integer
integerByteStringLE = go . BS.unpack
 where
   go [] = 0
   go (a:l) = (toInteger a) .|. shiftL (go l) (bitSize a)