module Purecoin.Utils ( showHexByteStringBE, showHexByteStringLE
                      , integerByteStringLE, nonNegativeByteStringBE, nonNegativeByteStringLE
                      , integerToByteStringLE, integerToNByteStringLE, integerToNByteStringBE
                      ) where

import Data.Word (Word8)
import Data.Bits (Bits, shiftR, shiftL, (.|.), (.&.), xor, bitSize, testBit)
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
integerByteStringLE bs = if sgn then negate n else n
 where
   (sgn,n) = go (BS.unpack bs)
   go [] = (False, 0)
   go [w] = (testBit w 7, toInteger (0x7f .&. w))
   go (w:ws) = let (sgn0, n0) = go ws in (sgn0, (toInteger w) .|. shiftL n0 (bitSize w))

nonNegativeByteStringLE :: BS.ByteString -> Integer
nonNegativeByteStringLE = wordsToNonNegativeLE . BS.unpack

nonNegativeByteStringBE :: BS.ByteString -> Integer
nonNegativeByteStringBE = wordsToNonNegativeLE . reverse . BS.unpack

wordsToNonNegativeLE :: (Integral a, Bits a) => [a] -> Integer
wordsToNonNegativeLE = foldr f 0
 where
   f w n =  (toInteger w) .|. shiftL n (bitSize w)

integerToByteStringLE :: Integer -> BS.ByteString
integerToByteStringLE n = BS.pack $ go (if 0 <= n then 0x00 else 0x80) (abs n)
 where
  go sgn n | 0xff < n  = fromInteger n : go sgn (n `shiftR` 8)
           | 0x7f < n  = [fromInteger n, sgn]
           | 0x00 < n  = [(fromInteger n) `xor` sgn]
           | otherwise = []

-- produces a ByteString of length n
integerToNByteStringLE :: Int -> Integer -> BS.ByteString
integerToNByteStringLE n = BS.pack . take n . map (fromInteger . (.&. 0xff))
                         . iterate (`shiftR` 8)

integerToNByteStringBE :: Int -> Integer -> BS.ByteString
integerToNByteStringBE n = BS.reverse . integerToNByteStringLE n
