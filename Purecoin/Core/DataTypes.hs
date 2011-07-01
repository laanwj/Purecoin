module Purecoin.Core.DataTypes (Difficulty, target, fromTarget) where

import Data.Word (Word32, Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.List (unfoldr)
import Data.Serialize (Serialize, get, getWord32le, put, putWord32le)
import Control.Applicative ((<$>),(<*>))

-- for historical reasons the 9th bit of the difficulty encoding must always be 0.
-- note: that even though one target may have multiple represenatations in this compact form,
-- comparison that blocks have the correct target is done on the compact representation.
newtype Difficulty = Difficulty Word32 deriving Eq

target :: Difficulty -> Integer
target (Difficulty x) = shiftL man exp
  where
    exp :: Int
    exp = 8*(fromIntegral (shiftR x 24) - 3)
    man = toInteger (x .&. 0x00ffffff)

-- 0 <= y ==> target (fromTarget y) <= y
fromTarget :: Integer -> Difficulty
fromTarget y | y <= 0     = Difficulty 0
             | 0xff < exp = Difficulty 0xff7fffff
             | otherwise  = Difficulty (shiftL exp 24 .|. man)
 where
   octets :: [Word8]
   octets = (unfoldr f y)
    where
     f 0 = Nothing
     f z = Just (fromIntegral z, shiftR z 8)
   len = length octets + if signedIntegerNonsenseForBitcoin then 1 else 0
   -- call to last is safe because y is positive hence octets is too.
   signedIntegerNonsenseForBitcoin = (last octets .&. 0x80 == 0x80)
   exp :: Word32
   exp = fromIntegral len
   man :: Word32
   man = fromInteger (shiftR y (8*(fromIntegral exp - 3)))
  
instance Serialize Difficulty where
  get = Difficulty <$> getWord32le
  
  put (Difficulty x) = putWord32le x