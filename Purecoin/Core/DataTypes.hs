module Purecoin.Core.DataTypes
       ( Difficulty, target, fromTarget
       , Hash, hash0, hashBS
       , Lock, unlocked, lockBlock, lockTime, lockView, LockView(..)
       , OutPoint, outPoint, opHash, opIndex
       , BTC(..)
       , TxInput, txiPreviousOutput, txiScript, txiFinal
       , TxOutput, txOutput, txoValue, txoScript
       ) where

import Data.Word (Word64, Word32, Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.List (unfoldr)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Serialize ( Serialize
                      , get, getWord32le, getWord64le
                      , put, putWord32le, putWord64le
                      , encode )
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Control.Monad (guard)
import Control.Applicative ((<$>),(<*>))
import Purecoin.Core.Script (Script)
import Purecoin.Digest.SHA256 (Hash256, sha256)
import Purecoin.Utils (showHexByteStringLE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Hashable as H

-- for historical reasons the 9th bit of the difficulty encoding must always be 0.
-- note: that even though one target may have multiple represenatations in this compact form,
-- comparison that blocks have the correct target is done on the compact representation.
newtype Difficulty = Difficulty Word32 deriving Eq

instance Serialize Difficulty where
  get = Difficulty <$> getWord32le

  put (Difficulty x) = putWord32le x

-- perhaps this code should be extended to support negative numbers; though they are not used
-- in bitcoin at the moment.
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

-- the Ord instance makes it useful as a key for a Map
newtype Hash = Hash Hash256 deriving (Eq, Ord)

instance Serialize Hash where
  get = Hash <$> get

  put (Hash h) = put h

-- The standard way of displaying a bitcoin hash is in little endian format.
instance Show Hash where
  show = showHexByteStringLE . encode

instance H.Hashable Hash where
  hash (Hash h) = H.hash h

-- Like all crap C programs, the 0 value is copted to have a sepearate special meaning.
hash0 :: Hash
hash0 = Hash mempty

-- For some reason in bitcoin hashing is done by two separate rounds of sha256.
-- It makes hashing slower and shortens the effectiveness of the hash by close a little less than a bit.
-- I do not know what is gained by this.
hashBS :: BS.ByteString -> Hash
hashBS = Hash . round . encode . round
 where
   round = sha256 . BSL.fromChunks . (:[])

newtype Lock = Lock Word32 deriving Eq

instance Show Lock where
  show = show . lockView

instance Serialize Lock where
  get = Lock <$> getWord32le

  put (Lock l) = putWord32le l

unlocked :: Lock
unlocked = Lock 0

lockBlockMaxValue :: Word32
lockBlockMaxValue = 500000000

lockBlock :: Integer -> Maybe Lock
lockBlock n | n < 1                           = fail "lockBlock: input too small"
            | n < toInteger lockBlockMaxValue = return . Lock . fromInteger $ n
            | otherwise                       = fail "lockBlock: input too large"

lockTime :: UTCTime -> Maybe Lock
lockTime t | wt < toInteger lockBlockMaxValue     = fail "lockTime: input too small"
           | wt <= toInteger (maxBound :: Word32) = return . Lock . fromInteger $ wt
           | otherwise                            = fail "lockTime: input too large"
  where
    wt :: Integer
    wt = round . utcTimeToPOSIXSeconds $ t

data LockView = Unlocked
              | LockBlock Integer
              | LockTime  UTCTime
              deriving Show

-- again because C doesn't have easy disjoint types we are forced to deal with nonsense encodings
-- such as this.
-- We use a view to deconstruct a Lock because we don't want people to create Locks using
-- contructors LockBlock or LockTime.  They might create out of range values.
-- Yet we still want users to be able to pattern match on locks.
lockView :: Lock -> LockView
lockView (Lock 0) = Unlocked
lockView (Lock x) | x < lockBlockMaxValue = LockBlock . toInteger $ x
                  | otherwise             = LockTime . posixSecondsToUTCTime . fromIntegral $ x

{- Ord instance is given because OutPoint are used for Keys in a Map -}
data OutPoint = OutPoint { opHash :: !Hash
                         , opIndex :: !Word32
                         } deriving (Eq, Ord, Show)

instance H.Hashable OutPoint where
  hash (OutPoint h i) = H.combine (H.hash h) (fromIntegral i)

instance Serialize OutPoint where
  get = do h <- get
           i <- getWord32le
           maybe (fail "get (OutPoint): received coinbase") return (outPoint h i)

  put (OutPoint h i) = put h >> putWord32le i

outPoint :: Hash -> Word32 -> Maybe OutPoint
outPoint h (-1) | h == hash0 = Nothing
outPoint h i                 = Just (OutPoint h i)

-- We don't want BTC to be a Num because multiplication of a BTC by another BTC doesn't make sense.
newtype BTC = Satoshi Integer deriving (Eq, Ord)

instance Monoid BTC where
  mempty = Satoshi 0
  (Satoshi x) `mappend` (Satoshi y) = Satoshi $ x + y

instance Show BTC where
  show (Satoshi x) = coins ++ "." ++ padFilings ++ filings ++ " BTC"
    where
      rawString = show x
      len = length rawString
      (coins,filings) = splitAt (8 - len) rawString
      padFilings = replicate (8 - length filings) '0'

data TxInput = TxInput { txiPreviousOutput :: OutPoint
                       , txiScript :: Script
                       , txiFinal_ :: Word32
                       } deriving Show

txiFinal :: TxInput -> Bool
txiFinal txi = txiFinal_ txi == maxBound

instance Serialize TxInput where
  get = TxInput <$> get <*> get <*> getWord32le

  put (TxInput p s sq) = put p >> put s >> putWord32le sq

data TxOutput = TxOutput { txoValue_ :: !Word64
                         , txoScript :: !Script
                         } deriving Show

txoValue :: TxOutput -> BTC
txoValue = Satoshi . toInteger . txoValue_

instance Serialize TxOutput where
  get = TxOutput <$> getWord64le <*> get

  put (TxOutput v s) = putWord64le v >> put s

txOutput :: BTC -> Script -> Maybe TxOutput
txOutput btc@(Satoshi v) s | v < 0     = fail $ "txOutput: value "++show btc++" too small"
                           | v <= toInteger (maxBound :: Word64) = return $ TxOutput (fromInteger v) s
                           | otherwise = fail $ "txOutput: value "++show btc++" too large"