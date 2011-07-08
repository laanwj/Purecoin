module Purecoin.Core.DataTypes
       ( Hash, hash0, hashBS, hash, merkleHash
       , Difficulty, target, fromTarget, hashMeetsTarget
       , Lock, unlocked, lockBlock, lockTime, lockView, LockView(..)
       , OutPoint, outPoint, opHash, opIndex
       , BTC(..), satoshi, scale
       , TxInput, txiPreviousOutput, txiScript, txiFinal
       , TxOutput, txOutput, txoValue, txoScript
       , Tx(..)
       , TxCoinBase, txCoinBase, txcbVersion, txcbExtraNonce, txcbFinal, txcbOut, txcbLock
       , Block, block, bVersion, bPrevBlock, bMerkle_root, bTimestamp, bBits, bCoinBase, bTxs, bHash
       ) where

import Data.Word (Word64, Word32, Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.List (unfoldr)
import Data.NEList (NEList(..), (<|))
import Data.Monoid (Monoid, mempty, mappend)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Fixed (Fixed, HasResolution, resolution)
import Control.Monad (guard, unless, replicateM)
import Control.Applicative ((<$>),(<*>))
import Purecoin.Core.Serialize ( Serialize, Get
                               , get, getWord32le, getWord64le, getVarInteger, getNEList
                               , put, putWord32le, putWord64le, putVarInteger, putNEList
                               , encode, runPut )
import Purecoin.Core.Script (Script)
import Purecoin.Digest.SHA256 (Hash256, sha256)
import Purecoin.Utils (showHexByteStringLE, integerByteStringLE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Hashable as H

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

hash :: (Serialize a) => a -> Hash
hash = hashBS . encode

merkleHash :: NEList Hash -> Hash
merkleHash (NENil x) = x
merkleHash l = merkleHash (go l)
 where
  merkle :: Hash -> Hash -> Hash
  h1 `merkle` h2 = hashBS $ encode h1 `BS.append` encode h2
  go :: NEList Hash -> NEList Hash
  go (NENil x) = NENil (x `merkle` x)
  go (NECons x (NENil y)) = NENil (x `merkle` y)
  go (NECons x (NECons y l)) = NECons (x `merkle` y) (go l)

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

hashMeetsTarget :: Hash -> Difficulty -> Bool
hashMeetsTarget h d = hi <= target d
  where
    hi = integerByteStringLE . encode $ h

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

newtype E8 = E8 E8

instance HasResolution E8 where
  resolution _ = 100000000

-- We don't want BTC to be a Num because multiplication of a BTC by another BTC doesn't make sense.
newtype BTC = BTC (Fixed E8) deriving (Eq, Ord, Show)

instance Monoid BTC where
  mempty = BTC 0
  (BTC x) `mappend` (BTC y) = BTC $ x + y

satoshi :: BTC
satoshi = BTC (succ 0)

scale :: Integer -> BTC -> BTC
n `scale` (BTC x) = BTC $ fromInteger n * x

data TxInput = TxInput { txiPreviousOutput :: OutPoint
                       , txiScript :: Script
                       , txiFinal_ :: Word32
                       } deriving Show

txiFinal :: TxInput -> Bool
txiFinal txi = txiFinal_ txi == maxBound

instance Serialize TxInput where
  get = TxInput <$> get <*> get <*> getWord32le

  put (TxInput p s f) = put p >> put s >> putWord32le f

data TxOutput = TxOutput { txoValue_ :: !Word64 -- bitcoin uses an Int64, but it doesn't really matter.
                         , txoScript :: !Script
                         } deriving Show

txoValue :: TxOutput -> BTC
txoValue = (`scale` satoshi) . toInteger . txoValue_

instance Serialize TxOutput where
  get = TxOutput <$> getWord64le <*> get

  put (TxOutput v s) = putWord64le v >> put s

txOutput :: BTC -> Script -> Maybe TxOutput
txOutput (BTC btc) s | v < 0     = fail $ "txOutput: value "++show btc++" too small"
                     | v <= toInteger (maxBound :: Word64) = return $ TxOutput (fromInteger v) s
                     | otherwise = fail $ "txOutput: value "++show btc++" too large"
 where
  (BTC tiny) = satoshi
  v = floor (btc / tiny)

data Tx = Tx { txVersion :: Word32 -- I think the txVersion should be restricted to 1, but this isn't how bitcoin works.
             , txIn :: NEList TxInput
             , txOut :: NEList TxOutput
             , txLock :: Lock
             } deriving Show

-- The bitcoin client checks to see that all the txiPreviousOutput's are not hash0.
-- This check isn't really needed.
-- It is extremely unlikely that any Tx referenceing hash0 will make it into the block chain.
instance Serialize Tx where
  get = Tx <$> getWord32le <*> getNEList <*> getNEList <*> get

  put (Tx v is os t) = putWord32le v >> putNEList is >> putNEList os >> put t

data TxCoinBase = TxCoinBase { txcbVersion :: Word32
                             , txcbExtraNonce :: Script
                             , txcbFinal_ :: Word32
                             , txcbOut :: NEList TxOutput
                             , txcbLock :: Lock
                             } deriving Show

instance Serialize TxCoinBase where
  get = do v <- getWord32le
           1 <- getVarInteger
           h <- get
           guard (h == hash0)
           (-1) <- getWord32le -- the null output index
           en <- get
           f <- getWord32le
           os <- getNEList
           t <- get
           return $ TxCoinBase v en f os t

  put (TxCoinBase v en sq os t) = putWord32le v >> putVarInteger 1 >> put hash0
                               >> putWord32le (-1) >>  put en >> putWord32le sq
                               >> putNEList os >> put t

txcbFinal :: TxCoinBase -> Bool
txcbFinal txcb = txcbFinal_ txcb == maxBound

-- only builds coinbases that are final and unlocked
txCoinBase :: Word32 -> Script -> NEList TxOutput -> TxCoinBase
txCoinBase v en os = TxCoinBase v en maxBound os unlocked

-- This should be split into a block header (probably with cached merkle root) and transactions.
data Block = Block { bVersion :: Word32 -- unused
                   , bPrevBlock :: Hash
                   , bTimestamp_ :: Word32
                   , bBits :: Difficulty
                   , bNonce :: Word32
                   , bCoinBase :: TxCoinBase
                   , bTxs :: [Tx]
                   }

instance Serialize Block where
  get = do
         v <- getWord32le
         pb <- get
         get :: Get Hash -- ignore the serialized merkle root; we will compute it ourselves
         t <- getWord32le
         b <- get
         n <- getWord32le
         len <- getVarInteger
         unless (len < toInteger (maxBound :: Int))
                (fail $ "get (Block) transaction list too long")
         (cb,txs) <- if len == 0 then fail "get (Block): found only block header"
                                 else (,) <$> get <*> replicateM (fromInteger len - 1) get
         makeBlock v pb t b n cb txs

  put bl@(Block v pb t b n cb txs) =
    putWord32le v >> put pb >> put (bMerkle_root bl) >>
    putWord32le t >> put b >> putWord32le n >>
    putVarInteger (toInteger (length txs) + 1) >> put cb >> mapM_ put txs

bTimestamp :: Block -> UTCTime
bTimestamp = posixSecondsToUTCTime . fromIntegral . bTimestamp_

bHash :: Block -> Hash
bHash bl@(Block v pb t b n _ _) =
  hashBS . runPut $ putWord32le v >> put pb >> put (bMerkle_root bl) >>
                    putWord32le t >> put b >> putWord32le n

bMerkle_root :: Block -> Hash
bMerkle_root b = merkleHash $ hash (bCoinBase b) <| map hash (bTxs b)

block :: Word32 -> Hash -> UTCTime -> Difficulty -> Word32 -> TxCoinBase -> [Tx] -> Maybe Block
block v pb t' b n cb txs = do
  guard ((0 <= t) && (t <= toInteger (maxBound :: Word32)))
  makeBlock v pb (fromInteger t) b n cb txs
 where
  t :: Integer
  t = round . utcTimeToPOSIXSeconds $ t'

makeBlock :: (Monad m) => Word32 -> Hash -> Word32 -> Difficulty -> Word32 -> TxCoinBase -> [Tx] ->
                          m Block
makeBlock v pb t b n cb txs = unless (hashMeetsTarget (bHash blk) b) (fail $ "block : Target not met")
                           >> return blk
 where
  blk = Block v pb t b n cb txs
