module Purecoin.Core.DataTypes
       ( Difficulty, target, fromTarget, hashMeetsTarget
       , Lock, unlocked, lockBlock, lockTime, lockView, LockView(..)
       , OutPoint, outPoint, opHash, opIndex
       , BTC(..), btc, satoshi, scale
       , TxInput(..)
       , TxOutput, txOutput, txoValue, txoScript, nullOutput
       , GeneralizedTx(..), Tx
       , TxCoinBase, txCoinBase, txcbVersion, txcbExtraNonce, txcbFinal, txcbOut, txcbLock
       , Block, block, bVersion, bPrevBlock, bMerkle_root, bTimestamp, bBits, bNonce, bCoinBase, bTxs, bHash
       ) where

import Data.Word (Word64, Word32, Word8)
import Data.Bits (shiftR, shiftL, (.|.), (.&.))
import Data.List (unfoldr)
import Data.NEList (NEList(..), (<|))
import Data.Foldable (Foldable, toList)
import Data.Monoid (Monoid, mempty, mappend)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.Fixed (Fixed, HasResolution, resolution)
import Control.Monad (guard, unless, replicateM)
import Control.Applicative ((<$>),(<*>))
import qualified Data.Hashable as H
import Purecoin.Core.Serialize ( Serialize, Get, FromList
                               , get, getWord32le, getWord64le, getVarInteger, getList
                               , put, putWord32le, putWord64le, putVarInteger, putList
                               , encode, runPut )
import Purecoin.Core.Hash (Hash, hash0, hash, hashBS, merkleHash)
import Purecoin.Core.Script (Script, nullScript)
import Purecoin.Utils (nonNegativeByteStringLE)

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
target (Difficulty x) = shiftL mant expo
  where
    expo :: Int
    expo = 8*(fromIntegral (shiftR x 24) - 3)
    mant = toInteger (x .&. 0x00ffffff)

-- 0 <= y ==> target (fromTarget y) <= y
fromTarget :: Integer -> Difficulty
fromTarget y | y <= 0      = Difficulty 0
             | 0xff < expo = Difficulty 0xff7fffff
             | otherwise   = Difficulty (shiftL expo 24 .|. mant)
 where
   octets :: [Word8]
   octets = (unfoldr f y)
    where
     f 0 = Nothing
     f z = Just (fromIntegral z, shiftR z 8)
   len = length octets + if signedIntegerNonsenseForBitcoin then 1 else 0
   -- call to last is safe because y is positive hence octets is too.
   signedIntegerNonsenseForBitcoin = (last octets .&. 0x80 == 0x80)
   expo :: Word32
   expo = fromIntegral len
   mant :: Word32
   mant = fromInteger (shiftR y (8*(fromIntegral expo - 3)))

hashMeetsTarget :: Hash -> Difficulty -> Bool
hashMeetsTarget h d = hi <= target d
  where
    hi = nonNegativeByteStringLE . encode $ h

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
newtype BTC = Ƀ (Fixed E8) deriving (Eq, Ord, Show)

instance Monoid BTC where
  mempty = Ƀ 0
  (Ƀ x) `mappend` (Ƀ y) = Ƀ (x + y)

btc :: Fixed E8 -> BTC
btc = Ƀ

satoshi :: BTC
satoshi = Ƀ (succ 0)

scale :: Integer -> BTC -> BTC
n `scale` (Ƀ x) = Ƀ (fromInteger n * x)

data TxInput = TxInput { txiPreviousOutput :: OutPoint
                       , txiScript :: Script
                       , txiSequence :: Word32
                       } deriving Show

instance Serialize TxInput where
  get = TxInput <$> get <*> get <*> getWord32le

  put (TxInput p s sq) = put p >> put s >> putWord32le sq

data TxOutput = TxOutput { txoValue_ :: !Word64 -- bitcoin uses an Int64, but it doesn't really matter.
                         , txoScript :: !Script
                         } deriving Show

txoValue :: TxOutput -> BTC
txoValue = (`scale` satoshi) . toInteger . txoValue_

instance Serialize TxOutput where
  get = TxOutput <$> getWord64le <*> get

  put (TxOutput v s) = putWord64le v >> put s

txOutput :: BTC -> Script -> Maybe TxOutput
txOutput (Ƀ b) s | v < 0     = fail $ "txOutput: value "++show b++" too small"
                 | v <= toInteger (maxBound :: Word64) = return $ TxOutput (fromInteger v) s
                 | otherwise = fail $ "txOutput: value "++show b++" too large"
 where
  (Ƀ tiny) = satoshi
  v = floor (b / tiny)

nullOutput :: TxOutput
nullOutput = TxOutput (-1) nullScript

type Tx = GeneralizedTx NEList

data GeneralizedTx f = Tx { txVersion :: Word32 -- I think the txVersion should be restricted to 1, but this isn't how bitcoin works.
                          , txIn :: NEList TxInput
                          , txOut :: f TxOutput
                          , txLock :: Lock
                          }

instance (Foldable f) => Show (GeneralizedTx f) where
  showsPrec d tx = showParen (10 < d) showStr
   where
     showStr = showString "Tx {txVersion = " . shows (txVersion tx)
             . showString "; txIn = " . shows (txIn tx)
             . showString "; txOut = " . shows (toList (txOut tx))
             . showString "; txLock = " . shows (txLock tx)
             . showString "}"

-- The bitcoin client checks to see that all the txiPreviousOutput's are not hash0.
-- This check isn't really needed.
-- It is extremely unlikely that any Tx referenceing hash0 will make it into the block chain.
instance (FromList f, Foldable f) => Serialize (GeneralizedTx f) where
  get = Tx <$> getWord32le <*> getList <*> getList <*> get

  put (Tx v is os t) = putWord32le v >> putList is >> putList os >> put t

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
           os <- getList
           t <- get
           return $ TxCoinBase v en f os t

  put (TxCoinBase v en sq os t) = putWord32le v >> putVarInteger 1 >> put hash0
                               >> putWord32le (-1) >>  put en >> putWord32le sq
                               >> putList os >> put t

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
  get = do v <- getWord32le
           pb <- get
           _ <- get :: Get Hash -- ignore the serialized merkle root; we will compute it ourselves
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
