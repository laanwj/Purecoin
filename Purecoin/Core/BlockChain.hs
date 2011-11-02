module Purecoin.Core.BlockChain
       (BlockChain, newChain, getCoinMap, addBlock)
       where

import Control.Applicative ((<$>))
import Control.Arrow ((***), (&&&))
import Control.Monad (guard)
import Data.List (unfoldr, sort)
import Data.Maybe (listToMaybe)
import Data.Monoid (mempty, mconcat)
import Data.Time (UTCTime, addUTCTime, diffUTCTime)
import Data.Word (Word32)
import Control.Monad.State as SM
import qualified Data.PSQueue as PSQ
import Data.NEList (NEList(..), toList)
import qualified Purecoin.WordArray as WS
import Purecoin.Core.Hash (Hash, hash0, hash)
import Purecoin.Core.Script (opPushData, scriptOps, opsScript, OP(OP_CHECKSIG))
import Purecoin.Core.DataTypes ( Difficulty, target, fromTarget
                               , lockView, LockView(..)
                               , BTC(..)
                               , txCoinBase, txcbExtraNonce, txcbLock, txcbFinal
                               , txLock, txIn, txiSequence
                               , txOutput
                               , Block, block, bPrevBlock, bBits, bTimestamp, bCoinBase, bTxs, bHash)
import Purecoin.Core.Transaction (Coins, CoinMap, emptyCoinMap, addTransaction, addCoins, prepcbTransaction)

data BlockInfo = BlockInfo {biWork      :: Integer
                           ,biNumber    :: Integer
                           ,biTimestamp :: UTCTime
                           ,biPrevBlock :: Hash
                           ,biCoinMap   :: CoinMap
                           ,biBits      :: Difficulty
                           ,biCoinBase  :: Coins
                           }

instance Eq BlockInfo where -- this is the order used for the PSQ; do not use.
  x == y = compare x y == EQ

instance Ord BlockInfo where -- this is the order used for the PSQ; do not use.
  compare x y = compare (biWork y) (biWork x) -- comparision reversed because PSQ uses min priority

data BlockChain a = BlockChain { bcChain :: PSQ.PSQ Hash BlockInfo -- Must be non-empty.
                               , maxTarget :: Difficulty -- make this a typeclass method
                               }
chain :: BlockChain a -> Hash -> [BlockInfo]
chain bc = unfoldr go
 where
   go h = do bi <- PSQ.lookup h . bcChain $ bc
             return (bi, (biPrevBlock bi))

coinValue n = Ƀ (50 / (2 ^ fromIntegral (n `div` 210000)))

work :: Difficulty -> Integer
work x = sha256size `div` (target x + 1)
 where
  sha256size = 2^256

newChain :: Word32 -> UTCTime -> Difficulty -> Word32 -> Maybe (BlockChain a)
newChain version time difficulty nonce = 
  do txo <- txOutput (coinValue genesisNumber) (opsScript (NENil [opPushData $ WS.fromList [0x04,0x67,0x8a,0xfd,0xb0,0xfe,0x55,0x48,0x27,0x19,0x67,0xf1,0xa6,0x71,0x30,0xb7,0x10,0x5c,0xd6,0xa8,0x28,0xe0,0x39,0x09,0xa6,0x79,0x62,0xe0,0xea,0x1f,0x61,0xde,0xb6,0x49,0xf6,0xbc,0x3f,0x4c,0xef,0x38,0xc4,0xf3,0x55,0x04,0xe5,0x1e,0xc1,0x12,0xde,0x5c,0x38,0x4d,0xf7,0xba,0x0b,0x8d,0x57,0x8a,0x4c,0x70,0x2b,0x6b,0xf1,0x1d,0x5f],OP_CHECKSIG]))
     genesis <- block version hash0 time difficulty nonce (txCoinBase 1 (opsScript (NENil [opPushData $ WS.fromList [0xff,0xff,0,0x1d],opPushData $ WS.fromList [0x04],opPushData theTimes])) (NENil txo)) []
     genesisCoins <- either fail return $ prepcbTransaction (coinValue genesisNumber, mempty) (bCoinBase genesis)
     let genesisInfo = BlockInfo {biWork      = work (bBits genesis)
                                 ,biNumber    = genesisNumber
                                 ,biPrevBlock = hash0
                                 ,biTimestamp = bTimestamp genesis
                                 ,biBits      = bBits genesis
                                 ,biCoinMap   = emptyCoinMap
                                 ,biCoinBase  = genesisCoins
                                 }
     -- TODO: I had a bug where I used hash instead of bHash.  I need to do something with the type system to prevent this in the future --
     return (BlockChain (PSQ.singleton (bHash genesis) genesisInfo) (bBits genesis))
 where
  genesisNumber = 0
  theTimes = WS.fromAscii "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"

getCoinMap :: BlockChain a -> Maybe CoinMap
getCoinMap bc = (biCoinMap . PSQ.prio) <$> (PSQ.findMin . bcChain $ bc)

addBlock :: UTCTime -> Block -> BlockChain a -> Either String (BlockChain a)
addBlock currentTime bl bc = do newBlockInfo <- go (chain bc prevHash)
                                return $ bc{bcChain = PSQ.insert (bHash bl) newBlockInfo (bcChain bc)}
 where
  maxBits = maxTarget bc
  prevHash = bPrevBlock bl
  newCoinBase = bCoinBase bl
  newTxs = bTxs bl
  newBits = bBits bl
  newTimestamp = bTimestamp bl
  go [] = fail $ "Previous block "++show prevHash++" not found"
  go theChain@(prevBlockInfo:_) = do checkTarget
                                     checkTimestamp
                                     checkFinalTxs
                                     (cbase, newMap) <- either fail return $ runStateT processTxs (biCoinMap prevBlockInfo)
                                     return $ BlockInfo {biWork      = (biWork prevBlockInfo) + work (bBits bl)
                                                        ,biNumber    = newNumber
                                                        ,biPrevBlock = prevHash
                                                        ,biTimestamp = newTimestamp
                                                        ,biBits      = newBits
                                                        ,biCoinMap   = maybe newMap (\cb -> SM.execState (addCoins cb) newMap) gradcb
                                                        ,biCoinBase  = cbase
                                                        }
   where
    newNumber = succ (biNumber prevBlockInfo)
    processTxs = do fees <- mapM addTransaction newTxs
                    let totalFees = (mconcat *** mconcat) . unzip $ (coinValue newNumber, mempty):fees
                    lift (prepcbTransaction totalFees newCoinBase)
    -- on the *next* block the 98th-block-before-the-previous-block's coinbase will be useable.
    gradcb = fmap biCoinBase . listToMaybe . drop 98 $ theChain
    checkTarget = maybe (fail errTarget) return $ guard (requiredBits == newBits)
     where
      requiredBits | changeTarget = fromTarget (min newDifficulty (target maxBits))
                   | otherwise    = lastTarget
       where
        blocksPerTarget :: Int
        blocksPerTarget = 14 * 24 * 6
        tenMinutes     = 10 * 60
        lastTarget = biBits prevBlockInfo
        changeTarget = 0 == (fromIntegral (biNumber prevBlockInfo) + 1) `mod` blocksPerTarget
        -- notice the time it takes to generate a new block after a change in difficutly isn't taken into account!
        timeSpan = diffUTCTime (biTimestamp prevBlockInfo) (biTimestamp (theChain!!(blocksPerTarget-1)))
        targetTimeSpan = toInteger (blocksPerTarget * tenMinutes)
        lowerTimeSpan = targetTimeSpan `div` 4
        upperTimeSpan = targetTimeSpan * 4
        -- Try to round the same way the offical client does.
        newDifficulty :: Integer
        newDifficulty = (target lastTarget) * clamp lowerTimeSpan (round timeSpan) upperTimeSpan `div` targetTimeSpan
      errTarget = "Block "++show (hash bl)++" should have difficulty "++show (target requiredBits)++ " but has difficulty "++show (target newBits)++" instead."
    checkTimestamp = maybe (fail (errTimestamp currentTime)) return
                   $ do pts <- prevTimestamp
                        guard (pts < newTimestamp && newTimestamp <= addUTCTime twoHours currentTime)
     where
      twoHours = 2 * 60 * 60
      prevTimestamp = median . map (biTimestamp) . take 11 $ theChain
      errTimestamp ct = "Block "++show (hash bl)++" time of "++show newTimestamp++" is before "++show prevTimestamp++" or after currentTime "++show ct
    checkFinalTxs = maybe (fail errFinalTxs) return $ guard (all isFinalTx (cbLock:txLocks))
     where
      errFinalTxs = "Block "++show (hash bl)++" contains non-final transactions" -- more precise error message needed.
      cbLock = (txcbLock &&& txcbFinal) newCoinBase
      txLocks = map (txLock &&& (all txiFinal . toList . txIn)) newTxs
      txiFinal txi = txiSequence txi == maxBound
      isFinalTx (lock, finalSeq) = finalSeq || go (lockView lock)
       where
        go Unlocked = True
        go (LockBlock n) = newNumber < n
        go (LockTime t) = newTimestamp < t


median :: (Ord a) => [a] -> Maybe a
median [] = Nothing
median l = Just (sl!!((length sl) `div` 2))
  where
    sl = sort l

clamp low x high | x < low = low
                 | high < x = high
                 | otherwise = x
