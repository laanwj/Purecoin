module Purecoin.Core.Transaction
       ( Coins, CoinMap, emptyCoinMap, coinMapSize, addCoins, addTransaction, prepcbTransaction
       ) where

import Data.Maybe (fromMaybe)
import Data.Monoid (mconcat, mappend)
import qualified Data.Map as M
import Control.Applicative ((<$>))
import Control.Monad (unless, zipWithM)
import qualified Control.Monad.State as SM
import Data.NEList (NEList(..), (<|), appendNE, toList)
import Data.ByteString (append)
import Purecoin.Utils (nonNegativeByteStringBE)
import Purecoin.Core.Serialize (encode)
import Purecoin.Core.Script (MakeHash, scriptOps, opsScript, doScript, execScriptMonad)
import Purecoin.Core.Hash (Hash, hash, hashBS)
import Purecoin.Core.DataTypes ( TxInput(..)
                               , GeneralizedTx(..), Tx
                               , TxCoinBase, txcbOut
                               , TxOutput, nullOutput, txoValue, txoScript
                               , OutPoint, outPoint
                               , BTC
                               )
import Purecoin.Core.Signature ( HashKind(..)
                               , csHashKind, csAnyoneCanPay, csHashType)

data Coins = Coins [(OutPoint, TxOutput)]

coins :: Hash -> [TxOutput] -> Maybe Coins
coins h txos = Coins <$> zipWithM f txos [0..]
 where
  f txo i = do op <- outPoint h i
               return (op, txo)

newtype CoinMap = CoinMap (M.Map OutPoint TxOutput)

emptyCoinMap :: CoinMap
emptyCoinMap = CoinMap M.empty

coinMapSize :: CoinMap -> Int
coinMapSize (CoinMap m) = M.size m

removeCoin :: (Monad m) => OutPoint -> SM.StateT CoinMap m TxOutput
removeCoin key = do (CoinMap cm) <- SM.get
                    txo <- maybe (fail errMsg) return $ M.lookup key cm
                    SM.put . CoinMap . M.delete key $ cm
                    return txo
 where
  errMsg = "Input "++show key++" not found"

addCoins :: (Monad m) => Coins -> SM.StateT CoinMap m ()
addCoins (Coins toInsert) = SM.modify $ \(CoinMap cm) -> CoinMap $ foldr (uncurry M.insert) cm toInsert

addTransaction :: Tx -> SM.StateT CoinMap (Either String) (BTC,BTC)
addTransaction tx =
  do valueInputs <- mapM validateTxi . getTxInputs $ tx
     let ins = mconcat valueInputs
     let outs = mconcat valueOutputs
     unless (outs <= ins)
            (fail $ "Outputs greater than inputs in Tx: "++show txHash++
                    " outs:" ++ show outs ++ " ins:" ++ show ins)
     cs <- maybe (fail "addTransaction: bad coins!!") return $ coins (hash tx) txos
     addCoins cs
     return (ins, outs)
  where
   txHash = hash tx
   txos = toList . txOut $ tx
   valueOutputs = map txoValue txos
   validateTxi (txi, mkHash) = do
     ptxo <- removeCoin . txiPreviousOutput $ txi
     opsSig <- either fail return $ scriptOps (txiScript txi)
     opsCheck <- either fail return $ scriptOps (txoScript ptxo)
     maybe (fail errMsg) return . execScriptMonad mkHash $ doScript (toList opsSig)
                                                        >> doScript (toList opsCheck)
     return (txoValue ptxo)
    where
     errMsg = "Script for "++show (txiPreviousOutput txi)++" in transaction "++show txHash++" failed"

prepcbTransaction :: (BTC,BTC) -> TxCoinBase -> Either String Coins
prepcbTransaction (ins, otherOuts) tx
  | outs <= ins = maybe (fail "prebcbTransaction: bad coins!!") return $ coins (hash tx) txos
  | otherwise   = fail $ "Outputs greater than inputs in coinbase Tx: "++show txHash++
                         " outs:" ++ show outs ++ " ins:" ++ show ins
 where
  txHash = hash tx
  txos = toList . txcbOut $ tx
  outs = mconcat (map txoValue txos) `mappend` otherOuts

getTxInputs :: Tx -> [(TxInput, MakeHash)]
getTxInputs tx = map result . selections . toList . txIn $ tx
  where
   result (l, m, r) = (m, makeHash l m r)
   setScript s txi = txi{txiScript = opsScript s}
   makeHash l m r script sig = fromMaybe 1 $ do
     ntx <- newTx
     return . nonNegativeByteStringBE . encode . hashBS $ encode ntx `append` csHashType sig
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
        m' = setScript script m
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

selections :: [a] -> [([a],a,[a])]
selections [] = []
selections (x:xs) = ([],x,xs):(map (\(l,m,r) -> (x:l,m,r)) (selections xs))
