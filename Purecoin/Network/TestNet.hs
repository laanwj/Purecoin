module Purecoin.Network.TestNet where
       ( magic, port, difficulty
       , Chain, startBlockChain
       ) where

import Data.Word (Word8)
import Purecoin.Core.DataTypes (Difficulty, fromTarget)
import Purecoin.Core.BlockChain (BlockChain, newChain)

magic :: [Word8]
magic = [0xFA, 0xBF, 0xB5, 0xDA]

port :: Num a => a
port = 18333

difficulty :: Difficulty
difficulty = fromTarget (0x7fff8 * 2^208)

newtype Chain = Chain Chain

startBlockChain :: BlockChain Chain
Just startBlockChain = newChain 1 (read $ "2011-02-02 23:16:42 UTC") difficulty 384568319
