module Purecoin.Network.MainNet
       ( magic, port, difficulty
       , Chain, startBlockChain
       ) where

import Data.Word (Word8)
import Purecoin.Core.DataTypes (Difficulty, fromTarget)
import Purecoin.Core.BlockChain (BlockChain, newChain)

magic :: [Word8]
magic = [0xF9, 0xBE, 0xB4, 0xD9]

port :: Num a => a
port = 8333

difficulty :: Difficulty
difficulty = fromTarget (0xffff * 2^208)

newtype Chain = Chain Chain

startBlockChain :: BlockChain Chain
Just startBlockChain = newChain 1 (read $ "2009-01-03 18:15:05 UTC") difficulty 2083236893