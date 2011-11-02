module Purecoin.Network.DataTypes
       ( NetworkAddress(..)
       , InventoryVector(..)
       , Version(..)
       , GetBlocks(..)
       , Inv(..)
       , protocolVersion, protocolSubVersion
       ) where

import Control.Applicative ((<$>), (<*>))
import Data.Word (Word16, Word32, Word64)
import Data.Time (UTCTime)
import Data.Time.Clock.POSIX (posixSecondsToUTCTime, utcTimeToPOSIXSeconds)
import Data.ByteString (ByteString, pack)
import Purecoin.Core.Serialize ( Serialize
                               , get, getWord16be, getWord32le, getWord64le, getList, getBytes, getVarByteString
                               , put, putWord16be, putWord32le, putWord64le, putList, putByteString, putVarByteString
                               )
import Purecoin.Core.Hash (Hash)

data NetworkAddress = NetworkAddress { naServices :: Word64
                                     , naIPv6     :: ByteString
                                     , naPort     :: Word16
                                     } deriving Show

data InventoryVector = InventoryVector { ivType :: Word32
                                       , ivHash :: Hash
                                       } deriving Show

data Version = Version { version :: Word32
                       , services :: Word64
                       , timestamp :: UTCTime
                       , addr_me :: NetworkAddress
                       , addr_you :: NetworkAddress
                       , nonce :: Word64
                       , sub_version_num :: ByteString
                       , start_height :: Word32
                       } deriving Show

data GetBlocks = GetBlocks { gbVersion :: Word32
                           , gbHash_start :: [Hash]
                           , gbHash_stop :: Hash
                           } deriving Show

newtype Inv = Inv { inventory :: [InventoryVector] } deriving Show

instance Serialize NetworkAddress where
  get = NetworkAddress <$> getWord64le <*> getBytes 16 <*> getWord16be
  put (NetworkAddress s ip p) = putWord64le s >> putByteString ip >> putWord16be p

instance Serialize InventoryVector where
  get = InventoryVector <$> getWord32le <*> get
  put (InventoryVector t h) = putWord32le t >> put h

instance Serialize Version where
  get = Version <$> getWord32le
                <*> getWord64le
                <*> (posixSecondsToUTCTime . fromIntegral <$> getWord64le)
                <*> get
                <*> get
                <*> getWord64le
                <*> getVarByteString
                <*> getWord32le
  put (Version v s t am ay n sv sh) = putWord32le v
                        >> putWord64le s
                        >> (putWord64le . round . utcTimeToPOSIXSeconds $ t)
                        >> put am
                        >> put ay
                        >> putWord64le n
                        >> putVarByteString sv
                        >> putWord32le sh

instance Serialize GetBlocks where
  get = GetBlocks <$> getWord32le <*> getList <*> get
  put (GetBlocks v hs he) = putWord32le v >> putList hs >> put he

instance Serialize Inv where
  get = Inv <$> getList
  put (Inv ivs) = putList ivs

protocolVersion :: Word32
protocolVersion = 32002

protocolSubVersion :: ByteString
protocolSubVersion = pack (map (toEnum.fromEnum) "Purecoin")
