module Main where

import Numeric
import Data.ByteString (ByteString, unpack, hGet, hPut)
import qualified Data.ByteString as BS
import Purecoin.Core.Serialize
import Control.Exception hiding (block)
import Data.Word
import Data.Maybe
import Network
import System.IO
import System.Random (randomIO)
import Control.Monad.Reader
import Control.Monad.Error (MonadError, throwError, strMsg)
import qualified Data.Foldable as F
import Data.Ratio ((%))
import Data.Time (getCurrentTime)
import Purecoin.Core.DataTypes
import Purecoin.Core.Hash
import Purecoin.Core.Transaction
import Purecoin.Core.BlockChain
import Purecoin.Network.DataTypes
import qualified Purecoin.Network.TestNet as Network

import Data.IORef
import Debug.Trace
tr x = traceShow x x

difficultyLevel :: Difficulty -> Rational
difficultyLevel x = target Network.difficulty % target x

instance Show Difficulty where
  show x = show (fromRational . difficultyLevel $ x :: Double)

instance Show Block where
  show b = show (hash (bCoinBase b))++":"++show (bCoinBase b)++"::"
        ++ concatMap (\x -> (show (hash x))++":"++(show x)) (bTxs b)

mkIPv4 :: [Word8] -> ByteString
mkIPv4 [a,b,c,d] = BS.pack (replicate 10 0 ++ [0xff,0xff] ++ [a,b,c,d])

toIOError :: Either String a -> IO a
toIOError = either (throwError . strMsg) return

joinIOError :: IO (Either String a) -> IO a
joinIOError x = x >>= toIOError

hGetWord32le :: Handle -> IO Word32
hGetWord32le h = joinIOError $ runGet getWord32le `fmap` hGet h 4

putPacket :: String -> ByteString -> ByteString
putPacket name payload = runPut
                   $ mapM_ putWord8 Network.magic
                  >> putByteString name'
                  >> putWord32le (fromIntegral (BS.length payload))
                  >> unless (name `elem` ["version", "verack"]) (putByteString checksum)
                  >> putByteString payload
 where
  name' = BS.pack (take 12 (map (toEnum . fromEnum) name ++ repeat 0))
  checksum = BS.take 4 . encode . hashBS $ payload

data IOStore a = IOStore { networkHandle :: Handle
                         , txDb :: IORef (BlockChain a)
                         , connectionNonce :: Word64}

newIOStore :: Handle -> BlockChain a -> IO (IOStore a)
newIOStore h bc = do db <- newIORef bc
                     n <- randomIO
                     return $ IOStore h db n

type MainIO a = ReaderT (IOStore a) IO

askHandle :: MainIO a Handle
askHandle = networkHandle `fmap` ask

askDb :: MainIO a (IORef (BlockChain a))
askDb = txDb `fmap` ask

askNonce :: MainIO a Word64
askNonce = connectionNonce `fmap` ask

hPutPacket :: String -> ByteString -> MainIO a ()
hPutPacket name payload = do h <- askHandle
                             liftIO $ hPut h (putPacket name payload) >> hFlush h

getPacket :: MainIO a (String, ByteString)
getPacket = do
  h <- askHandle
  liftIO $ do
    passHeader h Network.magic
    command <- unpack `fmap` hGet h 12
    let name = map (toEnum.fromEnum) . takeWhile (/= 0) $ command
    len <- hGetWord32le h
    unless (name `elem` ["version", "verack"]) (hGet h 4 >> return ())
    payload <- hGet h (fromIntegral len)
    return (name, payload)
 where
  passHeader h [] = return ()
  passHeader h (m:ms) = do
    [x] <- BS.unpack `fmap` hGet h 1
    if x == m then passHeader h ms else passHeader h Network.magic

myVersion :: MainIO a Version
myVersion = do ct <- liftIO getCurrentTime
               n <- askNonce
               return $ Version protocolVersion 0 ct (NetworkAddress 1 (mkIPv4 [127,0,0,1]) Network.port) (NetworkAddress 1 (mkIPv4 [127,0,0,1]) Network.port) n protocolSubVersion 0

updateBlock :: Block -> MainIO a ()
updateBlock blk = do
  dbp <- askDb
  liftIO . putStrLn $ "processing block: "++show (bHash blk)++":"++show (bTimestamp blk)++":"
                      ++ show (sum [length . F.toList . txIn $ tx | tx<-(bTxs blk)])
  modifyBlockChain <- liftIO $ addBlock blk
  liftIO . atomicModifyIORef dbp 
         $ \db -> (\x -> (x,())) . either (\err -> trace err db) id . modifyBlockChain $ db
  -- liftIO $ print =<< readIORef dbp

printTx :: MainIO a ()
printTx = do
  dbp <- askDb
  db <- liftIO $ readIORef dbp
  liftIO . putStrLn . (\x -> "Number of unclaimed coins: "++show (coinMapSize x)) . getCoinMap $ db

main :: IO ()
main =
  bracket (connectTo "127.0.0.1" (PortNumber Network.port)) hClose
  $ \h ->
     do
      store <- newIOStore h Network.startBlockChain
      flip runReaderT store $ do
       -- replicateM (4+12+4+85) ((putStrLn . show) =<< hGet h 1)
       versionPacket <- myVersion
       hPutPacket "version" (encode versionPacket)
       (command,payload) <- getPacket
       liftIO $ putStrLn command
       liftIO $ print (map (flip showHex "") (unpack payload))
       liftIO $ print =<< (toIOError (decode payload) :: IO Version)

       liftIO $ print (map (flip showHex "") (unpack (putPacket "verack" BS.empty)))
       liftIO $ print (map (flip showHex "") (unpack (putPacket "getblocks" (encode (GetBlocks protocolVersion [hash0] hash0)))))
       liftIO $ print (map (flip showHex "") (unpack (putPacket "getaddr" BS.empty)))

       hPutPacket "verack" BS.empty
       -- hPutPacket "getaddr" BS.empty
       -- hPutPacket "getdata" (encode (Inv [InventoryVector 2 testGenesis]))
       hPutPacket "getblocks" (encode (GetBlocks protocolVersion [hash0] hash0))
       forever $ do { (command,payload) <- getPacket; run command payload }
 where
  run "inv" = runInv
  run "block" = runBlock
  run "tx" = runTx
  run "ping" = \_ -> liftIO (putStrLn "ping") >> hPutPacket "ping" BS.empty
  run cmd = \_ -> liftIO $ putStrLn cmd

  runTx payload = do
    tx <- liftIO (toIOError $ decode payload :: IO Tx)
    liftIO $ print tx

  runInv payload = do
    inv <- liftIO $ inventory `fmap` (toIOError $ decode payload)
    liftIO $ print (length inv)
    liftIO $ mapM_ print inv
    hPutPacket "getblocks" (encode (GetBlocks protocolVersion [ivHash . last $ inv] hash0))
    hPutPacket "getdata" payload

  runBlock payload = {-# SCC "runBlock" #-} do
    block <- liftIO (toIOError $ decode payload :: IO Block)
    let bh = bHash block
    let bb = bBits block
    -- putStrLn $ show bh ++ ":" ++ show (compare (hashToInteger bh) (target bb))
    -- liftIO $ print block
    updateBlock block
    printTx
