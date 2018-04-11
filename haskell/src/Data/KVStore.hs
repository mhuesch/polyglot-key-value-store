module Data.KVStore where


import           Control.Monad
import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import           Data.IORef
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           System.Directory
import           System.Exit
import           System.IO


data KVStore = KVStore
  { handle :: IORef Handle
  , dictRef :: IORef Dict
  }

printKVStore :: KVStore -> IO ()
printKVStore (KVStore hdlR dict) = do
  hdl <- readIORef hdlR
  print hdl
  mp <- readIORef dict
  print mp


type Key = String
type Val = String
type Dict = Map Key ValStruct

data ValStruct = ValStruct { valSLen :: Int64, valSOffset :: Int64 }
               deriving (Show)
data ValHeader = ValHeader { keyLen :: Int64, valLen :: Int64 }

instance Binary ValHeader where
  get = do
    keyL <- getInt64be
    valL <- getInt64be
    return (ValHeader keyL valL)

  put (ValHeader keyL valL) = do
    putInt64be keyL
    putInt64be valL


kvSet :: KVStore -> Key -> Val -> IO ()
kvSet (KVStore hdlR dictR) key val = do
  hdl <- readIORef hdlR
  let keyBS = encode key
      valBS = encode val
      valL  = BS.length valBS
      valHeader = ValHeader {
                    keyLen = BS.length keyBS,
                    valLen = valL }
  hSeek hdl SeekFromEnd 0
  BS.hPut hdl (encode valHeader)
  BS.hPut hdl keyBS
  offset <- hTell hdl
  BS.hPut hdl valBS
  modifyIORef' dictR (M.insert key (ValStruct (fromIntegral valL) (fromIntegral offset)))
  hFlush hdl


kvDel :: KVStore -> Key -> IO (Either String ())
kvDel (KVStore hdlR dictR) key = do
  dict <- readIORef dictR
  case M.lookup key dict of
    Nothing -> return (Left "key not found")
    Just _  -> do
      let keyBS = encode key
          valHeader = ValHeader {
                        keyLen = BS.length keyBS,
                        valLen = 0 }
      hdl <- readIORef hdlR
      hSeek hdl SeekFromEnd 0
      BS.hPut hdl (encode valHeader)
      BS.hPut hdl keyBS
      modifyIORef' dictR (M.delete key)
      hFlush hdl
      return (Right ())


kvCompact :: KVStore -> IO FilePath
kvCompact kvs1@(KVStore hdlR dictR) = do
  (kvs2, fp2) <- openKVStore Nothing
  let KVStore hdl2 dictR2 = kvs2
  --
  dict1Keys <- M.keys <$> readIORef dictR
  forM_ dict1Keys $ \key -> do
    val <- fromJust <$> kvGet kvs1 key
    kvSet kvs2 key val
  -- write the new kvs into the old one, so we
  -- can maintain the old handle
  writeIORef hdlR =<< readIORef hdl2
  writeIORef dictR =<< readIORef dictR
  return fp2


kvAllKeys :: KVStore -> IO [Key]
kvAllKeys (KVStore _ dictR) = do
  dict <- readIORef dictR
  return (M.keys dict)


kvGet :: KVStore -> Key -> IO (Maybe Val)
kvGet (KVStore hdlR dictR) key = do
  dict <- readIORef dictR
  case M.lookup key dict of
    Nothing -> return Nothing
    Just valStruct -> do
      hdl <- readIORef hdlR
      hSeek hdl AbsoluteSeek (fromIntegral (valSOffset valStruct))
      valBS <- BS.hGet hdl (fromIntegral (valSLen valStruct))
      return (Just (runGet get valBS))

kvDir :: FilePath
kvDir = "kv.db"

openKVStore :: Maybe FilePath -> IO (KVStore, FilePath)
openKVStore Nothing = do
  createDirectoryIfMissing True kvDir
  (fp,hdl) <- openTempFile kvDir "db"
  hdlR <- newIORef hdl
  dictR <- newIORef M.empty
  return (KVStore hdlR dictR, fp)
--
openKVStore (Just fp) = do
  exists <- doesFileExist fp
  hdl <- if exists
            then openFile fp ReadWriteMode
            else hPutStrLn stderr "db file doesn't exist" >> exitFailure
  hdlR <- newIORef hdl
  dictR <- newIORef =<< extractDict hdl
  return (KVStore hdlR dictR, fp)

closeKVStore :: KVStore -> IO ()
closeKVStore (KVStore hdlR _) = do
  hClose =<< readIORef hdlR

extractDict :: Handle -> IO Dict
extractDict hdl = go M.empty
  where
    go dict = do
      isEnd <- hIsEOF hdl
      if isEnd
         then return dict
         else do
           let headerLen = 16
           headerBS <- BS.hGet hdl headerLen
           let (ValHeader keyLen valLen) = runGet get headerBS
           --                    \/ this might be an issue on non-64-bit
           --                       machines
           keyBS <- BS.hGet hdl (fromIntegral keyLen)
           let key = runGet get keyBS

           valOffset <- fromIntegral <$> hTell hdl
           let valStruct = ValStruct valLen valOffset
               -- zero-length value is a tombstone - delete key from dict.
               dict' = if valLen == 0
                          then M.delete key dict
                          else M.insert key valStruct dict

           hSeek hdl RelativeSeek (fromIntegral valLen)
           go dict'

