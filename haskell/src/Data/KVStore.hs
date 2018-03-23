module Data.KVStore where


import           Data.Binary
import           Data.Binary.Get
import           Data.Binary.Put
import qualified Data.ByteString.Lazy as BS
import           Data.IORef
import           Data.Int
import           Data.Map (Map)
import qualified Data.Map as M
import           System.Directory
import           System.Exit
import           System.IO


data KVStore = KVStore
  { handle :: Handle
  , dictRef :: IORef Dict
  }

printKVStore :: KVStore -> IO ()
printKVStore (KVStore hdl dict) = do
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
kvSet (KVStore hdl dictR) key val = do
  let keyBS = runPut (put key)
      valBS = runPut (put val)
      keyL = BS.length keyBS
      valL = BS.length valBS
  hSeek hdl SeekFromEnd 0
  BS.hPut hdl (runPut (put keyL))
  BS.hPut hdl (runPut (put valL))
  BS.hPut hdl keyBS
  offset <- hTell hdl
  BS.hPut hdl valBS
  modifyIORef' dictR (M.insert key (ValStruct (fromIntegral valL) (fromIntegral offset)))
  hFlush hdl


kvAllKeys :: KVStore -> IO [Key]
kvAllKeys (KVStore _ dictR) = do
  dict <- readIORef dictR
  return (M.keys dict)


kvGet :: KVStore -> Key -> IO (Maybe Val)
kvGet (KVStore hdl dictR) key = do
  dict <- readIORef dictR
  case M.lookup key dict of
    Nothing -> return Nothing
    Just valStruct -> do
      hSeek hdl AbsoluteSeek (fromIntegral (valSOffset valStruct))
      valBS <- BS.hGet hdl (fromIntegral (valSLen valStruct))
      return (Just (runGet get valBS))

kvDir :: FilePath
kvDir = "kv.db"

openKVStore :: Maybe FilePath -> IO (KVStore, FilePath)
openKVStore Nothing = do
  createDirectoryIfMissing True kvDir
  (fp,hdl) <- openTempFile kvDir "db"
  dict <- newIORef M.empty
  return (KVStore hdl dict, fp)
--
openKVStore (Just fp) = do
  exists <- doesFileExist fp
  hdl <- if exists
            then openFile fp ReadWriteMode
            else hPutStrLn stderr "db file doesn't exist" >> exitFailure
  dict <- newIORef =<< extractDict hdl
  return (KVStore hdl dict, fp)

closeKVStore :: KVStore -> IO ()
closeKVStore (KVStore hdl _) = do
  hClose hdl

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
           hSeek hdl RelativeSeek (fromIntegral valLen)
           go (M.insert key valStruct dict)

