module Main where


import           Control.Monad.IO.Class
import           System.Console.Haskeline
import           System.Environment
import           System.IO

import           Data.KVStore


main :: IO ()
main = do
  args <- getArgs
  let mbFP = case args of
               [fp] -> Just fp
               _    -> Nothing
  kvs <- openKVStore mbFP
  printWelcome
  runInputT defaultSettings (loop kvs)
  where
    loop :: KVStore -> InputT IO ()
    loop kvs = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just cmd -> do
          case words cmd of
            ["set", key, value] -> liftIO (kvSet kvs key value)
            ("set":_) -> outputStrLn "set requires two arguments"
            --
            ["get", key] -> outprint =<< liftIO (kvGet kvs key)
            ("get":_) -> outputStrLn "get requires one argument"
            --
            ["delete", key] -> outputStrLn "delete is not currently supported"
              -- liftIO (update state (DeleteKey key))
            ("delete":_) -> outputStrLn "delete requires one argument"
            --
            ["all-keys"] -> mapM_ outprint =<< liftIO (kvAllKeys kvs)
            ("all-keys":_) -> outputStrLn "all-keys has no arguments"
            --
            ["help"] -> liftIO printHelp

            _ -> outputStrLn "unrecognized command. type `help` for help."

          loop kvs


outprint :: Show a => a -> InputT IO ()
outprint = outputStrLn . show


printWelcome :: IO ()
printWelcome = do
  putStrLn (replicate 40 '-')
  putStrLn "welcome to mhueschen's key-value store"
  printHelp
  putStrLn (replicate 40 '-')

printHelp :: IO ()
printHelp = do
  putStrLn "available commands:"
  mapM_ putStrLn availableCommands
    where
      availableCommands = [ "set <key> value>"
                          , "get <key>"
                          , "delete <key>"
                          , "all-keys"
                          , "help"
                          , "quit"
                          ]
