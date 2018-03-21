module Main where


import           Control.Monad
import           Control.Monad.IO.Class
import           Data.List
import           System.Console.Haskeline
import           System.Environment
import           System.IO

import           CmdLine
import           Data.KVStore


main :: IO ()
main = do
  (posArgs, opts) <- getOptions
  kvs <- openKVStore (optDBFP opts)
  when (optInteractive opts) $
    printWelcome
  runInputT mySettings (runRepl (optInteractive opts) kvs)


runRepl :: Bool -> KVStore -> InputT IO ()
runRepl isInteractive kvs = loop
  where
    prompt = if isInteractive
                then "> "
                else ""
    loop = do
      minput <- getInputLine prompt
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

          loop


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

availableCommands :: [String]
availableCommands = [ "set <key> value>"
                    , "get <key>"
                    , "delete <key>"
                    , "all-keys"
                    , "help"
                    , "quit"
                    ]

wordList = map (head . words) availableCommands

searchFunc :: String -> [Completion]
searchFunc str = map simpleCompletion $ filter (str `isPrefixOf`) wordList

mySettings :: Settings IO
mySettings = Settings { historyFile = Nothing
                      , complete = completeWord Nothing " \t" $ return . searchFunc
                      , autoAddHistory = True
                      }
