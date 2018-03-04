{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Main where

import           Control.Monad.Reader
import           Control.Monad.State
import           Data.Acid
import           Data.Map (Map)
import qualified Data.Map as M
import           System.Exit
import           System.Console.Haskeline
import           System.IO


type Key = String
type Value = String
type Store = Map Key Value

setKey :: Key -> Value -> Update Store ()
setKey k a = modify (M.insert k a)

getKey :: Key -> Query Store (Maybe Value)
getKey k = M.lookup k <$> ask

deleteKey :: Key -> Update Store ()
deleteKey k = modify (M.delete k)

allKeys :: Query Store [Key]
allKeys = M.keys <$> ask

makeAcidic ''Store ['setKey, 'getKey, 'deleteKey, 'allKeys]


main :: IO ()
main = do
  state <- openLocalStateFrom "state.bin" M.empty
  printWelcome
  runInputT defaultSettings (loop state)
  where
    loop :: AcidState (EventState SetKey) -> InputT IO ()
    loop state = do
      minput <- getInputLine "> "
      case minput of
        Nothing -> return ()
        Just "quit" -> return ()
        Just cmd -> do
          case words cmd of
            ["set", key, value] -> liftIO (update state (SetKey key value))
            ("set":_) -> outputStrLn "set requires two arguments"
            --
            ["get", key] -> outprint =<< liftIO (query state (GetKey key))
            ("get":_) -> outputStrLn "get requires one argument"
            --
            ["delete", key] -> liftIO (update state (DeleteKey key))
            ("delete":_) -> outputStrLn "delete requires one argument"
            --
            ["all-keys"] -> mapM_ outprint =<< liftIO (query state AllKeys)
            ("all-keys":_) -> outputStrLn "all-keys has no arguments"
            --
            ["help"] -> liftIO printHelp

            _ -> outputStrLn "unrecognized command. type `help` for help."

          loop state


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


