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

makeAcidic ''Store ['setKey, 'getKey, 'deleteKey]


main :: IO ()
main = do
  state <- openLocalStateFrom "state.bin" M.empty
  forever $ do
    putStr "> "
    hFlush stdout
    cmd <- getLine
    case words cmd of
      ["set", key, value] -> update state (SetKey key value)
      ("set":_) -> putStrLn "set requires two arguments"
      --
      ["get", key] -> print =<< query state (GetKey key)
      ("get":_) -> putStrLn "get requires one argument"
      --
      ["delete", key] -> update state (DeleteKey key)
      ("delete":_) -> putStrLn "delete requires one argument"

      _ -> putStrLn "bad command"


