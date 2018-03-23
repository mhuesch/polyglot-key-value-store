module CmdLine
  ( getOptions
  , PosArgs(..)
  , Options(..)
  ) where


import           Control.Monad
import           Data.Word
import           System.Console.GetOpt
import           System.Directory
import           System.Environment
import           System.Exit
import           System.IO


--------------------------------------------------------------------------------
-- Options data types
--------------------------------------------------------------------------------
type PosArgs = ()

data Options = Options
  { optInteractive :: Bool
  , optDBFP :: Maybe FilePath }

defaultOptions = Options
  { optInteractive = True
  , optDBFP = Nothing }

--------------------------------------------------------------------------------
-- Interface
--------------------------------------------------------------------------------
getOptions :: IO (PosArgs, Options)
getOptions = do
  -- Parse command line --------------------------------------------------
  (actions, positionals, _) <- getOpt Permute options <$> getArgs
  --                     ^ we ignore error messages here, because they are
  --                     handled in the option parsers via IO

  -- fold the options over the list of actions
  opts <- foldl (>>=) (return defaultOptions) actions

  -- check positional arguments
  posArgs <- case positionals of
    []   -> return ()
    _    -> showUsage >> exitFailure

  return (posArgs, opts)

--------------------------------------------------------------------------------
-- Usage printout
--------------------------------------------------------------------------------
showUsage :: IO ()
showUsage = do
  prg <- getProgName
  let header = unlines [ "usage: " ++ prg ++ " [OPTIONS..]"
                       , ""
                       , "OPTIONS:"
                       ]
  hPutStrLn stderr (usageInfo header options)

--------------------------------------------------------------------------------
-- Options to handle
--------------------------------------------------------------------------------
options :: [ OptDescr (Options -> IO Options) ]
options =
  [ Option "p" ["pipe"]
        (NoArg
            (\opt -> return opt { optInteractive = False }))
        "non-interactive / pipe mode"

  , Option "d" ["db-path"]
        (ReqArg
            (\arg opt -> return opt { optDBFP = Just arg })
            "FILEPATH")
        "path to existing database file"

  , Option "h" ["help"]
        (NoArg
            (\_ -> do
              showUsage
              exitSuccess))
        "Show help"
  ]
