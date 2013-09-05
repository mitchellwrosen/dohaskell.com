module Types where

import Data.Text (Text)
import Prelude (String)
import System.Exit (ExitCode)

import Model

type ModuleName = Text
type FunctionName = Text
type FilePath = Text

-- | Type to unify LibFunction and UserFunction
data Function = LibF  LibFunction
              | UserF UserFunction

-- | The result of running a process.
data Result = Result
    { resultExitCode :: ExitCode
    , resultStdout   :: String
    , resultStderr   :: String
    }

toResult :: (ExitCode, String, String) -> Result
toResult (exit_code, std_out, std_err) = Result
    { resultExitCode = exit_code
    , resultStdout   = std_out
    , resultStderr   = std_err
    }
