{-# LANGUAGE DeriveDataTypeable
           , FlexibleContexts
           , GADTs
           , OverloadedStrings
           , TemplateHaskell
           , TypeFamilies #-}

module Model where

import Prelude
import Data.Text (Text)
import Database.Persist.Quasi
import Data.Typeable (Typeable)
import System.Exit (ExitCode)

import Yesod

-- You can define all of your database entities in the entities file.
-- You can find more information on persistent and how to declare entities
-- at:
-- http://www.yesodweb.com/book/persistent/
share [mkPersist sqlOnlySettings, mkMigrate "migrateAll"]
    $(persistFileWith lowerCaseSettings "config/models")

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
    } deriving Show

toResult :: (ExitCode, String, String) -> Result
toResult (exit_code, std_out, std_err) = Result
    { resultExitCode = exit_code
    , resultStdout   = std_out
    , resultStderr   = std_err
    }
