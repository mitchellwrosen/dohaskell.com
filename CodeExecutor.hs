{-# LANGUAGE OverloadedStrings #-}

module CodeExecutor
    ( runLibFunction
    , runghc
    , runghcSafe
    ) where

import Import

import Data.Text (pack, unpack)
import Data.Text.IO (hPutStr)
import System.Directory (removeFile, doesFileExist)
import System.FilePath.Posix (takeBaseName)
import System.IO (SeekMode(AbsoluteSeek), hClose, hSeek)
import System.IO.Temp (openTempFile, withTempFile)
import System.Process (readProcessWithExitCode)

import Function.Utils (libFunctionToText)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- | QuickChecks a library function against a user's definition, after applying the Safe pragma to it.
runLibFunction :: LibFunction -> Text -> IO Result
runLibFunction func user_code = withTempSafeModuleContents user_code action
  where
    action :: FilePath -> IO Result
    action user_filename = libFunctionToText func (takeBaseName' user_filename) >>= runghc

-- | Writes Haskell code @contents@ to a temp file, prepending the code with the Safe pragma and a module statement that
-- places the code in the same module as the file name. Note that the temp file template thus MUST begin with a capital
-- letter, and not contain any characters would be illegal as a module name.
withTempSafeModuleContents :: Text -> (FilePath -> IO a) -> IO a
withTempSafeModuleContents contents action = withTempFile "." "Dohaskell.hs" (writeContentsAndDoAction . pack)
  where
    writeContentsAndDoAction file_path handle = do
        hPutStr handle $ makeSafe (takeBaseName' file_path) contents
        hClose handle
        action file_path

-- | Place @code@ inside @module_name@ with the Safe pragma.
-- TODO: Handle module statement and language pragmas gracefully. Right now, language pragmas are completely ignored,
-- and a module statement will cause a compiler error (because it becomes two).
makeSafe :: ModuleName -> Text -> Text
makeSafe module_name code = mconcat ["{-# LANGUAGE Safe #-}\nmodule ", module_name, " where\n", code]

-- | System.FilePath.Posix.takeBaseName, but for Text.
takeBaseName' :: Text -> Text
takeBaseName' = pack . takeBaseName . unpack

-- | Writes @contents@ to a temp file, and then executes @action@ on it.
withTempFileContents :: Text -> (FilePath -> IO a) -> IO a
withTempFileContents contents action = withTempFile "." "dohaskell" (writeContentsAndDoAction . pack)
  where
    writeContentsAndDoAction file_path handle = do
        hPutStr handle contents
        hClose handle
        action file_path

-- | Compiles and runs Haskell code from @code@.
runghc :: Text -> IO Result
runghc code = withTempFileContents code runghcFile

-- | Compiles and runs Haskell code from @file_path@.
runghcFile :: FilePath -> IO Result
runghcFile file_path = toResult <$> readProcessWithExitCode "/usr/bin/runghc" [unpack file_path] ""

-- | Compiles and runs Haskell code from Text, after prepending the Safe language pragma.
runghcSafe :: Text -> IO Result
runghcSafe = runghc . ("{-# LANGUAGE Safe #-}\n" <>)
