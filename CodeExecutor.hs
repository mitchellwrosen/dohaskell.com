{-# LANGUAGE OverloadedStrings #-}

module CodeExecutor
    ( runghc
    , runghcDebug
    ) where

import Import

import Data.Text (pack)
import Data.Text.IO (hPutStr)
import System.Exit (ExitCode)
import System.IO (hClose)
import System.IO.Temp (withTempFile)
import System.Process (readProcessWithExitCode)


-- | Compiles and runs Haskell code from Text, after prepending the Safe language pragma.
runghc :: Text -> IO Result
runghc code = withTempFileContents safe_code runghc'
  where
    safe_code :: Text
    safe_code = "{-# LANGUAGE Safe #-}\n" <> code

    runghc' :: FilePath -> IO Result
    runghc' file_path = toResult <$> readProcessWithExitCode "/usr/bin/runghc" [file_path] ""

-- | Writes @contents@ to a temp file, and then executes @action@ on it.
withTempFileContents :: Text -> (FilePath -> IO a) -> IO a
withTempFileContents contents action = withTempFile "." "dohaskell.hs" writeContentsAndDoAction
  where
    writeContentsAndDoAction file_path handle = do
        hPutStr handle contents
        hClose handle
        action file_path

runghcDebug :: Text -> IO Text
runghcDebug code = do
    result <- runghc code
    return $
        "Exit code: " <> (pack . show) (resultExitCode result) <> "\n" <>
        "Stdout: "    <> pack          (resultStdout   result) <> "\n" <>
        "Stderr: "    <> pack          (resultStderr   result) <> "\n"
