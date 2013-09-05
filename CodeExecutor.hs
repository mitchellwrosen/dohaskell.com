{-# LANGUAGE OverloadedStrings #-}

module CodeExecutor
    ( runLibFunction
    , runghc
    , runghcSafe
    , runghcSafeDebug
    ) where

import Import

import Data.Text (pack, unpack)
import Data.Text.IO (hPutStr)
import System.Directory (removeFile, doesFileExist)
import System.IO (SeekMode(AbsoluteSeek), hClose, hSeek)
import System.IO.Temp (openTempFile, withTempFile)
import System.Process (readProcessWithExitCode)

import Function.Utils (libFunctionToText)

import qualified Data.Text    as T
import qualified Data.Text.IO as T

-- | QuickChecks a library function against a user's definition, after applying the Safe pragma to it.
runLibFunction :: LibFunction -> Text -> IO Result
runLibFunction func user_code = do
    withTempSafeModuleContents user_code (\user_filename -> do
        main_code <- libFunctionToText func ((T.takeWhile (/= '.') . T.drop 2) user_filename) -- drop './'
        runghc main_code)

    (file_path, handle) <- openTempFile "." "Dohaskell.hs"
    putStrLn $ "file_path :::::::: " <> file_path
    let module_name = (T.init . T.init . T.init) (T.drop 2 (pack file_path)) -- drop './'
    let user_code' = mconcat ["{-# LANGUAGE Safe #-}\nmodule ", module_name, " where\n", user_code]
    hPutStr handle user_code'
    hClose handle

    exists <- doesFileExist file_path
    putStrLn $ "Does " ++ file_path ++ " exist: " ++ show exists

    main_code <- libFunctionToText func module_name
    result <- runghc main_code
    removeFile file_path
    return result


-- | Writes @contents@ to a temp file, and then executes @action@ on it.
withTempFileContents :: Text -> (FilePath -> IO a) -> IO a
withTempFileContents contents action = withTempFile "." "dohaskell" (writeContentsAndDoAction . pack)
  where
    writeContentsAndDoAction file_path handle = do
        T.putStrLn $ file_path <> "\n----------\n" <> contents <> "\n=========="
        hPutStr handle contents
        hSeek handle AbsoluteSeek 0
        --hClose handle
        action file_path

-- | Writes Haskell code @contents@ to a temp file, prepending the code with the Safe pragma and a module statement that
-- places the code in the same module as the file name. Note that the temp file template thus MUST begin with a capital
-- letter, and not contain any characters would be illegal as a module name.
withTempSafeModuleContents :: Text -> (FilePath -> IO a) -> IO a
withTempSafeModuleContents contents action = withTempFile "." "Dohaskell.hs" (writeContentsAndDoAction . pack)
  where
    writeContentsAndDoAction file_path handle = do
        T.putStrLn $ file_path <> "\n----------\n" <> contents' <> "\n=========="
        hPutStr handle contents'
        --hClose handle
        action file_path
      where
        contents' :: Text
        contents' = mconcat ["{-# LANGUAGE Safe #-}\nmodule ", T.drop 2 file_path, " where\n", contents]

-- | Compiles and runs Haskell code from @code@.
runghc :: Text -> IO Result
runghc code = withTempFileContents code runghcFile

-- | Compiles and runs Haskell code from @file_path@.
runghcFile :: FilePath -> IO Result
runghcFile file_path = toResult <$> readProcessWithExitCode "/usr/bin/runghc" [unpack file_path] ""

-- | Compiles and runs Haskell code from Text, after prepending the Safe language pragma.
runghcSafe :: Text -> IO Result
runghcSafe = runghc . ("{-# LANGUAGE Safe #-}\n" <>)

runghcSafeDebug :: Text -> IO Text
runghcSafeDebug code = do
    result <- runghc code
    return $
        "Exit code: " <> (pack . show) (resultExitCode result) <> "\n" <>
        "Stdout: "    <> pack          (resultStdout   result) <> "\n" <>
        "Stderr: "    <> pack          (resultStderr   result) <> "\n"
