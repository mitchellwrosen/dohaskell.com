{-# LANGUAGE OverloadedStrings #-}

-- | Execute a batch of Persistent statements. Provide a models file (probably "config/models"), a database file (like
-- "dohaskell-test.sqlite3"), and a statements file, containing one Persistent statement per line.
--
-- Usage: runhaskell StatementExecutor.hs config/models dohaskell-test.sqlite3 my_statements
module Main where

import           Control.Applicative              ((<$>))
import           Control.Monad                    (unless)
import           Data.ByteString.Lazy.Char8       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as BS
import           System.Environment               (getArgs)
import           System.Exit                      (ExitCode(..), exitFailure)
import           System.IO                        (hClose, hPutStrLn, stderr)
import           System.IO.Temp                   (withTempFile)
import           System.Process                   (readProcessWithExitCode)
import           Text.Hastache                    (MuType(..), defaultConfig, hastacheFile)
import           Text.Hastache.Context            (mkStrContext)

templateFile :: FilePath
templateFile = "mustache-templates/exec-statements.mustache"

main :: IO ()
main = do
    args <- getArgs
    unless (length args == 3) $
        printUsageAndExit
    main' (args !! 0) (args !! 1) (args !! 2)

printUsageAndExit :: IO ()
printUsageAndExit = do
    hPutStrLn stderr $ "Usage: runhaskell StatementExecutor.hs modelsFile databaseFile statementsFile"
    exitFailure

main' :: FilePath -> FilePath -> FilePath -> IO ()
main' modelsFile databaseFile statementsFile = do
    statements <- lines <$> readFile statementsFile

    let context :: Monad m => String -> MuType m
        context "modelsFile"   = MuVariable modelsFile
        context "databaseFile" = MuVariable databaseFile
        context "statements"   = MuList $ map (mkStrContext . mkListContext) statements
          where
            mkListContext s = \"statement" -> MuVariable s

    withTempFile "." "foobar.hs" $ \file handle -> do
        contents <- hastacheFile defaultConfig templateFile (mkStrContext context)
        BS.hPutStr handle contents
        hClose handle

        readProcessWithExitCode "runhaskell" [file] "" >>= handleOutput contents

handleOutput :: ByteString -> (ExitCode, String, String) -> IO ()
handleOutput _        (ExitSuccess, _,    _)    = putStrLn "Success."
handleOutput contents (_,           sout, serr) = mapM_ BS.putStrLn
    [ "-----------------------"
    , "Failure. File contents:"
    , "-----------------------"
    , contents
    , ""
    , "-------"
    , "Stdout:"
    , "-------"
    , BS.pack sout
    , ""
    , "-------"
    , "Stderr:"
    , "-------"
    , BS.pack serr
    ]
