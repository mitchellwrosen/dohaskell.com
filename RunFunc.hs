{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module RunFunc where

import Prelude

import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory (removeFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Plugins (LoadStatus(..), MakeStatus(..), Module, load_, make, unloadAll)
import Test.QuickCheck (Result)
import Text.Hastache (MuConfig(..), MuType(..), defaultConfig, emptyEscape, hastacheFile)
import Text.Hastache.Context (mkStrContext)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL

import DohaskellFunc (DohaskellFunc, runDohaskellFunc)
import Types (ModuleName)

data Function = Function
    { functionRealName :: T.Text
    , functionUserName :: T.Text
    , functionTypeSignature :: T.Text
    , functionDocumentation :: T.Text
    , functionNumArgs :: Int
    , functionModules :: [T.Text]
    }

sampleFunc :: Function
sampleFunc = Function
    { functionRealName = "(&&)"
    , functionUserName = "my_and"
    , functionTypeSignature = "Bool -> Bool -> Bool"
    , functionDocumentation = "Boolean and"
    , functionNumArgs = 2
    , functionModules = ["Prelude"]
    }

userDefinition1 :: T.Text
userDefinition1 = "my_and True True = True\nmy_and _ _ = False"

userDefinition2 :: T.Text
userDefinition2 = "my_and _ _ = False"

debugPrintResult :: DohaskellFunc Result -> IO String
debugPrintResult res =
    runDohaskellFunc res >>=
    \case
        Right result -> return $ "Right: " ++ show result
        Left  err    -> return $ "Left: "  ++ err

-----

runHaskell :: ModuleName -> Function -> T.Text -> DohaskellFunc Result
runHaskell module_name function user_definition = do
    (modul, func) <- setup
    liftIO $ teardown modul
    liftIO func
  where
    setup :: DohaskellFunc (Module, IO Result)
    setup = do
        liftIO $ writeModule module_name function user_definition
        makeDohaskellModule module_name
        loadDohaskellModule module_name

    teardown :: Module -> IO ()
    teardown modul =
        unloadAll modul >>
        cleanupModule module_name

writeModule :: ModuleName -> Function -> T.Text -> IO ()
writeModule module_name function user_definition =
    fillFunctionTemplate module_name function user_definition >>=
    BSL.writeFile (T.unpack $ hsFile module_name)

fillFunctionTemplate :: MonadIO m => ModuleName -> Function -> T.Text -> m BSL.ByteString
fillFunctionTemplate module_name function user_definition =
    hastacheFile config "mustache-templates/func.mustache" (mkStrContext context)
  where
    config :: MonadIO m => MuConfig m
    config = defaultConfig { muEscapeFunc = emptyEscape }

    context "args"            = MuVariable $ numArgsToArgsStr $ functionNumArgs function
    context "module_name"     = MuVariable $ module_name
    context "modules"         = MuList     $ map (mkStrContext . mkListContext) (functionModules function)
    context "real_name"       = MuVariable $ functionRealName function
    context "type_signature"  = MuVariable $ functionTypeSignature function
    context "user_definition" = MuVariable $ user_definition
    context "user_name"       = MuVariable $ functionUserName function

    mkListContext module_name = \"module" -> MuVariable module_name

    -- 3 -> "arg1 arg2 arg3"
    numArgsToArgsStr :: Int -> String
    numArgsToArgsStr n = unwords $ map (\(arg,num) -> arg ++ show num) tups
      where
        tups :: [(String, Int)]
        tups = zip (repeat "arg") [1..n]

makeDohaskellModule :: ModuleName -> DohaskellFunc ()
makeDohaskellModule module_name =
    liftIO doMakeModule >>=
    \case
        MakeSuccess _ _ -> return ()
        MakeFailure errs -> fail $ unlines errs
  where
    doMakeModule :: IO MakeStatus
    doMakeModule = make (T.unpack $ hsFile module_name) []

loadDohaskellModule :: ModuleName -> DohaskellFunc (Module, IO Result)
loadDohaskellModule module_name =
    liftIO doLoadDohaskellModule >>=
    \case
        LoadSuccess modul func -> return (modul, func)
        LoadFailure errs -> fail $ unlines errs
  where
    doLoadDohaskellModule :: IO (LoadStatus (IO Result))
    doLoadDohaskellModule = load_ (T.unpack $ module_name `T.append` ".o") [] "dohaskell"

cleanupModule :: ModuleName -> IO ()
cleanupModule module_name = do
    removeFileIfExists $ T.unpack $ hsFile module_name
    removeFileIfExists $ T.unpack $ oFile  module_name
    removeFileIfExists $ T.unpack $ hiFile module_name

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file_path = removeFile file_path `catchIOError` handler
  where
    handler :: IOError -> IO ()
    handler err
        | isDoesNotExistError err = return ()
        | otherwise = throwIO err

hsFile :: T.Text -> T.Text
hsFile = (`T.append` ".hs")

oFile :: T.Text -> T.Text
oFile = (`T.append` ".hs")

hiFile :: T.Text -> T.Text
hiFile = (`T.append` ".hi")
