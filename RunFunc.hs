{-# LANGUAGE OverloadedStrings #-}

module RunFunc {-# DEPRECATED "Use CodeExecutor instead" #-} where

import Import hiding (Module)
import Prelude

import Control.Exception (throwIO)
import System.Directory (removeFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Plugins (LoadStatus(..), MakeStatus(..), Module(..), load_, make, unloadAll)
import Text.Hastache (MuConfig(..), MuType(..), defaultConfig, emptyEscape, hastacheFile)
import Text.Hastache.Context (mkStrContext)

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BSL
import qualified Test.QuickCheck as QC

import DohaskellFunc (DohaskellFunc)
import Function.Utils (argsStr, typeSignature, toUserName)
import System.Random.Extras (randomModuleName)

{-data Function = Function-}
    {-{ functionRealName :: Text-}
    {-, functionUserName :: Text-}
    {-, functionTypeSignature :: Text-}
    {-, functionDocumentation :: Text-}
    {-, functionNumArgs :: Int-}
    {-, functionModules :: [Text]-}
    {-}-}

sampleFunc :: LibFunction
sampleFunc = LibFunction
    { libFunctionName = "(&&)"
    , libFunctionTypes = ["Bool", "Bool", "Bool"]
    , libFunctionDocumentation = "Boolean and"
    , libFunctionModule = "Prelude"
    }

userDefinition1 :: Text
userDefinition1 = "my_and True True = True\nmy_and _ _ = False"

userDefinition2 :: Text
userDefinition2 = "my_and _ _ = False"

userDefinition3 :: Text
userDefinition3 = "foo"

-----

runHaskell :: LibFunction -> Text -> DohaskellFunc QC.Result
runHaskell function user_definition = do
    random_module_name <- liftIO $ randomModuleName 20
    (random_module, func) <- setup random_module_name
    liftIO (teardown random_module)
    liftIO func
  where
    setup :: ModuleName -> DohaskellFunc (Module, IO QC.Result)
    setup random_module_name = do
        liftIO $ makeUserFile random_module_name function user_definition
        makeDohaskellModule random_module_name
        loadDohaskellModule random_module_name

    teardown :: Module -> IO ()
    teardown modul = do
        unloadAll modul
        cleanupModule (T.pack $ mname modul)

makeUserFile :: ModuleName -> LibFunction -> Text -> IO ()
makeUserFile module_name function user_definition =
    fillFunctionTemplate module_name function user_definition >>=
    BSL.writeFile (T.unpack $ hsFile module_name)

fillFunctionTemplate :: MonadIO m => ModuleName -> LibFunction -> Text -> m BSL.ByteString
fillFunctionTemplate module_name function user_definition =
    hastacheFile config "mustache-templates/func.mustache" (mkStrContext context)
  where
    config :: MonadIO m => MuConfig m
    config = defaultConfig { muEscapeFunc = emptyEscape }

    context "args"            = MuVariable $ argsStr (LibF function)
    context "module_name"     = MuVariable   module_name
    context "module"          = MuVariable $ libFunctionModule function
    context "name"            = MuVariable $ libFunctionName function
    context "type_signature"  = MuVariable $ typeSignature (LibF function)
    context "user_definition" = MuVariable   user_definition
    context "user_name"       = MuVariable $ toUserName $ libFunctionName $ function

makeDohaskellModule :: ModuleName -> DohaskellFunc ()
makeDohaskellModule module_name =
    liftIO doMakeModule >>= \val ->
    case val of
        MakeSuccess _ _ -> return ()
        MakeFailure errs -> fail $ unlines errs
  where
    doMakeModule :: IO MakeStatus
    doMakeModule = make (T.unpack $ hsFile module_name) []

loadDohaskellModule :: ModuleName -> DohaskellFunc (Module, IO QC.Result)
loadDohaskellModule module_name =
    liftIO doLoadDohaskellModule >>= \val ->
    case val of
        LoadSuccess modul func -> return (modul, func)
        LoadFailure errs -> fail $ unlines errs
  where
    doLoadDohaskellModule :: IO (LoadStatus (IO QC.Result))
    doLoadDohaskellModule = load_ (T.unpack $ oFile module_name) [] "dohaskell"

cleanupModule :: ModuleName -> IO ()
cleanupModule module_name =
    mapM_ (removeFileIfExists . T.unpack) $
        pure module_name <**> [hsFile, oFile, hiFile]

removeFileIfExists :: FilePath -> IO ()
removeFileIfExists file_path = removeFile file_path `catchIOError` handler
  where
    handler :: IOError -> IO ()
    handler err
        | isDoesNotExistError err = return ()
        | otherwise = throwIO err

hsFile :: Text -> Text
hsFile = (<> ".hs")

oFile :: Text -> Text
oFile = (<> ".o")

hiFile :: Text -> Text
hiFile = (<> ".hi")
