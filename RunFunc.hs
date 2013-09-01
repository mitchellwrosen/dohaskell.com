{-# LANGUAGE OverloadedStrings #-}

module RunFunc where

import Prelude

import Control.Applicative (pure, (<$>), (<*>), (<**>))
import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)
import Data.Monoid ((<>))
import Data.Text (Text)
import System.Directory (removeFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Plugins (LoadStatus(..), MakeStatus(..), Module(..), load_, make, unloadAll)
import Test.QuickCheck (Result)
import Text.Hastache (MuConfig(..), MuType(..), defaultConfig, emptyEscape, hastacheFile)
import Text.Hastache.Context (mkStrContext)

import qualified Data.Text                  as T
import qualified Data.ByteString.Lazy.Char8 as BSL

import DohaskellFunc (DohaskellFunc, runDohaskellFunc)
import FunctionUtils (argsStr, typeSignature)
import Model (Function(..))
import System.Random.Extras (randomModuleName)
import Types (ModuleName)

{-data Function = Function-}
    {-{ functionRealName :: Text-}
    {-, functionUserName :: Text-}
    {-, functionTypeSignature :: Text-}
    {-, functionDocumentation :: Text-}
    {-, functionNumArgs :: Int-}
    {-, functionModules :: [Text]-}
    {-}-}

{-sampleFunc :: Function-}
{-sampleFunc = Function-}
    {-{ functionRealName = "(&&)"-}
    {-, functionUserName = "my_and"-}
    {-, functionTypeSignature = "Bool -> Bool -> Bool"-}
    {-, functionDocumentation = "Boolean and"-}
    {-, functionNumArgs = 2-}
    {-, functionModules = ["Prelude"]-}
    {-}-}

{-userDefinition1 :: Text-}
{-userDefinition1 = "my_and True True = True\nmy_and _ _ = False"-}

{-userDefinition2 :: Text-}
{-userDefinition2 = "my_and _ _ = False"-}

{-debugPrintResult :: DohaskellFunc Result -> IO String-}
{-debugPrintResult res =-}
    {-runDohaskellFunc res >>= \val ->-}
    {-case val of-}
        {-Right result -> return $ "Right: " ++ show result-}
        {-Left  err    -> return $ "Left: "  ++ err-}

-----

runHaskell :: Function -> Text -> DohaskellFunc Result
runHaskell function user_definition = do
    random_module_name <- liftIO $ randomModuleName 20
    (random_module, result) <- setup random_module_name
    liftIO (teardown random_module)
    liftIO result
  where
    setup :: ModuleName -> DohaskellFunc (Module, IO Result)
    setup random_module_name = do
        liftIO $ makeUserFile random_module_name function user_definition
        makeDohaskellModule random_module_name
        loadDohaskellModule random_module_name

    teardown :: Module -> IO ()
    teardown modul = do
        unloadAll modul
        cleanupModule (T.pack $ mname modul)

makeUserFile :: ModuleName -> Function -> Text -> IO ()
makeUserFile module_name function user_definition =
    fillFunctionTemplate module_name function user_definition >>=
    BSL.writeFile (T.unpack $ hsFile module_name)

fillFunctionTemplate :: MonadIO m => ModuleName -> Function -> Text -> m BSL.ByteString
fillFunctionTemplate module_name function user_definition =
    hastacheFile config "mustache-templates/func.mustache" (mkStrContext context)
  where
    config :: MonadIO m => MuConfig m
    config = defaultConfig { muEscapeFunc = emptyEscape }

    context "args"            = MuVariable $ argsStr function
    context "module_name"     = MuVariable   module_name
    context "module"          = MuVariable $ functionModule function
    context "real_name"       = MuVariable $ functionRealName function
    context "type_signature"  = MuVariable $ typeSignature function
    context "user_definition" = MuVariable   user_definition
    context "user_name"       = MuVariable $ functionUserName function

makeDohaskellModule :: ModuleName -> DohaskellFunc ()
makeDohaskellModule module_name =
    liftIO doMakeModule >>= \val ->
    case val of
        MakeSuccess _ _ -> return ()
        MakeFailure errs -> fail $ unlines errs
  where
    doMakeModule :: IO MakeStatus
    doMakeModule = make (T.unpack $ hsFile module_name) []

loadDohaskellModule :: ModuleName -> DohaskellFunc (Module, IO Result)
loadDohaskellModule module_name =
    liftIO doLoadDohaskellModule >>= \val ->
    case val of
        LoadSuccess modul func -> return (modul, func)
        LoadFailure errs -> fail $ unlines errs
  where
    doLoadDohaskellModule :: IO (LoadStatus (IO Result))
    doLoadDohaskellModule = load_ (T.unpack $ module_name <> ".o") [] "dohaskell"

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

hsFile :: T.Text -> T.Text
hsFile basename = basename <> ".hs"

oFile :: T.Text -> T.Text
oFile basename = basename <> ".hs"

hiFile :: T.Text -> T.Text
hiFile basename = basename <> ".hi"
