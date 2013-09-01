{-# LANGUAGE OverloadedStrings #-}

module RunFunc where

import Control.Exception (throwIO)
import Control.Monad.Trans (MonadIO, liftIO)
import System.Directory (removeFile)
import System.IO.Error (catchIOError, isDoesNotExistError)
import System.Plugins (LoadStatus(..), MakeStatus(..), Module, load_, make, unloadAll)
import Test.QuickCheck (Result)
import Text.Hastache (MuConfig(..), MuType(..), MuContext, defaultConfig, emptyEscape, hastacheFile)
import Text.Hastache.Context (mkStrContext)

import qualified Data.Text as T
import qualified Data.ByteString.Lazy.Char8 as BSL

import DohaskellFunc (DohaskellFunc, runDohaskellFunc)

type ModuleName = String

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
    runDohaskellFunc res >>= val -> val ->
   case val of val of
        Right result -> return $ "Right: " ++ show result
        Left  err    -> return $ "Left: "  ++ err

-----

runHaskell :: ModuleName -> Function -> T.Text -> DohaskellFunc Result
runHaskell module_name function user_definition = do
    liftIO (writeModule module_name function user_definition)
    makeDohaskellModule module_name
    (modul, func) <- loadDohaskellModule module_name
    result <- runDohaskell func
    liftIO $ unloadAll modul
    return result

writeModule :: ModuleName -> Function -> T.Text -> IO ()
writeModule module_name function user_definition =
    fillFunctionTemplate module_name function user_definition >>=
    BSL.writeFile (module_name ++ ".hs")

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
    liftIO doMakeModule >>= val ->
    case val of
        MakeSuccess _ _ -> return ()
        MakeFailure errs -> fail $ unlines errs
  where
    doMakeModule :: IO MakeStatus
    doMakeModule = make (module_name ++ ".hs") []

loadDohaskellModule :: ModuleName -> DohaskellFunc (Module, IO Result)
loadDohaskellModule module_name =
    liftIO doLoadDohaskellModule >>= val ->
    case val of
        LoadSuccess modul func -> return (modul, func)
        LoadFailure errs -> fail $ unlines errs
  where
    doLoadDohaskellModule :: IO (LoadStatus (IO Result))
    doLoadDohaskellModule = load_ (module_name ++ ".o") [] "dohaskell"

runDohaskell :: IO Result -> DohaskellFunc Result
runDohaskell func = liftIO func >>= return
