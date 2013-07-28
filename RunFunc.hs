{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase        #-}

module RunFunc where

import Control.Monad.IO.Class
import Control.Monad.Trans.Either
import System.Extras
import System.Plugins
import Test.QuickCheck
import Text.Hastache
import Text.Hastache.Context

import qualified Data.Text as T

import qualified Data.ByteString.Lazy.Char8 as BSL

-- Monad representing either a failed compilation, or successful compilation/run of loaded Haskell code.
type DohaskellFunc = EitherT String IO Result

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

debugPrintResult :: EitherT String IO Result -> IO String
debugPrintResult res = eitherT onFailure onSuccess res
  where
    onFailure str = return $ "Left: " ++ str
    onSuccess result = return $ "Right: " ++ show result

-----

runHaskell :: ModuleName -> Function -> T.Text -> DohaskellFunc
runHaskell module_name function user_definition = do
    liftIO $ writeModule module_name function user_definition
    makeLoadRun module_name

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

makeLoadRun :: ModuleName -> DohaskellFunc
makeLoadRun module_name =
    liftIO makeModule >>=
    \case
        MakeSuccess _ _ -> loadRun module_name
        MakeFailure errs -> left $ unlines errs
  where
    makeModule :: IO MakeStatus
    makeModule = make (module_name ++ ".hs") []

loadRun :: ModuleName -> DohaskellFunc
loadRun module_name =
    liftIO loadDohaskellSymbol >>=
    \case
        LoadSuccess _ func -> liftIO func >>= right
        LoadFailure errs -> left $ unlines errs
  where
    loadDohaskellSymbol :: IO (LoadStatus (IO Result))
    loadDohaskellSymbol = load_ (module_name ++ ".o") [] "dohaskell"

run :: IO Result -> DohaskellFunc
run func = liftIO func >>= right
