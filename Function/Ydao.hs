module Function.Ydao
    ( getLibFunction
    , getUserFunction
    , getLibFunctionByModuleAndName
    , getAllLibFunctionsFromModule
    , getAllLibFunctionNamesFromModule
    , getRandomLibFunctionFromModule
    , insertLibFunction
    ) where

import Import

import System.Random.Extras (randomElem)

import qualified Function.Dao as D

getAllLibFunctionsFromModule :: ModuleName -> Handler [LibFunction]
getAllLibFunctionsFromModule module_name = runDB $ do
    functions <- D.getAllLibFunctionsFromModule module_name
    return $ entityVal <$> functions

getAllLibFunctionNamesFromModule :: ModuleName -> Handler [FunctionName]
getAllLibFunctionNamesFromModule = (libFunctionName <$$>) . getAllLibFunctionsFromModule

getLibFunction :: LibFunctionId -> Handler LibFunction
getLibFunction = runDB . get404

getLibFunctionByModuleAndName :: ModuleName -> FunctionName -> Handler LibFunction
getLibFunctionByModuleAndName module_name function_name = runDB $ do
    func <- getBy404 $ UniqueLibFunction module_name function_name
    return $ entityVal func

getRandomLibFunctionFromModule :: ModuleName -> Handler (Maybe LibFunction)
getRandomLibFunctionFromModule module_name =
    getAllLibFunctionsFromModule module_name >>=
    liftIO . randomElem

getUserFunction :: UserFunctionId -> Handler UserFunction
getUserFunction = runDB . get404

insertLibFunction :: LibFunction -> Handler (Key LibFunction)
insertLibFunction = runDB . insert
