module HaskFunction.Dao
    ( getFunctionById
    , getFunctionByModuleAndName
    , getAllFunctionsFromModule
    , getAllFunctionNamesFromModule
    , getRandomFunctionFromModule
    ) where

import Import

import System.Random.Extras (randomElem)

getFunctionById :: HaskFunctionId -> Handler (Maybe HaskFunction)
getFunctionById function_id = runDB $ get function_id

getFunctionByModuleAndName :: ModuleName -> FunctionName -> Handler (Maybe HaskFunction)
getFunctionByModuleAndName module_name function_name = runDB $ do
    func <- getBy $ UniqueHaskFunction module_name function_name
    return $ entityVal <$> func

getAllFunctionsFromModule :: ModuleName -> Handler [HaskFunction]
getAllFunctionsFromModule module_name = runDB $ do
    functions <- selectList [HaskFunctionModule ==. module_name] [Asc HaskFunctionName]
    return $ entityVal <$> functions

getAllFunctionNamesFromModule :: ModuleName -> Handler [FunctionName]
getAllFunctionNamesFromModule = (haskFunctionName <$$>) . getAllFunctionsFromModule

getRandomFunctionFromModule :: ModuleName -> Handler (Maybe HaskFunction)
getRandomFunctionFromModule module_name =
    getAllFunctionsFromModule module_name >>=
    liftIO . randomElem
