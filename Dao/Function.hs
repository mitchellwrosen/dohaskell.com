module Dao.Function where

import Import

import System.Random.Extras (randomElem)

getFunctionById :: FunctionId -> Handler (Maybe Function)
getFunctionById function_id = runDB $ get function_id

getFunctionByModuleAndName :: ModuleName -> FunctionName -> Handler (Maybe Function)
getFunctionByModuleAndName module_name function_name = runDB $ do
    maybe_function <- getBy $ UniqueFunction module_name function_name
    case maybe_function of
        Nothing -> return Nothing
        Just (Entity _ function) -> return $ Just function

getFunctionsFromModule :: ModuleName -> Handler [Function]
getFunctionsFromModule module_name = runDB $ do
    functions <- selectList [FunctionModule ==. module_name] [Asc FunctionName]
    return $ map entityVal functions

getRandomFunctionFromModule :: ModuleName -> Handler (Maybe Function)
getRandomFunctionFromModule module_name =
    getFunctionsFromModule module_name >>=
    liftIO . randomElem
