module Dao.Function where

import Import

getFunctionById :: FunctionId -> IO (Maybe Function)
getFunctionById = undefined
{-getFunctionById function_id = get function_id-}

getFunctionByModuleAndName :: ModuleName -> FunctionName -> IO (Maybe Function)
getFunctionByModuleAndName = undefined
{-getFunctionByModuleAndName module_name function_name = runDB $ do-}
    {-maybe_function <- getBy $ UniqueFunction module_name function_name-}
    {-case maybe_function of-}
        {-Nothing -> return Nothing-}
        {-Just (Entity _ function) -> return function-}

{-getRandomFunctionFromModule :: ModuleName -> IO [Entity Function]-}
{-getRandomFunctionFromModule module_name = do-}
    {-funcs <- runDB $ selectList [FunctionModule ==. module_name] []-}
    {-if null funcs-}
        {-then return Nothing-}
        {-else return $ Just $ (funcs !!) <$> liftIO (randomRIO (0, length funcs - 1))-}
