module Function.Dao
    ( getAllLibFunctionsFromModule
    ) where

import Import

import Database.Persist.Sql (SqlBackend)

-- TODO: Get rid of SqlBackend
getAllLibFunctionsFromModule :: (PersistQuery m, PersistMonadBackend m ~ SqlBackend) => ModuleName -> m [Entity LibFunction]
getAllLibFunctionsFromModule module_name = selectList [LibFunctionModule ==. module_name] [Asc LibFunctionName]
