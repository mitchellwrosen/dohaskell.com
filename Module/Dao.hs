module Module.Dao
    ( getAllModules
    ) where

import Import

import Database.Persist.Sql (SqlBackend)

getAllModules :: (PersistQuery m, PersistMonadBackend m ~ SqlBackend) => m [Entity Module]
getAllModules = selectList [] [Asc ModuleName]
