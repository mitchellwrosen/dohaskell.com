module HaskModule.Dao
    ( getAllModules
    , getAllModuleNames
    ) where

import Import

getAllModules :: Handler [HaskModule]
getAllModules = runDB $ entityVal <$$> selectList [] [Asc HaskModuleName]

getAllModuleNames :: Handler [ModuleName]
getAllModuleNames = haskModuleName <$$> getAllModules
