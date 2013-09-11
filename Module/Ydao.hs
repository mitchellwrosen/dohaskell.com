module Module.Ydao
    ( getAllModules
    , getAllModuleNames
    , insertModule
    ) where

import Import

import qualified Module.Dao as D

getAllModules :: Handler [Module]
getAllModules = runDB $ entityVal <$$> D.getAllModules

getAllModuleNames :: Handler [ModuleName]
getAllModuleNames = moduleName <$$> getAllModules

insertModule :: Module -> Handler (Key Module)
insertModule = runDB . insert
