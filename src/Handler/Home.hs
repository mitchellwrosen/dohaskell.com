{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Function.Ydao (getAllLibFunctionNamesFromModule)
import Module.Ydao (getAllModuleNames)
import Settings.StaticFiles

import qualified Data.Map      as M

data DoFunction = DoFunction String
data DoModule = DoModule String [DoFunction]

type ModuleMap = M.Map ModuleName [FunctionName]

getHomeR :: Handler Html
getHomeR = do
    sidebarClass <- newIdent
    moduleMap <- makeModuleMap

    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        setTitle "Welcome To DoHaskell!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = getHomeR

makeModuleMap :: Handler ModuleMap
makeModuleMap = do
    module_names <- getAllModuleNames
    function_names <- mapM getAllLibFunctionNamesFromModule module_names
    return $ M.fromList (zip module_names function_names)
