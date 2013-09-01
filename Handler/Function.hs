module Handler.Function where

import Import

getFunctionR :: ModuleName -> FunctionName -> Handler Html
getFunctionR moduleName functionName = do
    addScriptRemote "hello.jpg"
    {-addScript $ StaticR codemirror_lib_codemirror_js-}
    --addStylesheet $ StaticR codemirror_lib_codemirror_css
    --addScript $ StaticR codemirror_haskell_haskell_js
    defaultLayout $ do
        setTitle "DoHaskell!"
        $(widgetFile "function")

postFunctionR :: ModuleName -> FunctionName -> Handler Html
postFunctionR = getFunctionR
