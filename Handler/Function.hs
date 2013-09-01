module Handler.Function where

import Import

codeForm :: Form Textarea
codeForm = renderDivs $ areq textareaField "" (Just $ Textarea "-- Insert code here:")

getFunctionR :: ModuleName -> FunctionName -> Handler Html
getFunctionR moduleName functionName = do
    (codeAreaWidget, enctype) <- generateFormPost codeForm
    defaultLayout $ do
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        addScript $ StaticR codemirror_lib_codemirror_js
        addStylesheet $ StaticR codemirror_lib_codemirror_css
        addScript $ StaticR codemirror_haskell_haskell_js
        setTitle "DoHaskell!"
        $(widgetFile "function")

postFunctionR :: ModuleName -> FunctionName -> Handler Html
postFunctionR = getFunctionR
