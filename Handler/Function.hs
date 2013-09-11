module Handler.Function where

import Import

import CodeExecutor (runLibFunction)
import Data.Text (unpack)
import Function.Utils (libFunctionToText)
import Function.Ydao (getLibFunctionByModuleAndName)

codeForm :: Form Textarea
codeForm = renderDivs $ areq textareaField "" (Just $ Textarea "-- Insert code here:")

getFunctionR :: ModuleName -> FunctionName -> Handler Html
getFunctionR module_name function_name = do
    func <- getLibFunctionByModuleAndName module_name function_name
    ((formResult, widget), enctype) <- runFormPost codeForm
    case formResult of
        FormSuccess text -> do
            result <- liftIO $ runLibFunction func (unTextarea text)

            let maybeResult = Just result

            defaultLayout $ do
                addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                addScript $ StaticR codemirror_lib_codemirror_js
                addStylesheet $ StaticR codemirror_lib_codemirror_css
                addScript $ StaticR codemirror_haskell_haskell_js
                setTitle "DoHaskell!"
                $(widgetFile "function")
        _ -> do
            let maybeResult = Nothing

            defaultLayout $ do
                addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
                addScript $ StaticR codemirror_lib_codemirror_js
                addStylesheet $ StaticR codemirror_lib_codemirror_css
                addScript $ StaticR codemirror_haskell_haskell_js
                setTitle "DoHaskell!"
                $(widgetFile "function")


postFunctionR :: ModuleName -> FunctionName -> Handler Html
postFunctionR = getFunctionR
