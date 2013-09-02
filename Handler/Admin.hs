module Handler.Admin where

import Import

import Data.Text (unpack)
import Safe (readMay)

import Function.Ydao (insertLibFunction)

getAdminR :: Handler Html
getAdminR = do
    (addLibFunctionFormWidget, addLibFunctionFormEnctype) <- generateFormPost addLibFunctionForm

    let sampleFunction = LibFunction {
          libFunctionName          = "fmap"
        , libFunctionTypes         = ["(a -> b)", "f a", "f b"]
        , libFunctionDocumentation = "...documentation..."
        , libFunctionModule        = "Prelude"
        }

    defaultLayout
        [whamlet|
            <h3>Add Function

            <div>Sample function:
            <div>#{show sampleFunction}
            <form method=post action=@{AddFunctionR} enctype=#{addLibFunctionFormEnctype}>
                ^{addLibFunctionFormWidget}
                <input type=submit value="Submit">

            <form method=post action=@{ListModulesR}>
                <input type=submit action=@{ListModulesR} value="List Modules">
        |]

postAddFunctionR :: Handler Html
postAddFunctionR = do
    ((formResult, _), _) <- runFormPost addLibFunctionForm
    case formResult of
        FormSuccess function -> do
            key <- insertLibFunction function
            setMessageRedirect (toHtml $ show key) AdminR
        _ -> setMessageRedirect (toHtml $ show formResult) AdminR
  where
    setMessageRedirect :: RedirectUrl App url => Html -> url -> Handler Html
    setMessageRedirect msg resource = setMessage msg >> redirect resource

postListModulesR :: Handler Html
postListModulesR = do
    setMessage $ toHtml ("Modules: TODO" :: Text)
    redirect AdminR

addLibFunctionForm :: Form LibFunction
addLibFunctionForm = renderDivs $ areq libFunctionField "Function (as 'show')" Nothing

readField :: (Monad m, RenderMessage (HandlerSite m) FormMessage, Read a, Show a) => Field m a
readField = Field
    { fieldParse = parseHelper $ maybe (Left MsgValueRequired) Right . readMay . unpack
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
            $newline never
            <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either (const "") show val}">
        |]
    , fieldEnctype = UrlEncoded
    }

libFunctionField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m LibFunction
libFunctionField = readField
