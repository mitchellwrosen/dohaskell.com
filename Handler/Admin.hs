module Handler.Admin where

import Import

import Data.Text (unpack)
import Safe (readMay)

import HaskFunction.Dao (insertFunction)

getAdminR :: Handler Html
getAdminR = do
    (addFunctionFormWidget, addFunctionFormEnctype) <- generateFormPost addFunctionForm

    let sampleFunction = HaskFunction { 
          haskFunctionModule        = "Prelude"
        , haskFunctionName          = "fmap"
        , haskFunctionUserName      = "my_fmap"
        , haskFunctionTypes         = ["(a -> b)", "f a", "f b"]
        , haskFunctionDocumentation = "...documentation..."
        }

    defaultLayout
        [whamlet|
            <h3>Add Function

            <div>Sample function:
            <div>#{show sampleFunction}
            <form method=post action=@{AddFunctionR} enctype=#{addFunctionFormEnctype}>
                ^{addFunctionFormWidget}
                <input type=submit value="Submit">

            <form method=post action=@{ListModulesR}>
                <input type=submit action=@{ListModulesR} value="List Modules">
        |]

postAddFunctionR :: Handler Html
postAddFunctionR = do
    ((formResult, _), _) <- runFormPost addFunctionForm
    case formResult of
        FormSuccess function -> do
            key <- insertFunction function
            setMessageRedirect (toHtml $ show key) AdminR
        _ -> setMessageRedirect (toHtml $ show formResult) AdminR
  where
    setMessageRedirect :: RedirectUrl App url => Html -> url -> Handler Html
    setMessageRedirect msg resource = setMessage msg >> redirect resource

postListModulesR :: Handler Html
postListModulesR = do
    setMessage $ toHtml ("Modules: TODO" :: Text)
    redirect AdminR

addFunctionForm :: Form HaskFunction
addFunctionForm = renderDivs $ areq haskFunctionField "Function (as 'show')" Nothing

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

haskFunctionField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m HaskFunction
haskFunctionField = readField
