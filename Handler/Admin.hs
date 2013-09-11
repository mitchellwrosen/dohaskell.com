module Handler.Admin where

import Import

import Data.Text (unpack)
import Safe (readMay)

import Function.Ydao (insertLibFunction)
import Module.Ydao (insertModule)

getAdminR :: Handler Html
getAdminR = do
    (insert_lib_func_widget, insert_lib_func_enctype) <- generateFormPost insertLibFunctionForm
    (insert_module_widget, insert_module_enctype) <- generateFormPost insertModuleForm

    let sample_module = Module {
          moduleName = "Data.Text"
        }

    let sample_func = LibFunction {
          libFunctionName          = "fmap"
        , libFunctionTypeSignature = "(a -> b) -> f a -> f b"
        , libFunctionNumArgs       = 2
        , libFunctionDocumentation = "...documentation..."
        , libFunctionModule        = "Prelude"
        }

    defaultLayout $ do
        $(widgetFile "admin")

postInsertFunctionR :: Handler Html
postInsertFunctionR = do
    ((formResult, _), _) <- runFormPost insertLibFunctionForm
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

postInsertModuleR :: Handler Html
postInsertModuleR = do
    ((formResult, _), _) <- runFormPost insertModuleForm
    case formResult of
        FormSuccess haskModule -> do
            key <- insertModule haskModule
            setMessageRedirect (toHtml $ show key) AdminR
        _ -> setMessageRedirect (toHtml $ show formResult) AdminR
  where
    setMessageRedirect :: RedirectUrl App url => Html -> url -> Handler Html
    setMessageRedirect msg resource = setMessage msg >> redirect resource

insertLibFunctionForm :: Form LibFunction
insertLibFunctionForm = renderDivs $ areq libFunctionField "Function (as 'show')" Nothing

insertModuleForm :: Form Module
insertModuleForm = renderDivs $ areq moduleField "Module (as 'show')" Nothing

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

moduleField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m Module
moduleField = readField
