module Handler.Admin where

import Import

import Data.Text (split)

getAdminR :: Handler Html
getAdminR = do
    (addFunctionFormWidget, addFunctionFormEnctype) <- generateFormPost addFunctionForm

    defaultLayout
        [whamlet|
            <h3>Add Function
            <form method=post action=@{AddFunctionR} enctype=#{addFunctionFormEnctype}>
                ^{addFunctionFormWidget}
                <input type=submit value="Submit">

            <form method=post action=@{ListModulesR}>
                <input type=submit action=@{ListModulesR} value="List Modules">
        |]

postAddFunctionR :: Handler Html
postAddFunctionR = do
    ((formResult, _), _) <- runFormPost addFunctionForm
    setMessage $ toHtml $ show formResult
    redirect AdminR

postListModulesR :: Handler Html
postListModulesR = do
    setMessage $ toHtml ("Modules: TODO" :: Text)
    redirect AdminR

addFunctionForm :: Form HaskFunction
addFunctionForm = renderDivs $ HaskFunction
    <$> areq textField "Module"                  Nothing
    <*> areq textField "Name"                    Nothing
    <*> areq textField "User name"               Nothing
    <*> areq listField "Types (comma delimited)" Nothing
    <*> areq textField "Documentation"           Nothing

listField :: (Monad m, RenderMessage (HandlerSite m) FormMessage) => Field m [Text]
listField = Field
    { fieldParse = parseHelper $ Right . splitOnComma
    , fieldView = \theId name attrs val isReq ->
        [whamlet|
            $newline never
            <input id="#{theId}" name="#{name}" *{attrs} type="text" :isReq:required value="#{either (const "") show val}">
        |]
    , fieldEnctype = UrlEncoded
    }
  where
    splitOnComma :: Text -> [Text]
    splitOnComma = split (== ',')
