{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import

import Data.Text (toUpper)

getHomeR :: Handler Html
getHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm

    case result of
        FormSuccess text -> do
            submission <- liftIO $ handleSubmission (unTextarea text)
            defaultLayout $ do
                setTitle "Do Haskell"
                $(widgetFile "homepage")
        _ -> do
            let submission = Nothing :: Maybe Textarea
            defaultLayout $ do
                setTitle "Do Haskell"
                $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = getHomeR

sampleForm :: Form Textarea
sampleForm = renderDivs $ areq textareaField "Enter some text" Nothing

handleSubmission :: Text -> IO (Maybe Textarea)
handleSubmission = return . Just . Textarea . toUpper
