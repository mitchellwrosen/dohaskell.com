module Handler.Executor where

import Import

import CodeExecutor (runghc)

getExecutorR :: Handler Html
getExecutorR = do
    ((formResult, widget), enctype) <- runFormPost executeForm
    case formResult of
        FormSuccess textArea -> do
            result <- liftIO (runghc (unTextarea textArea))
            let maybeResult = Just result
            defaultLayout $(widgetFile "executor")
        _ -> do
            let maybeResult = Nothing
            defaultLayout $(widgetFile "executor")

postExecutorR :: Handler Html
postExecutorR = getExecutorR

executeForm :: Form Textarea
executeForm = renderDivs $ areq textareaField "" Nothing
