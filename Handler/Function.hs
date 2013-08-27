module Handler.Function where

import Import

import Data.Text (toUpper)
import System.Random (randomRIO)
import System.Random.Extras (randomModuleName)

import RunFunc (runHaskell)
import DohaskellFunc (runDohaskellFunc)

getFunctionR :: ModuleName -> Handler Html
getFunctionR module_name = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm

    case result of
        FormSuccess text -> do
            submission <- liftIO $ handleSubmission module_name (unTextarea text)
            let func_id = 1 :: Int
            defaultLayout $ do
                setTitle "Do Haskell"
                $(widgetFile "function")
        _ -> do
            let submission = Nothing :: Maybe Textarea

            {-func <- getRandomFunctionFromModule module_name-}

            let func_id = 1 :: Int

            defaultLayout $ do
                setTitle "Do Haskell"
                $(widgetFile "function")

postFunctionR :: ModuleName -> Handler Html
postFunctionR = getFunctionR

functionForm :: ModuleName -> Form (Maybe (Entity Function))
functionForm module_name = do
    maybe_func <- getRandomFunctionFromModule module_name

    let widget = toWidget
        [whamlet|


    case maybe_func of
        Nothing              -> return Nothing
        Just (Entity _ func) ->
sampleForm :: Form Textarea
sampleForm = renderDivs $ areq textareaField "Enter some text" Nothing

{-handleSubmission :: Text -> IO (Maybe Textarea)-}
handleSubmission module_name user_definition = undefined
    {-func <- getRandomFunctionFromModule module_name-}
    {-random_module_name <- randomModuleName 20 -- length 20-}
    {-either_result <- runDohaskellFunc $-}
        {-runHaskell random_module_name func user_definition-}
    {-case either_result of-}
        {-Left err -> return $ "Error: " ++ err-}
        {-Right result -> return $ "Success: " ++ show result-}

functionWidget :: ModuleName -> Widget
functionWidget module_name = do
    {-Entity key func <- handlerToWidget $ getRandomFunctionFromModule module_name-}
    maybe_entity <- handlerToWidget <$> getRandomFunctionFromModule module_name

    [whamlet|
        <p>key = #{show key}
        <p>func = #{show func}
    |]

{-getRandomFunctionFromModule :: ModuleName -> IO [Entity Function]-}
getRandomFunctionFromModule module_name = do
    funcs <- runDB $ selectList [FunctionModule ==. module_name] []
    if null funcs
        then Nothing
        else Just $ (functions !!) <$> liftIO (randomRIO (0, length functions - 1))
