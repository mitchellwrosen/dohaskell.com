{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Settings.StaticFiles

data DoFunction = DoFunction String
data Module = Module String [DoFunction]

-- This is a handler function for the GET request method on the HomeR
-- resource pattern. All of your resource patterns are defined in
-- config/routes
--
-- The majority of the code you will write in Yesod lives in these handler
-- functions. You can spread them across multiple files if you are so
-- inclined, or create a single monolithic file.
getHomeR :: Handler Html
getHomeR = do
    defaultLayout $ do
        let modules = [ Module "Prelude" [DoFunction "(&&)"]
                      , Module "Data.List" [DoFunction "(||)"]
                      , Module "Data.Maybe" [DoFunction "(==)"]
                      ]
        -- TODO(chebert): where to load, where to load?
        addScriptRemote "http://ajax.googleapis.com/ajax/libs/jquery/1.9.0/jquery.min.js"
        setTitle "Welcome To DoHaskell!"
        $(widgetFile "homepage")

postHomeR :: Handler Html
postHomeR = do
    ((result, formWidget), formEnctype) <- runFormPost sampleForm
    let handlerName = "postHomeR" :: Text
        submission = case result of
            FormSuccess res -> Just res
            _ -> Nothing

    defaultLayout $ do
        aDomId <- newIdent
        setTitle "Welcome To Yesod!"
        [whamlet|
        |]

sampleForm :: Form (FileInfo, Text)
sampleForm = renderDivs $ (,)
    <$> fileAFormReq "Choose a file"
    <*> areq textField "What's on the file?" Nothing
