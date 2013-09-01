{-# LANGUAGE TupleSections, OverloadedStrings #-}
module Handler.Home where

import Import
import Settings.StaticFiles

data DoFunction = DoFunction String
data Module = Module String [DoFunction]

import Data.Text (toUpper)

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
postHomeR = getHomeR

handleSubmission :: Text -> IO (Maybe Textarea)
handleSubmission = return . Just . Textarea . toUpper
