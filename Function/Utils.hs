{-# LANGUAGE OverloadedStrings #-}

module Function.Utils
    ( argsStr
    , typeSignature
    , toUserName
    ) where

import Import

import Data.Map (Map, fromList, toList)
import Data.Map.Extras (lookupJust)
import Data.Text (head, init, intercalate, tail, unpack)
import Data.Tuple (swap)

import qualified Data.Text as T

argsStr :: Function -> String
argsStr (LibF  func) = argsStr' $ libFunctionTypes  func
argsStr (UserF func) = argsStr' $ userFunctionTypes func
  where
    argsStr' :: [Text] -> String
    argsStr' = numArgsToArgsStr . length . drop 1

-- 3 -> "arg1 arg2 arg3"
numArgsToArgsStr :: Int -> String
numArgsToArgsStr n = unwords $ map (\(arg,num) -> arg ++ show num) tups
  where
    tups :: [(String, Int)]
    tups = zip (repeat "arg") [1..n]

typeSignature :: Function -> String
typeSignature (LibF  func) = typeSignature' $ libFunctionTypes  func
typeSignature (UserF func) = typeSignature' $ userFunctionTypes func
  where
    typeSignature' :: [Text] -> String
    typeSignature' = unpack . intercalate " -> "

-- | Alias a function from its real name. Most functions can simply be prefaced with an arbitrary string, e.g. "my_".
-- Operators (functions that begin with '(' and consist of only symbols) must be transformed to a text-version, e.g.
-- "(&&)" -> "amp_amp".
toUserName :: FunctionName -> FunctionName
toUserName name
    | head name == '(' = (operatorToUserName . init . tail) name
    | otherwise = "my_" <> name

operatorToUserName :: FunctionName -> FunctionName
operatorToUserName = intercalate "_" . map encodeSymbol . unpack

encodeSymbol :: Char -> Text
encodeSymbol c = lookupJust c symbolToTextMap

decodeSymbol :: Text -> Char
decodeSymbol c = lookupJust c textToSymbolMap

-- | See http://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-180002.4 for information on legal
-- Haskell 2010 identifiers.
symbolToTextMap :: Map Char Text
symbolToTextMap = fromList
    [ ('!', "bang")
    , ('#', "hash")
    , ('$', "dollar")
    , ('%', "pct")
    , ('&', "amp")
    , ('*', "astk")
    , ('+', "plus")
    , ('.', "dot")
    , ('/', "fslash")
    , ('<', "lt")
    , ('=', "eq")
    , ('>', "gt")
    , ('?', "qmark")
    , ('@', "at")
    , ('\\', "bslash")
    , ('^', "caret")
    , ('|', "pipe")
    , ('-', "dash")
    , ('~', "tilde")
    , (':', "colon")
    ]

textToSymbolMap :: Map Text Char
textToSymbolMap = (fromList . map swap . toList) symbolToTextMap
