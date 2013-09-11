{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}

module Function.Utils
    ( libFunctionToText
    ) where

import Import

import Data.ByteString.Lazy (toChunks)
import Data.List (intersperse)
import Data.Map (Map, fromList, toList)
import Data.Map.Extras (lookupJust)
import Data.Text (head, init, intercalate, pack, singleton, tail, unpack)
import Data.Text.Encoding (decodeUtf8)
import Data.Tuple (swap)
import Text.Hastache
import Text.Hastache.Context (mkStrContext)
import Text.Regex (Regex, mkRegex, subRegex)

import qualified Data.Text as T

-- | Given a library function @func@ and a module @random_module_name@ containing user code, fill out the template in
-- mustache-templates/libFunc.mustache, which contains QuickCheck code.
-- TODO: This function and the mustache template are badly named. What if we want to do something else besides
-- QuickCheck?
libFunctionToText :: (Functor m, MonadIO m) => LibFunction -> ModuleName -> m Text
libFunctionToText func random_module_name = (decodeUtf8 . mconcat . toChunks) <$>
    hastacheFile config "mustache-templates/libFunc.mustache" (mkStrContext context)
  where
    config :: MonadIO m => MuConfig m
    config = defaultConfig { muEscapeFunc = emptyEscape }

    context :: Monad m => String -> MuType m
    context "module_name"        = MuVariable $ libFunctionModule func
    context "random_module_name" = MuVariable   random_module_name
    context "args"               = MuVariable $ argsStr (libFunctionNumArgs func)
    context "name"               = MuVariable $ libFunctionName func
    context "stripped_name"      = MuVariable $ stripParens (libFunctionName func)

-- | Creates an argument string suitable for inserting into a template file from an int representing number of 
-- parameters, e.g. 3 -> "a b c".
argsStr :: Int -> String
argsStr n = intersperse ' ' $ take n ['a'..]

-- | Strip the parens around an operator (if it's an operator).
-- TODO: Avoid partial functions? Kind of a pain with Text.
stripParens :: FunctionName -> FunctionName
stripParens name
    | head name == '(' = (init . tail) name
    | otherwise = name
