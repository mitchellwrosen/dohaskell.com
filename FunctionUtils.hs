{-# LANGUAGE OverloadedStrings #-}

module FunctionUtils (argsStr, typeSignature) where

import Prelude
import Data.Text (intercalate, unpack)

import Model (Function(..))

argsStr :: Function -> String
argsStr = numArgsToArgsStr . length . drop 1 . functionTypes

-- 3 -> "arg1 arg2 arg3"
numArgsToArgsStr :: Int -> String
numArgsToArgsStr n = unwords $ map (\(arg,num) -> arg ++ show num) tups
  where
    tups :: [(String, Int)]
    tups = zip (repeat "arg") [1..n]

typeSignature :: Function -> String
typeSignature = unpack . intercalate " -> " . functionTypes
