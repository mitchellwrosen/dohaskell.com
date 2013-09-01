{-# LANGUAGE OverloadedStrings #-}

module HaskFunction.Utils
    ( argsStr
    , typeSignature
    ) where

import Prelude
import Data.Text (intercalate, unpack)

import Model (HaskFunction(..))

argsStr :: HaskFunction -> String
argsStr = numArgsToArgsStr . length . drop 1 . haskFunctionTypes

-- 3 -> "arg1 arg2 arg3"
numArgsToArgsStr :: Int -> String
numArgsToArgsStr n = unwords $ map (\(arg,num) -> arg ++ show num) tups
  where
    tups :: [(String, Int)]
    tups = zip (repeat "arg") [1..n]

typeSignature :: HaskFunction -> String
typeSignature = unpack . intercalate " -> " . haskFunctionTypes
