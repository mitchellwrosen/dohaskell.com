{-# LANGUAGE OverloadedStrings #-}

import Test.HUnit

import Control.Monad (void)
import DohaskellFunc
import Model
import RunFunc

main :: IO ()
main = void $ runTestTT tests

tests :: Test
tests = TestList
            [ "testRunHaskell" ~: testRunHaskell ]

testRunHaskell = TestCase $ do
    let func = Function { functionRealName      = "(&&)"
        , functionUserName      = "my_and"
        , functionTypes         = ["Bool", "Bool", "Bool"]
        , functionDocumentation = ""
        , functionModule        = "Prelude"
        }
    let userDefinition = "my_and True True = True\nmy_and _ _ = False"

    result <- runDohaskellFunc $ runHaskell func userDefinition
    case result of
        Left err -> assertFailure err
        Right _  -> return ()
