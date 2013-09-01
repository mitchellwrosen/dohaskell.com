{-# LANGUAGE OverloadedStrings #-}

import System.Random.Extras

import Test.QuickCheck
import Test.QuickCheck.Monadic (assert, monadicIO, run)

import Prelude hiding (head)
import Control.Applicative ((<$>))
import Data.Text (head)

prop_startsWithCapital :: Int -> Property
prop_startsWithCapital n = n `elem` [2..50] ==> monadicIO test
  where
    test = do
        first_letter <- run $ head <$> randomModuleName n
        assert $ first_letter `elem` ['A'..'Z']

main :: IO ()
main = quickCheck prop_startsWithCapital
