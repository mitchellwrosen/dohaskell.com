{-# LANGUAGE OverloadedStrings #-}

import Test.QuickCheck
import Test.QuickCheck.Monadic

import Prelude hiding (take)
import Control.Applicative ((<$>))
import Data.Text (take)
import System.Random.Extras

prop_startsWithCapital :: Int -> Property
prop_startsWithCapital n = n > 0 ==> monadicIO test
  where
    test = do 
        first_letter <- take 1 <$> randomModuleName n
        assert $ first_letter `elem` ['A'..'Z']

main :: IO ()
main = quickCheck prop_startsWithCapital
