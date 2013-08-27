module System.Random.Extras
    ( randomModuleName
    ) where

import Prelude

import Control.Applicative ((<$>))
import Control.Monad.Random (getRandomRs)
import Data.Char (isAlpha)

randomModuleName :: Int -> IO [Char]
randomModuleName n = do
    x  <- take 1     <$> randomCapitalLetters
    xs <- take (n-1) <$> randomLetters
    return (x ++ xs)

randomCapitalLetters :: IO [Char]
randomCapitalLetters = getRandomRs ('A', 'Z')

randomLetters :: IO [Char]
randomLetters = filter isAlpha <$> getRandomRs ('A', 'z')
