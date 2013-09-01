module System.Random.Extras
    ( randomModuleName
    ) where

import Control.Applicative ((<$>))
import Control.Monad.Random (getRandomRs)
import Data.Char (isAlpha)
import Data.Monoid ((<>))
import Data.Text (Text, pack)

randomModuleName :: Int -> IO Text
randomModuleName n
    | n <= 1 = fail "randomModuleName requires n > 1"
    | otherwise = do
        x  <- take 1     <$> randomCapitalLetters
        xs <- take (n-1) <$> randomLetters
        return . pack $ x <> xs

randomCapitalLetters :: IO String
randomCapitalLetters = getRandomRs ('A', 'Z')

randomLetters :: IO String
randomLetters = filter isAlpha <$> getRandomRs ('A', 'z')
