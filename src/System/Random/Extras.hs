module System.Random.Extras
    ( randomElem
    , randomModuleName
    ) where

import Prelude

import Control.Applicative ((<$>))
import Control.Monad.Random (getRandomRs, randomRIO)
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

randomElem :: [a] -> IO (Maybe a)
randomElem [] = return Nothing
randomElem xs = randomRIO (0, length xs - 1) >>= return . Just . (xs !!)
