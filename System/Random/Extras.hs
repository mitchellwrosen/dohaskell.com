module System.Random.Extras
    ( randomModuleName
    ) where

import Prelude hiding (take)

import Control.Applicative ((<$>))
import Control.Monad.Random (getRandomRs)
import Data.Char (isAlpha)
import Data.Monoid ((<>))
import Data.Text (Text, pack, take)

randomModuleName :: Int -> IO Text
randomModuleName n = do
    x  <- take 1     <$> randomCapitalLetters
    xs <- take (n-1) <$> randomLetters
    return $ x <> xs

randomCapitalLetters :: IO Text
randomCapitalLetters = pack <$> getRandomRs ('A', 'Z')

randomLetters :: IO Text
randomLetters = pack . filter isAlpha <$> getRandomRs ('A', 'z')
