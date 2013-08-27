{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module DohaskellFunc
    ( DohaskellFunc
    , runDohaskellFunc
    ) where

import Prelude

import Control.Monad.Trans (MonadIO)
import Control.Monad.Trans.Either (EitherT, left, right, runEitherT)

-- Monad representing either a failed action, or the result of a successful action.
newtype DohaskellFunc a = DohaskellFunc {
                              runDohaskellF :: EitherT String IO a
                          } deriving (MonadIO)

instance Monad DohaskellFunc where
    return = DohaskellFunc . right
    m >>= k = DohaskellFunc $ runDohaskellF m >>= runDohaskellF . k
    fail = DohaskellFunc . left

runDohaskellFunc :: DohaskellFunc a -> IO (Either String a)
runDohaskellFunc = runEitherT . runDohaskellF
