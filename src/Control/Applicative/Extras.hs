module Control.Applicative.Extras ((<$$>)) where

import Prelude

(<$$>) :: (Functor f, Functor g) => (a -> b) -> f (g a) -> f (g b)
(<$$>) = fmap . fmap
