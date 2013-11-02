module Data.Map.Extras where

import Prelude
import Data.Maybe (fromJust)

import qualified Data.Map as M

lookupJust :: Ord k => k -> M.Map k a -> a
lookupJust k = fromJust . M.lookup k
