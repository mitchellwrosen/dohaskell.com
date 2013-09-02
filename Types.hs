module Types where

import Data.Text (Text)

import Model

type ModuleName = Text
type FunctionName = Text

-- Type to unify LibFunction and UserFunction
data Function = LibF  LibFunction
              | UserF UserFunction
