{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Template.ReflexToken (
    ReflexToken(..)
  ) where

import Data.Text (Text)
import Data.Map (Map)

data ReflexToken =
    RTText Text
  | RTElement Text (Map Text Text) [ReflexToken]
  deriving (Eq, Ord, Show, Read)
