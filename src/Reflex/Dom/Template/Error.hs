{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Template.Error (
    TemplateError(..)
  ) where

import Reflex.Dom.Core (XhrException)

data TemplateError =
    XHRError XhrException
  | TemplateNotFoundError
  | HTMLError
  deriving (Eq, Ord, Show, Read)
