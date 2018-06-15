{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Template.ReflexToken.Render (
    renderReflexTokens
  ) where

import Data.Foldable (traverse_)

import Reflex.Dom.Core

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Builder as LTB

import HTMLEntities.Decoder (htmlEncodedText)

import Reflex.Dom.Template.Rule
import Reflex.Dom.Template.Error
import Reflex.Dom.Template.ReflexToken

renderReflexTokens :: MonadWidget t m
                   => Rule m
                   -> [ReflexToken]
                   -> m ()
renderReflexTokens rule =
  traverse_ (renderReflexToken rule)

renderReflexToken :: MonadWidget t m
                  => Rule m
                  -> ReflexToken
                  -> m ()
renderReflexToken rule@(Rule fn) rt =
  case fn rt (renderReflexTokens rule) of
    Just w -> w
    Nothing -> case rt of
      RTElement l a xs ->
        elAttr l a $ renderReflexTokens rule xs
      RTText t ->
        text . LT.toStrict . LTB.toLazyText . htmlEncodedText $ t
