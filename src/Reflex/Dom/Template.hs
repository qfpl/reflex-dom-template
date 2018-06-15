{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Template (
    loadTemplate
  , module X
  ) where

import Data.Text (Text)

import Reflex.Dom.Core

import Reflex.Dom.Template.Rule as X
import Reflex.Dom.Template.Error as X
import Reflex.Dom.Template.ReflexToken as X
import Reflex.Dom.Template.ReflexToken.Parse
import Reflex.Dom.Template.ReflexToken.Render
import Reflex.Dom.Template.Xhr

template :: MonadWidget t m
         => Rule m
         -> Text
         -> Either TemplateError (m ())
template rule =
  fmap (renderReflexTokens rule) . parseReflexTokens

loadTemplate :: MonadWidget t m
             => Rule m
             -> Event t Text
             -> m (Event t (Either TemplateError (m ())))
loadTemplate rule ePath = do
  eEither <- fetch ePath
  let
    (eError1, eText) = fanEither eEither
    (eError2, eResult) = fanEither $ template rule <$> eText
    eError = leftmost [eError1, eError2]
  pure . leftmost $ [Left <$> eError, Right <$> eResult]
