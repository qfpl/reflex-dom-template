{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module Reflex.Dom.Template.Xhr (
    fetch
  ) where

import Control.Lens (view)

import Data.Text (Text)

import Reflex.Dom.Core

import Reflex.Dom.Template.Error

fetch :: MonadWidget t m
      => Event t Text
      -> m (Event t (Either TemplateError Text))
fetch ePath = do
  eRes <- performRequestAsyncWithError $ (\path -> XhrRequest "GET" path def) <$> ePath
  let
    (eFailure, eSuccess) =
      fanEither eRes
    mkSuccess =
      view xhrResponse_responseText
  pure . leftmost $ [Left . XHRError <$> eFailure, fmap Right . fmapMaybe mkSuccess $ eSuccess]
