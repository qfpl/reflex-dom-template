{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Util.Files (
    fileDiv
  , fetch
  ) where

import Control.Monad (void)
import Data.Semigroup ((<>))

import Control.Monad.Trans (liftIO)

import System.FilePath ((</>))

import Data.Text (Text)
import qualified Data.Text as Text

import Control.Lens

import Reflex.Dom

import GHCJS.DOM.Element (setInnerHTML, getInnerHTML)
import qualified GHCJS.DOM.Types as DOM (Element)

contentsAdded :: forall t m. MonadWidget t m
              => DOM.Element
              -> String
              -> m (Event t ())
contentsAdded e s = do
  let
    check = do
      eTick <- tickLossyFromPostBuildTime 0.1
      eOut <- performEvent $ getInnerHTML e <$ eTick
      pure . void . ffilter (== s) $ eOut
  rec
    deDone <- widgetHold check $ pure never <$ eDone
    let eDone = switchDyn deDone
  pure eDone

stringDiv :: forall t m. MonadWidget t m
          => String
          -> m (Event t ())
stringDiv contents = do
  (e, _) <- el' "div" $ pure ()
  setInnerHTML (_element_raw e) contents

  if null contents
  then getPostBuild
  else contentsAdded (_element_raw e) contents

fetch :: MonadWidget t m
      => Text
      -> m (Event t Text, Event t Text)
fetch path = do
  ePostBuild <- getPostBuild
  eRes <- performRequestAsyncWithError $ XhrRequest "GET" path def <$ ePostBuild
  let
    (eFailure, eSuccess) =
      fanEither eRes
    mkSuccess =
      view xhrResponse_responseText
    mkFailure e =
      let
        failureText XhrException_Error = "Error"
        failureText XhrException_Aborted = "Aborted"
      in
        failureText e <> " while loading " <> path

  pure (mkFailure <$> eFailure, fmapMaybe mkSuccess eSuccess)

fileDiv :: forall t m. MonadWidget t m
        => Text
        -> m (Event t ())
fileDiv path = do
  ePostBuild <- getPostBuild
  eRes <- performRequestAsyncWithError $ XhrRequest "GET" path def <$ ePostBuild
  let
    (eFailure, eSuccess) = fanEither eRes

    mkSuccess =
      maybe (pure never) (stringDiv . Text.unpack) . view xhrResponse_responseText

    exceptionText XhrException_Error =
      "Error"
    exceptionText XhrException_Aborted =
      "Aborted"
    mkFailure e = do
      el "div" . text $ exceptionText e <> " while loading " <> path
      pure never

  fmap switchDyn . widgetHold (pure never) . leftmost $ [
      mkSuccess <$> eSuccess
    , mkFailure <$> eFailure
    ]
