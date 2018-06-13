{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Template.Internal where

import Data.Maybe (mapMaybe, catMaybes)
import Data.Semigroup (Semigroup(..))

import Control.Lens

import Reflex.Dom.Core

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Lazy as LT

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Foldable (traverse_)
import Text.HTML.Parser
import Text.HTML.Tree
import Data.Tree

data TemplateError =
    XHRError XhrException
  | HTMLError
  deriving (Eq, Ord, Show)

data ReflexToken =
    RTText Text
  | RTElement Text (Map Text Text) [ReflexToken]

forestToReflex :: Forest Token -> [ReflexToken]
forestToReflex = mapMaybe treeToReflex

treeToReflex :: Tree Token -> Maybe (ReflexToken)
treeToReflex =
  let
    treeFold f (Node x xs) = f x (fmap (treeFold f) xs)
  in
    treeFold nodeToReflex

mkAttrs :: [Attr] -> Map Text Text
mkAttrs =
  foldMap mkAttr

mkAttr :: Attr -> Map Text Text
mkAttr (Attr k v) =
  Map.singleton k v

nodeToReflex :: Token -> [Maybe ReflexToken] -> Maybe ReflexToken
nodeToReflex (TagOpen l a) xs =
  Just $ RTElement l (mkAttrs a) (catMaybes xs)
nodeToReflex (TagSelfClose l a) xs =
  Just $ RTElement l (mkAttrs a) (catMaybes xs)
nodeToReflex (ContentText t) _ =
  Just $ RTText t
nodeToReflex (ContentChar c) _ =
  Just $ RTText $ Text.pack . pure $ c
nodeToReflex _ _ =
  Nothing

type Rule m = ReflexToken -> ([ReflexToken] -> m ()) -> Maybe (m ())

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
renderReflexToken rule rt =
  case rule rt (renderReflexTokens rule) of
    Just w -> w
    Nothing -> case rt of
      RTElement l a xs ->
        elAttr l a $ renderReflexTokens rule xs
      RTText t ->
        text t

htmlToWidget :: MonadWidget t m
             => Rule m
             -> Text
             -> Either TemplateError (m ())
htmlToWidget rule t =
  case tokensToForest . canonicalizeTokens . parseTokens $ t of
    Left _ -> Left HTMLError
    Right x -> Right . renderReflexTokens rule . forestToReflex $ x

fetch :: MonadWidget t m
      => Event t Text
      -> m (Event t TemplateError, Event t Text)
fetch ePath = do
  eRes <- performRequestAsyncWithError $ (\path -> XhrRequest "GET" path def) <$> ePath
  let
    (eFailure, eSuccess) =
      fanEither eRes
    mkSuccess =
      view xhrResponse_responseText
  pure (XHRError <$> eFailure, fmapMaybe mkSuccess eSuccess)

loadTemplate :: MonadWidget t m
             => Rule m
             -> Event t Text
             -> m (Event t TemplateError, Event t (m ()))
loadTemplate rule ePath = do
  (eError1, eText) <- fetch ePath
  let
    (eError2, eResult) = fanEither $ htmlToWidget rule <$> eText
    eError = leftmost [eError1, eError2]
  pure (eError, eResult)
