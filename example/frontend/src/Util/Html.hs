{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Util.Html (
    ReflexToken(..)
  , Rule
  , htmlWidget
  ) where

import Data.Maybe (mapMaybe, catMaybes)
import Data.Semigroup (Semigroup)

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

htmlToWidget :: (MonadWidget t m)
             => Text
             -> Rule m
             -> Either Text (m ())
htmlToWidget t rule =
  case tokensToForest . canonicalizeTokens . parseTokens $ t of
    Left e -> Left . Text.pack . show $ e
    Right x -> Right . renderReflexTokens rule . forestToReflex $ x

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
