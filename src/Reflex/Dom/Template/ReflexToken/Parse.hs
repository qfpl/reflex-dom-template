{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
module Reflex.Dom.Template.ReflexToken.Parse (
    parseReflexTokens
  ) where

import Data.Maybe (catMaybes, mapMaybe)

import Data.Text (Text)
import qualified Data.Text as Text

import Data.Map (Map)
import qualified Data.Map as Map

import Text.HTML.Parser
import Text.HTML.Tree
import Data.Tree

import Reflex.Dom.Template.ReflexToken
import Reflex.Dom.Template.Error

parseReflexTokens :: Text -> Either TemplateError [ReflexToken]
parseReflexTokens t =
  case tokensToForest . canonicalizeTokens . parseTokens $ t of
    Left _ -> Left HTMLError
    Right x -> Right . forestToReflex $ x

forestToReflex :: Forest Token -> [ReflexToken]
forestToReflex = mapMaybe treeToReflex

treeToReflex :: Tree Token -> Maybe ReflexToken
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
