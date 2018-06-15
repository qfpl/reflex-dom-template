{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module Reflex.Dom.Template.Rule (
    Rule(..)
  , elRule
  , elIdRule
  , elAttrsRule
  ) where

import Control.Applicative ((<|>))
import Data.Semigroup (Semigroup(..))

import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Template.ReflexToken

newtype Rule m = Rule { unRule :: ReflexToken -> ([ReflexToken] -> m ()) -> Maybe (m ())}

instance Semigroup (Rule m) where
  Rule r1 <> Rule r2 = Rule $ \rt rts -> r1 rt rts <|> r2 rt rts

instance Monoid (Rule m) where
  mempty = Rule $ \_ _ -> Nothing
  mappend = (<>)

elRule :: Text -> m () -> Rule m
elRule t w =
  elAttrsRule t $ const (Just w)

elIdRule :: Text -> (Text -> Maybe (m ())) -> Rule m
elIdRule t fn =
  elAttrsRule t $ \a -> Map.lookup "id" a >>= fn

elAttrsRule :: Text -> (Map Text Text -> Maybe (m ())) -> Rule m
elAttrsRule t fn = Rule $ \rt _ ->
  case rt of
    RTElement l a _ -> 
      if l == t then fn a else Nothing
    _ -> Nothing
