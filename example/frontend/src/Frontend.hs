{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}
module Frontend where

import qualified Data.Text as T
import Reflex.Dom.Core

import Common.Api
import Static

import Control.Monad (void)

import Data.Text (Text)

import Data.Map (Map)
import qualified Data.Map as Map

import Reflex.Dom.Template

frontend :: (StaticWidget x (), Widget x ())
frontend = (head', body)
  where
    head' = el "title" $ text "Obelisk Minimal Example"

body :: MonadWidget t m
     => m ()
body = mdo

  let
    loading = text "Loading"
    mkError e = text "Error"
    done = text "Done"

  eClick <- el "div" . fmap snd . runEventWriterT $ do
    (eError, eSuccess) <- loadTemplate (rule exs) $ static @ "test1.html"
    _ <- widgetHold loading . leftmost $ [
              mkError <$> eError
            , eSuccess
            , done <$ eClick
            ]
    pure ()
  pure ()

rule :: MonadWidget t m
     => Map Text (m ())
     -> ReflexToken
     -> ([ReflexToken] -> m ())
     -> Maybe (m ())
rule m (RTElement t a _) _ =
  if t == "exercise"
  then do
    i <- Map.lookup "id" a
    Map.lookup i m
  else Nothing
rule _ _ _ =
  Nothing

exs :: (MonadWidget t m, EventWriter t () m)
    => Map Text (m ())
exs = Map.fromList [("ex1", ex1), ("ex2", ex2)]

ex1 :: (MonadWidget t m, EventWriter t () m)
    => m ()
ex1 = do
  e <- button "Hi"
  tellEvent e

ex2 :: (MonadWidget t m, EventWriter t () m)
    => m ()
ex2 = do
  e <- button "Bye"
  tellEvent e

