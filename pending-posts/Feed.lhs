---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Atom Feed
---

> {-# LANGUAGE OverloadedStrings #-}
>
> module Feed where

> import           Data.Monoid         ((<>))
> import           Hakyll
> import           Posts

> feedConfig :: FeedConfiguration
> feedConfig = FeedConfiguration
>            { feedTitle       = "Wright Access"
>            , feedDescription = "dpwright's notes on code, Japan, Japanese, and anything else"
>            , feedAuthorName  = "Daniel P. Wright"
>            , feedAuthorEmail = "dani@dpwright.com"
>            , feedRoot        = "http://dpwright.com"
>            }

> feed :: Tags -> Rules ()
> feed tags = create ["atom.xml"] $ do
>   route idRoute
>   compile $ do
>     ps <- fmap (take 10) . recentFirst =<< loadAllSnapshots "posts/*" "content"
>     renderAtom feedConfig feedCtx ps
>   where feedCtx = postCtx tags <> bodyField "description"
