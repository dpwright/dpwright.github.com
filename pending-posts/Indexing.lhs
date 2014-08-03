---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Indexing
---

> {-# LANGUAGE OverloadedStrings #-}
>
> module Indexing where

> import           Data.Monoid         ((<>))
> import           Control.Applicative ((<$>))
> import           Hakyll

> itemCtx :: Context String
> itemCtx = dateField "date" "%e %B, %Y"
>        <> defaultContext

> postList :: Pattern
>          -> ([Item String] -> Compiler [Item String])
>          -> Compiler String
> postList pattern sortFilter = do
>   ps      <- sortFilter =<< loadAll pattern
>   itemTpl <- loadBody "templates/post-item.html"
>   applyTemplateList itemTpl itemCtx ps
>
> indexCtx :: Context String -> String -> Context String
> indexCtx base list = constField "posts" list <> base
>
> indexCompiler :: Pattern -> Context String -> Compiler (Item String)
> indexCompiler pattern baseCtx = do
>   ctx <- indexCtx baseCtx <$> postList pattern recentFirst
>   makeItem "" >>= loadAndApplyTemplate "templates/archive.html" ctx
>               >>= loadAndApplyTemplate "templates/default.html" ctx
>               >>= relativizeUrls
>
> tagIndex :: Tags -> Rules ()
> tagIndex tags = tagsRules tags $ \_ pattern -> do
>   route idRoute
>   compile $ indexCompiler pattern defaultContext
>
> index :: Rules ()
> index = create ["index.html"] $ do
>   route idRoute
>   compile $ indexCompiler "posts/*" baseContext
>   where baseContext = constField "title" "" <> defaultContext
