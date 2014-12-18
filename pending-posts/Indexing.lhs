---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Indexing
---

> {-# LANGUAGE OverloadedStrings #-}
> module Indexing where
> import           Hakyll

> import           Data.Monoid         ((<>))
> import           Control.Applicative ((<$>))

> import Data.Hashable
> import Data.Ord

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

> index :: Rules ()
> index = create ["index.html"] $ do
>   route idRoute
>   compile $ indexCompiler "posts/*" baseContext
>   where baseContext = constField "title" "" <> defaultContext

Tag index
---------

> tagCloud :: Tags -> Rules ()
> tagCloud tags = create ["tags/index.html"] $ do
>   route idRoute
>   compile $ makeItem ""
>         >>= loadAndApplyTemplate "templates/tags.html"    tagsCtx
>         >>= loadAndApplyTemplate "templates/default.html" tagsCtx
>         >>= relativizeUrls
>   where tagsCtx = constField "title" "tags"
>                <> tagCloudField "tagCloud" 100 500 (shuffle tags)
>                <> defaultContext
>         shuffle = sortTagsBy . comparing $ hash . fst
>
> tagIndex :: Tags -> Rules ()
> tagIndex tags = tagsRules tags $ \_ pattern -> do
>   route idRoute
>   compile $ indexCompiler pattern defaultContext
