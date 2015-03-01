---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Indexing
---

> {-# LANGUAGE OverloadedStrings #-}
> module Indexing where
> import Hakyll

> import Data.Monoid         	((<>))
> import Control.Applicative 	((<$>), empty)
> import Control.Monad

> import Data.Hashable
> import Data.Maybe
> import Data.Ord

> itemCtx :: Tags -> Context String
> itemCtx tags 	= tagsField "tags" tags
>              	<> dateField "date" "%e %B, %Y"
>              	<> urlField' "url"
>              	<> defaultContext

> postList 	:: 	Tags
>          	-> 	Pattern
>          	-> 	([Item String] -> Compiler [Item String])
>          	-> 	Compiler String
> postList tags pattern sortFilter = do
>   ps      	<- sortFilter =<< loadAll (pattern .&&. hasNoVersion)
>   itemTpl 	<- loadBody "templates/post-item.html"
>   applyTemplateList itemTpl (itemCtx tags) ps
>
> indexCtx :: Context String -> String -> Context String
> indexCtx base list = constField "posts" list <> base
>
> indexCompiler 	:: 	Tags
>               	-> 	Pattern
>               	-> 	Context String
>               	-> 	Compiler (Item String)
> indexCompiler tags pattern baseCtx = do
>   ctx <- indexCtx baseCtx <$> postList tags pattern recentFirst
>   makeItem "" 	>>= loadAndApplyTemplate "templates/archive.html" ctx
>               	>>= loadAndApplyTemplate "templates/default.html" ctx
>               	>>= relativizeUrls

> index :: Tags -> Rules ()
> index tags = create ["index.html"] $ do
>   route idRoute
>   compile $ indexCompiler tags "posts/*" indexContext

> indexContext :: Context String
> indexContext 	= bodyField     "body"
>              	<> metadataField
>              	<> urlField'     "url"
>              	<> pathField     "path"
>              	<> missingField

> urlField' :: String -> Context a
> urlField' key = field key $
>   fmap (maybe empty normaliseURL) . getRoute . itemIdentifier
>   where
>     normaliseURL url 	= toUrl . fromMaybe url
>                      	$ needlePrefix "index.html" url

Tag index
---------

> tagCloud :: Tags -> Rules ()
> tagCloud tags = create ["tags/index.html"] $ do
>   route idRoute
>   compile $ makeItem ""
>     >>= loadAndApplyTemplate "templates/tags.html"    tagsCtx
>     >>= loadAndApplyTemplate "templates/default.html" tagsCtx
>     >>= relativizeUrls
>   where 	tagsCtx 	= constField "title" "tags"
>         	        	<> tagCloudField "tagCloud" 100 500 (shuffle tags)
>         	        	<> defaultContext
>         	shuffle 	= sortTagsBy . comparing $ hash . fst
>
> tagIndex :: Tags -> Rules ()
> tagIndex tags = tagsRules tags $ \_ pattern -> do
>   route idRoute
>   compile $ indexCompiler tags pattern defaultContext
