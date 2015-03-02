---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Indexing
---

> {-# LANGUAGE OverloadedStrings #-}
> {-# LANGUAGE UnicodeSyntax #-}
> module Indexing where
> import Hakyll

> import Control.Applicative 	((<$>))

> import Data.Hashable
> import Data.Ord

> import Control.Monad.Unicode
> import Data.Monoid.Unicode
> import Prelude.Unicode

> import Posts

> itemCtx :: Tags → Context String
> itemCtx tags 	= tagsField "tags" tags
>              	⊕ dateField "date" "%e %B, %Y"
>              	⊕ urlField "url"
>              	⊕ defaultContext

> postList 	:: 	Tags
>          	→ 	Pattern
>          	→ 	([Item String] → Compiler [Item String])
>          	→ 	Compiler String
> postList tags pattern sortFilter = do
>   ps      	← sortFilter =≪ loadAll (pattern .&&. hasNoVersion)
>   itemTpl 	← loadBody "templates/post-item.html"
>   applyTemplateList itemTpl (itemCtx tags) ps ≫= removeIndexHtml
>
> indexCtx :: Context String → String → Context String
> indexCtx base list = constField "posts" list ⊕ base
>
> indexCompiler 	:: 	Tags
>               	→ 	Pattern
>               	→ 	Context String
>               	→ 	Compiler (Item String)
> indexCompiler tags pattern baseCtx = do
>   ctx ← indexCtx baseCtx <$> postList tags pattern recentFirst
>   makeItem "" 	≫= loadAndApplyTemplate "templates/archive.html" ctx
>               	≫= loadAndApplyTemplate "templates/default.html" ctx
>               	≫= relativizeUrls
>               	≫= withItemBody removeIndexHtml

> index :: Tags → Rules ()
> index tags = create ["index.html"] $ do
>   route idRoute
>   compile $ indexCompiler tags "posts/*" indexContext
>           ≫= withItemBody removeIndexHtml

> indexContext :: Context String
> indexContext 	= bodyField     	"body"
>              	⊕ metadataField 	
>              	⊕ urlField      	"url"
>              	⊕ pathField     	"path"
>              	⊕ missingField  	

Tag index
---------

> tagCloud :: Tags → Rules ()
> tagCloud tags = create ["tags/index.html"] $ do
>   route idRoute
>   compile $ makeItem ""
>     ≫= loadAndApplyTemplate "templates/tags.html"    tagsCtx
>     ≫= loadAndApplyTemplate "templates/default.html" tagsCtx
>     ≫= relativizeUrls
>     ≫= withItemBody removeIndexHtml
>   where
>     tagsCtx 	= tagCloudField 	"tagCloud" 100 500 (shuffle tags)
>             	⊕ bodyField     	"body"
>             	⊕ urlField      	"url"
>             	⊕ pathField     	"path"
>             	⊕ missingField  	
>     shuffle = sortTagsBy ∘ comparing $ hash ∘ fst
>
> tagIndex :: Tags → Rules ()
> tagIndex tags = tagsRules tags $ \_ pattern → do
>   route $ gsubRoute ".html" (const "/index.html")
>   compile $ indexCompiler tags pattern defaultContext
>           ≫= withItemBody removeIndexHtml
