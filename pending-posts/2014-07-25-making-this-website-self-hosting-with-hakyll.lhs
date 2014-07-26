---
date: 2014-07-25 08:38:23
tags: literate-programs, hakyll
title: Making this website self-hosting with Hakyll
---

Preliminaries
-------------

> {-# LANGUAGE OverloadedStrings #-}
>
> module Main where
>
> import           Data.Monoid         ((<>))
> import           Text.Pandoc.Options
> import           Hakyll
>
> import qualified Data.Map as M
> import qualified Data.Set as S

Compiler Settings
-----------------

> customCompiler :: Compiler (Item String)
> customCompiler = pandocCompilerWith readerOptions writerOptions
>   where readerOptions = def { readerSmart = True }
>         writerOptions = def
>                       { writerHTMLMathMethod = MathJax ""
>                       , writerHighlight      = True
>                       , writerExtensions     = writerExts
>                       }
>         writerExts    = writerExtensions def `S.union` S.fromList
>                       [ Ext_literate_haskell
>                       ]

Site builder
------------

> main :: IO ()
> main = hakyll $ do
>     templates
>     images
>     static
>     css
>     tags <- buildTags "posts/*" (fromCapture "tags/*.html")
>     posts    tags
>     archive  tags
>     tagIndex tags
>     index    tags

Some simple rules
-----------------

> templates, images, static, css :: Rules ()
>
> templates = match "templates/**" $ compile templateCompiler
>
> images = match "images/*" $ do
>     route   idRoute
>     compile copyFileCompiler
>
> static = match "static/**" $ do
>     route $ gsubRoute "static/" (const "")
>     compile copyFileCompiler
>
> css = match "css/*" $ do
>     route   idRoute
>     compile compressCssCompiler

Posts
-----

> posts, archive, tagIndex, index :: Tags -> Rules ()

> postCtx :: Tags -> Context String
> postCtx tags = modificationTimeField "mtime" "%U"
>             <> dateField "date" "%e %B, %Y"
>             <> tagsField "tags" tags
>             <> crosspostField "xp"
>             <> defaultContext
>
> postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
>          -> Compiler String
> postList tags pattern sortFilter = do
>     posts   <- sortFilter =<< loadAll pattern
>     itemTpl <- loadBody "templates/post-item.html"
>     applyTemplateList itemTpl (postCtx tags) posts
>
> posts tags = match "posts/*" $ do
>     route $ setExtension "html"
>     compile $ customCompiler
>         >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
>         >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
>         >>= relativizeUrls

Index pages
-----------

> archive tags = create ["archive.html"] $ do
>     route idRoute
>     compile $ do
>         list <- postList tags "posts/*" recentFirst
>         let archiveCtx = constField "posts" list
>                       <> constField "title" "Archives"
>                       <> defaultContext
>
>         makeItem ""
>             >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
>             >>= loadAndApplyTemplate "templates/default.html" archiveCtx
>             >>= relativizeUrls
>
> tagIndex tags = tagsRules tags $ \tag pattern -> do
>     let title = "Posts tagged " ++ tag
>     route idRoute
>     compile $ do
>         list <- postList tags pattern recentFirst
>         makeItem ""
>             >>= loadAndApplyTemplate "templates/archive.html"
>                 (constField "title" title <>
>                  constField "posts" list  <>
>                  defaultContext)
>             >>= loadAndApplyTemplate "templates/default.html" defaultContext
>             >>= relativizeUrls
>
> index tags = match "index.html" $ do
>     route idRoute
>     compile $ do
>         list <- postList tags "posts/*" recentFirst
>         let indexCtx = constField "posts" list
>                     <> field "tags" (\_ -> renderTagList tags)
>                     <> defaultContext
>
>         getResourceBody
>             >>= applyAsTemplate indexCtx
>             >>= loadAndApplyTemplate "templates/default.html" indexCtx
>             >>= relativizeUrls

Cross-posting
-------------

> getCrosspostHeader :: String -> Identifier -> Compiler String
> getCrosspostHeader key id' = getMetadata id' >>= toHeader . M.lookup key
>   where loadHeader = fmap itemBody . header
>         toHeader = maybe (return "") loadHeader
>         header name = makeItem "" >>= loadAndApplyTemplate (templatePath name) xpContext
>         templatePath name = fromFilePath $ "templates/xp/" ++ name ++ ".html"
>         xpContext = defaultContext
>
> crosspostField :: String -> Context a
> crosspostField key = field key $ getCrosspostHeader key . itemIdentifier
