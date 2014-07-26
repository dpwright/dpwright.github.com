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
> import           Control.Applicative (liftA2, (<$>))
> import           Text.Pandoc.Options (ReaderOptions(..), WriterOptions (..),
>                                       Extension (..), HTMLMathMethod(..), def)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import           Hakyll

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
>     simpleRules
>     generateTags >>= taggedRules

> simpleRules :: Rules ()
> simpleRules = do
>     templates
>     images
>     static
>     css

> generateTags :: Rules Tags
> generateTags = buildTags "posts/*" $ fromCapture "tags/*.html"

> taggedRules :: Tags -> Rules ()
> taggedRules = posts & tagIndex & index
>   where (&) = liftA2 (>>)

Some simple rules
-----------------

> templates, images, css, static :: Rules ()
>
> templates = match "templates/**" $ compile templateCompiler
>
> images = match "images/*" $ do
>     route   idRoute
>     compile copyFileCompiler
>
> css = match "css/*" $ do
>     route   idRoute
>     compile compressCssCompiler
>
> static = match "static/**" $ do
>     route $ gsubRoute "static/" (const "")
>     compile copyFileCompiler

Posts
-----

> postCtx :: Tags -> Context String
> postCtx tags = modificationTimeField "mtime" "%U"
>             <> dateField "date" "%e %B, %Y"
>             <> tagsField "tags" tags
>             <> crosspostField "xp"
>             <> defaultContext
>

> posts :: Tags -> Rules ()
> posts tags = match "posts/*" $ do
>     route $ setExtension "html"
>     compile $ customCompiler
>         >>= loadAndApplyTemplate "templates/post.html"    (postCtx tags)
>         >>= loadAndApplyTemplate "templates/default.html" (postCtx tags)
>         >>= relativizeUrls

Index pages
-----------

> indexCtx :: Tags -> String -> Context String
> indexCtx tags list = constField "posts" list
>                   <> field "tags" (\_ -> renderTagList tags)
>                   <> defaultContext
>
> postList :: Tags -> Pattern -> ([Item String] -> Compiler [Item String])
>          -> Compiler String
> postList tags pattern sortFilter = do
>     posts   <- sortFilter =<< loadAll pattern
>     itemTpl <- loadBody "templates/post-item.html"
>     applyTemplateList itemTpl (postCtx tags) posts
>
> tagIndex :: Tags -> Rules ()
> tagIndex tags = tagsRules tags $ \tag pattern -> do
>     route idRoute
>     compile $ do
>         ctx <- indexCtx tags <$> postList tags pattern recentFirst
>         makeItem ""
>             >>= loadAndApplyTemplate "templates/archive.html" ctx
>             >>= loadAndApplyTemplate "templates/default.html" ctx
>             >>= relativizeUrls
>
> index :: Tags -> Rules ()
> index tags = match "index.html" $ do
>     route idRoute
>     compile $ do
>         ctx <- indexCtx tags <$> postList tags "posts/*" recentFirst
>         getResourceBody
>             >>= applyAsTemplate ctx
>             >>= loadAndApplyTemplate "templates/default.html" ctx
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
