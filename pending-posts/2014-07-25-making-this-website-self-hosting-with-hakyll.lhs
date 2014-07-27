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
>                                       Extension (..), HTMLMathMethod(..),
>                                       pandocExtensions, def)

> import qualified Data.Map as M
> import qualified Data.Set as S

> import           Hakyll

Site builder
------------

> main :: IO ()
> main = hakyll $ do
>   simpleRules
>   generateTags >>= taggedRules

> simpleRules :: Rules ()
> simpleRules = do
>   templates
>   images
>   static
>   css

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
>   route   idRoute
>   compile copyFileCompiler
>
> css = match "css/*" $ do
>   route   idRoute
>   compile compressCssCompiler
>
> static = match "static/**" $ do
>   route $ gsubRoute "static/" (const "")
>   compile copyFileCompiler

Compiler Settings
-----------------

> customCompiler :: Compiler (Item String)
> customCompiler = pandocCompilerWith readerOptions writerOptions
>   where readerOptions = def
>                       { readerSmart          = True
>                       , readerExtensions     = extensions
>                       }
>         writerOptions = def
>                       { writerHTMLMathMethod = MathJax ""
>                       , writerHighlight      = True
>                       , writerExtensions     = extensions
>                       }
>         extensions    = pandocExtensions `S.union` S.fromList
>                       [ Ext_literate_haskell
>                       ]

Posts
-----

> postCtx :: Tags -> Context String
> postCtx tags = modificationTimeField "mtime" "%U"
>             <> dateField "date" "%e %B, %Y"
>             <> tagsField "tags" tags
>             <> crosspostField "xp"
>             <> defaultContext
>

> postCompiler :: Tags -> Compiler (Item String)
> postCompiler tags = customCompiler
>                 >>= loadAndApplyTemplate "templates/post.html"    ctx
>                 >>= loadAndApplyTemplate "templates/default.html" ctx
>                 >>= relativizeUrls
>   where ctx = postCtx tags

> posts :: Tags -> Rules ()
> posts tags = match "posts/*" $ do
>   route $ setExtension "html"
>   compile $ postCompiler tags

Index pages
-----------

> indexCtx :: Tags -> String -> Context String
> indexCtx tags list = constField "posts" list
>                   <> defaultContext
>
> postList :: Tags
>          -> Pattern
>          -> ([Item String] -> Compiler [Item String])
>          -> Compiler String
> postList tags pattern sortFilter = do
>   posts   <- sortFilter =<< loadAll pattern
>   itemTpl <- loadBody "templates/post-item.html"
>   applyTemplateList itemTpl (postCtx tags) posts
>
> indexCompiler :: Tags -> Pattern -> Compiler (Item String)
> indexCompiler tags pattern = do
>   ctx <- indexCtx tags <$> postList tags pattern recentFirst
>   makeItem "" >>= loadAndApplyTemplate "templates/archive.html" ctx
>               >>= loadAndApplyTemplate "templates/default.html" ctx
>               >>= relativizeUrls
>
> tagIndex :: Tags -> Rules ()
> tagIndex tags = tagsRules tags $ \tag pattern -> do
>   route idRoute
>   compile $ indexCompiler tags pattern
>
> index :: Tags -> Rules ()
> index tags = create ["index.html"] . compile $ indexCompiler tags "posts/*"

Cross-posting
-------------

> getCrosspostHeader :: String -> Identifier -> Compiler String
> getCrosspostHeader key n = getMetadata n >>= toHeader . M.lookup key
>   where loadHeader        = fmap itemBody . header
>         toHeader          = maybe (return "") loadHeader
>         header name       = makeItem ""
>                         >>= loadAndApplyTemplate (templatePath name) xpContext
>         templatePath name = fromFilePath $ "templates/xp/" ++ name ++ ".html"
>         xpContext         = defaultContext
>
> crosspostField :: String -> Context a
> crosspostField key = field key $ getCrosspostHeader key . itemIdentifier
