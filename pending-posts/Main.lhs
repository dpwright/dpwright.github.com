---
date: 2014-07-25 08:38:23
tags: literate-programs, hakyll, generating this website
title: Generating this website part 1: Introduction
---

Preliminaries
-------------

> {-# LANGUAGE OverloadedStrings #-}

> import           Control.Applicative (liftA2)
> import           Hakyll

> import           Posts
> import           Indexing
> import           Feed

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
>   index

> generateTags :: Rules Tags
> generateTags = buildTags "posts/*" $ fromCapture "tags/*.html"

> taggedRules :: Tags -> Rules ()
> taggedRules = posts & tagIndex & feed
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
