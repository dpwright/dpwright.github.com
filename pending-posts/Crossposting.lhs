---
date: 2014-07-28 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Crossposting
---

> {-# LANGUAGE OverloadedStrings #-}
>
> module Crossposting where

> import qualified Data.Map as M
> import           Hakyll

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
