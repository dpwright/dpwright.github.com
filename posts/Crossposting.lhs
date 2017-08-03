---
date: 2017-08-02 20:09:00
tags: hakyll, literate-programs, generating this website
title: "Generating this website part 8: Crossposting"
---

<div class="sidenote">
This is part eight of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</div>

Occasionally (very occasionally in fact -- this has only happened once!) I
write something which is intended to be cross-posted to another site as well as
being hosted on my own.  I did this with my post on [proxies and
pipes][proxies-pipes], which was also pretty much the first bit of Haskell code
I put out, and is probably hopelessly outdated by this point, but there it is.

The idea here is that I want to be able to write a post on my site as usual,
but also have it uploaded to the site that I'm contributing to.  I have another
script which deals with uploading the post to WordPress, so all this part of
the generator has to do is add a header to the top of the post linking to the
other version of the site.

Let's begin with the standard fluff...

> {-# LANGUAGE OverloadedStrings #-}
> module Crossposting where
> import Hakyll

All this module really does is expose a function, `crosspostField`, which
returns the content of the header we want to insert at the top of the page if
it's a crosspost.  This is fed into the template for [posts][posts].

> crosspostField :: String -> Context a
> crosspostField key = field key $ getCrosspostHeader key . itemIdentifier

The idea behind this is to extract the field identified by `key` from the
metadata at the top of the post file, and pass it off to `getCrosspostHeader`
to turn into a header body.  The job of this function, then, is to look in the
`templates/xp/` folder for a template file whose name matches the value of the
field, render that template if found, and return it -- otherwise we return an
empty string.  This is given below.

> getCrosspostHeader :: String -> Identifier -> Compiler String
> getCrosspostHeader key n = getMetadata n >>= toHeader . lookupString key
>   where loadHeader        = fmap itemBody . header
>         toHeader          = maybe (return "") loadHeader
>         header name       = makeItem ""
>                         >>= loadAndApplyTemplate (templatePath name) xpContext
>         templatePath name = fromFilePath $ "templates/xp/" ++ name ++ ".html"
>         xpContext         = defaultContext

Note that the header itself is a template, rendered using the `defaultContext`
context.  This means it has access to other fields in the post's metadata --
useful if you want to link to the other version of the site, as you can put its
URL or ID into the metadata.

[generating-this-website]: /tags/generating%20this%20website
[proxies-pipes]: /posts/2013/08/21/writing-a-tcp-server-in-haskell-using-proxies-and-pipes
[posts]:         /posts/2014/09/29/generating-this-website-part-2-posts
