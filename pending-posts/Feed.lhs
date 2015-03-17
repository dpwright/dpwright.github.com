---
date: 2015-03-16 08:29:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part 4: Atom Feed
---

<div class="sidenote">
This is part four of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</div>

By this point we have the basic features up and running, and everything we do
from here on are more or less "nice to have"s.  First of which is an Atom
feed, so people using feed readers can easily keep track of new posts.

Feeds seem to have become less popular recently, which I think is a shame
since I find them to be an incredibly easy way to keep on top of new content
across the various communities I'm interested in.  Perhaps I am part of the
problem, however, as I recently removed the Atom feed link from the header
during the redesign of this website over the new year holidays.  It may come
as a surprise, then, that I have a feed at all!  I do, and you can access it
at <http://dpwright.com/atom.xml>, which I had thought was a standard location
which browsers and feed readers would pick up automatically.  It turns out I
was wrong about that, so I guess I'll have to add the link back in.

Anyway, here's how I generate the above feed.  There's not a lot new in this
post that isn't already covered in the [official documentation on
feeds][hakyll-feeds], but here it is.

First, the usual pragmas and imports.  I was quite pleased with the results of
using `UnicodeSyntax` last post, so I'm going to do it again.

> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE OverloadedStrings #-}
> module Feed where
> import Hakyll

Let's import some useful operators...

> import Prelude.Unicode     	((⧺))
> import Data.Monoid.Unicode 	((⊕))
> import Control.Applicative 	((<$>))

Finally, we make use of `postCtx` from *`Posts`*.

> import Posts (postCtx)

Hakyll supports feeds natively and provides the *`FeedConfiguration`* type to
configure the feed's properties.  Here's mine.

> feedConfig :: FeedConfiguration
> feedConfig = FeedConfiguration
>   { feedTitle       	= 	"Wright Access"
>   , feedDescription 	= 	"dpwright's notes on code, Japan, " ⧺
>                     	 	"Japanese, and anything else"
>   , feedAuthorName  	= 	"Daniel P. Wright"
>   , feedAuthorEmail 	= 	"dani@dpwright.com"
>   , feedRoot        	= 	"http://dpwright.com"
>   }

Now we can generate the feed itself.  In this series so far, you've already
seen many times that to generate anything we need a *`Compiler`* and a
*`Context`*, which we can then refer to in a set of *`Rules`* which will
determine where and from what source files the content is made.  In this case,
all of these things are so simple we just define them inline.

> feed :: Tags → Rules ()
> feed tags = create ["atom.xml"] $ do
>   route idRoute
>   compile $ do
>     allContent 	← loadAllSnapshots "posts/*" "content"
>     ps         	← take 10 <$> recentFirst allContent
>     renderAtom feedConfig feedCtx ps
>   where feedCtx = postCtx tags ⊕ bodyField "description"

The feed context is just the post context augmented with the `description`
field.  You may remember when we were [generating the posts][generating-posts]
we used a function called `saveSnapshot` to save a copy of the content before
adding headers and other global design elements.  Here we load that content
and put it in the `description` field for the post, resulting in an Atom feed
which contains the entire post as its description, so that it can be read from
within the feed reader of your choice.

[generating-this-website]: http://www.dpwright.com/tags/generating%20this%20website
[hakyll-feeds]: http://jaspervdj.be/hakyll/tutorials/05-snapshots-feeds.html
[generating-posts]: /posts/2014/09/29/generating-this-website-part-2-posts/#generating-posts
