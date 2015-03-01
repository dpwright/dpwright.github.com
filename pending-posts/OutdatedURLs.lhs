---
date: 2015-03-01 20:28:17
tags: hakyll, literate-programs, generating this website
title: Generating this website part x: Outdated URLs
---

On February 1st, 2015, I decided to change the URL scheme for this site, such
that instead of outputting pages to `post-name.html` it would output to
`post-name/index.html`.  Unfortunately, this breaks any existing links to the
original pages that might be out there.  This module keeps those links active
by generating any posts prior to that date at the original URL.

Imports
-------

Let's get the obvious out the way...

> {-# LANGUAGE OverloadedStrings #-}
> module OutdatedURLs where
> import Hakyll

We'll also import *`Posts`*, since for the most part we want to generate these
posts exactly like their correctly-addressed counterparts. 

> import Posts

Finally, we import `Map` to look up metadata, and `Data.Time` so that we can
check the dates.  Note we hide `readTime` because we're going to use the
version from the `Posts` module.

> import qualified Data.Map as M
> import Data.Time hiding (readTime)

Setting up the rules
--------------------

First we define the cut-off date, before which an old-style `.html` page will
be created.  This applies to any posts made prior to midnight, February 1st,
2015.

> cutoffDate :: UTCTime
> cutoffDate = UTCTime 	(fromGregorian 2015 02 01)
>                      	(secondsToDiffTime 0)

The `Rules` managing these posts are similar to those for [standard
`Posts`][posts].  We make use of the recently-added `matchMetadata` to compare
the `date` field of the posts with our `cutoffDate`, and we tag the rule with
the version "outdated" to avoid them showing up in the index.  And of course
we use `setExtension` rather than `simplifyURL`, since that's the whole point
of the exercise!

> outdatedURLs :: Tags -> Rules ()
> outdatedURLs tags =
>   matchMetadata "posts/*" isOutdated . version "outdated" $
>   do 	route   	$ 	metadataRoute dateAndTitle `composeRoutes`
>      	        	  	setExtension ".html"
>      	compile 	$ 	postCompiler tags
>   where
>     isOutdated 	= maybe False checkDate . M.lookup "date"
>     checkDate  	= (> 0) . diffUTCTime cutoffDate . readTime

That's it!
----------

With just a small amount of effort, we've managed to completely change our URL
scheme without breaking any old links.  I wouldn't want to make a habit of
this sort of thing, but it's good to know it can be resolved fairly easily
should the need arise!

[posts]: /posts/2014-09-29-generating-this-website-part-2-posts
