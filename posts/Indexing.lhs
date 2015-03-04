---
date: 2015-03-04 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part 3: Indexing
---

<div class="sidenote">
This is part two of the "generating this website" series.  To read the rest of
the series, go to the series index [here][generating-this-website]
</div>

After something of a hiatus, I'm back with the next in the "generating this
website" series, which describes the actual code used to create the site
you're reading.  This time I'm going to cover indexing, which in combination
with the [introduction] and the previous post on [generating posts
themselves][posts] is enough to get a basic Hakyll site up and running.

What do I mean by "indexing"?  Well, basically just creating the list of blog
posts on the site's [main page], or the indices by [tags].  This is the main
mechanism by which visitors will navigate the site and be introduced to new
posts.

It's possible to go quite deep on this, breaking up indices into pages, having
lists by year or by month, and so forth.  As I don't have too many posts, I'm
keeping it simple for now, with a single-page index of all posts, and a
similar single-page index per tag.  I may revisit this as the number of posts
increases to split the index up over multiple pages, or have some sort of
infinite scrolling thing like twitter.  Should that happen I'll update this
post -- as I'll have to of course, because this post is a literate program,
and the code you're about to see is what I actually run to generate the index
pages for this site.

Let's get cracking.

Preliminaries
-------------

For this post, I'm going to try out something I haven't tried before, and
that's the `UnicodeSyntax` extension for GHC and the [base-unicode-symbols]
package which gives Unicode equivalents to commonly used Haskell operators.
Why?  Well, I've recently switched to using the [Computer Modern][cm-unicode]
series of fonts on the site; specifically I use `Computer Modern Serif` for
the prose and `Computer Modern Concrete` for code.  I think it looks really
nice (well, unless you're on Windows anyway), but some of Haskell's operators
are a little ungainly -- for example the ubiquitous monadic bind operator
comes out as `>>=`, and monoidal concatenation isn't much better: `<>`.
With unicode operators I can use `≫=` and `⊕` respectively, which look nicer
at the possible expense of being slightly more awkward to type.

Most of the operators are similar enough to their ASCII originals that I hope
readers will still be able to follow the code.

> {-# LANGUAGE UnicodeSyntax #-}

Having enabled Unicode syntax, I'll start with the usual incantation:

> {-# LANGUAGE OverloadedStrings #-}
> module Indexing where
> import Hakyll

Here come the Unicode modules, which give me the operators from
`Control.Monad`, `Data.Monoid`, and the Prelude, in Unicode.

> import Control.Monad.Unicode
> import Data.Monoid.Unicode
> import Prelude.Unicode

`base-unicode-symbols` does export a module for `Control.Applicative`, but
there is no Unicode symbol representing `<$>`, so I import that here.

> import Control.Applicative ((<$>))

The following two modules come in handy for imposing a pseudo-random order on
my tags (for the tag cloud).  In fact there's nothing random about it -- I
just order the tags based on the hash of their name, but the result looks a
lot less tacky than an alphabetically ordered list.

> import Data.Hashable
> import Data.Ord

Finally I import the previous blog on [posts], to make use of the
`removeIndexHtml` function.

> import Posts (removeIndexHtml)

The main index
--------------

This is the main index which you see on the [main page].  Even though it is
not an index by tag, I need to make use of the tags in order to list under
each post title what tags apply to that post.

> index :: Tags → Rules ()
> index tags = create ["index.html"] $ do
>   route idRoute
>   compile $ indexCompiler tags "posts/*" indexContext

The rule is pretty simple -- I create a page, `index.html`, which runs the
`indexCompiler` over all files in the `posts` directory.  Since I'm creating
the file from scratch, rather than converting from some source format, I don't
need to perform any transformation on the filename so I can just use
`idRoute`.

As always with Hakyll, we make use of a `Compiler` to give the
conversion/generation steps for a resource and a `Context`to supply the
fields.  The `Context` in this case is as follows:

> indexContext :: Context String
> indexContext 	= bodyField     	"body"
>              	⊕ metadataField 	
>              	⊕ urlField      	"url"
>              	⊕ pathField     	"path"
>              	⊕ missingField  	

These fields can then be used in the index template.

What about the `Compiler`?  Well, let's start by looking at its type signature.

> indexCompiler 	:: 	Tags
>               	→ 	Pattern
>               	→ 	Context String
>               	→ 	Compiler (Item String)

There are quite a few parameters here.  As mentioned, we need the `Tags`, and
the `Pattern` allows us to specify how to find the posts.  We could have just
used `indexContext` directly here rather than pass it in, but as we'll see if
we pass it in we can re-use this `Compiler` when we generate our tag-specific
indices by passing in a different `Context`.  Finally, the function returns
our `Compiler`.

> indexCompiler tags pattern baseCtx = do
>   ctx ← mkCtx baseCtx <$> postList tags pattern recentFirst
>   makeItem ""
>     ≫= loadAndApplyTemplate "templates/archive.html" ctx
>     ≫= loadAndApplyTemplate "templates/default.html" ctx
>     ≫= relativizeUrls
>     ≫= withItemBody removeIndexHtml
>   where mkCtx base list = constField "posts" list ⊕ base

The compiler is fairly standard, except perhaps for that first line -- what's
going on there?  Well, in order to generate our index, we first need to
generate our list of posts, and that's the job of `postList`.  We then add
that to our `Context`, and use this augmented `Context` to generate the index
page as a whole.

That is actually where most of the work happens -- if you think about it, the
`index` page itself is just a skeleton around the main thing we're trying to
display -- the list of posts.  That being the case, let's take a look at how
that's generated.

The list of posts
-----------------

OK, so we want a thing, it's Hakyll, what do we need?  A `Context` and a
`Compiler`, of course!  We define the `Context` required to generate a single
item in the list.

> itemCtx :: Tags → Context String
> itemCtx tags 	= tagsField "tags" tags
>              	⊕ dateField "date" "%e %B, %Y"
>              	⊕ urlField "url"
>              	⊕ defaultContext

In other words, to display a single item, we need the post's tags, its date
and URL, and the default context, which will give us its title.

Given this `Context`, Hakyll provides a function called `applyTemplateList`
which allows us to apply a template over each `Item` in a list, turning a
`Compiler` for a list of `Item`s into a `Compiler` for a single `Item`
representing the whole list.

> postList 	:: 	Tags
>          	→ 	Pattern
>          	→ 	([Item String] → Compiler [Item String])
>          	→ 	Compiler String
> postList tags pattern sortFilter = do
>   ps      	← sortFilter =≪ loadAll (pattern .&&. hasNoVersion)
>   itemTpl 	← loadBody "templates/post-item.html"
>   applyTemplateList itemTpl (itemCtx tags) ps
>     ≫= removeIndexHtml

The `sortFilter` parameter to this function allows us to order the `Item`s
based on their `Context`.  As you can see in the call to `postList` in
`indexCompiler` above, we pass in `recentFirst` to sort the posts by date.

Dealing with tags
-----------------

Extending our system to deal with tags is easy -- we just write another set of
`Rules` similar to `index` which generates a page for each tag.

> tagIndex :: Tags → Rules ()
> tagIndex tags = tagsRules tags $ \_ pattern → do
>   route $ gsubRoute ".html" (const "/index.html")
>   compile $ indexCompiler tags pattern defaultContext

The default `tagsRules` provided by Hakyll put the tags in a file called
`tagname.html`, so we use `gsubRoute` here to rename that to
`tagname/index.html` and get a prettier URL.

As an added bonus and to give us a way to browse all tags in the site, let's
generate a [tag cloud][tags].  This is a bit of an easter egg as I haven't linked to
it anywhere on the site except in this post, but you can always access it by
going to <http://dpwright.com/tags>.

First we define our context.

> tagCloudContext :: Tags -> Context String
> tagCloudContext tags 	= tagCloudField 	"tagCloud" 100 500 (shuffle tags)
>                      	⊕ bodyField     	"body"
>                      	⊕ urlField      	"url"
>                      	⊕ pathField     	"path"
>                      	⊕ missingField  	
>   where shuffle = sortTagsBy ∘ comparing $ hash ∘ fst

Most of the legwork here is done by `tagCloudField`, which is provided by
Hakyll.  As mentioned earlier, we sort the tags by the hash of their name,
which gives us a random-looking, but in fact predictable, order for the tags.
It is important that this ordering is deterministic and not random, otherwise
I'd have to regenerate the tags page even when no changes had been made to the
site!

Finally we describe the `Rules` for the `tagCloud` itself.  I've written the
`Compiler` inline here as we don't need to make use of it elsewhere and that
keeps things simple.

> tagCloud :: Tags → Rules ()
> tagCloud tags = create ["tags/index.html"] $ do
>   let tagsCtx = tagCloudContext tags
>   route idRoute
>   compile $ makeItem ""
>     ≫= loadAndApplyTemplate "templates/tags.html"    tagsCtx
>     ≫= loadAndApplyTemplate "templates/default.html" tagsCtx
>     ≫= relativizeUrls
>     ≫= withItemBody removeIndexHtml

And that's it!  The site is now fully indexed, with a single main index on the
front page an an index per-tag as well.

[generating-this-website]: http://www.dpwright.com/tags/generating%20this%20website
[main page]: /
[tags]: /tags
[introduction]: /posts/2014-08-04-generating-this-website-part-1-introduction
[posts]:           /posts/2014-09-29-generating-this-website-part-2-posts
[cm-unicode]: http://checkmyworking.com/cm-web-fonts/
[base-unicode-symbols]: https://hackage.haskell.org/package/base-unicode-symbols
