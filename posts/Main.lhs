---
date: 2014-08-04 08:38:23
tags: literate-programs, hakyll, generating this website
title: Generating this website part 1: Introduction
---

<div class="sidenote">
This is part one of the "generating this website" series.  To read the rest of
the series, go to the series index [here][gensite]
</div>

I've recently migrated this website to a new server, and while I was at it I
thought I'd have another shot at giving it a reboot and actually keeping it
up-to-date this time.  I'm much more experienced in both Haskell and literate
programming than I was when last I updated it, so when I was modifying my
configuration it suddenly occurred to me that since this site is generated using
Haskell, and since Haskell supports literate programming natively, I could very
easily make the site "self-hosting" by writing up the configuration itself as a
series of blog posts!

Thus begins this series, unimaginatively named [Generating this Website][gensite].
To recap, the site is generated using [Hakyll][hakyll], a Haskell library for
generating static websites.  I will write this series assuming basic Haskell
knowledge; you should be familiar with standard typeclasses like `Monoid` and
`Applicative`.  Specific knowledge of Hakyll is not required, and if an idiom
feels at all obscure I will attempt to explain it as I go.  If there's anything
you don't get, you can always drop me a tweet @[danielpwright][danielpwright]!

This post will just cover the basics -- for details of the generation of actual
posts, the index pages, and so on, check back for future posts!  I will update
this post with links as I write them.

Preliminaries
-------------

To begin with, a couple of lines which will grow familiar as this series
progresses:

> {-# LANGUAGE OverloadedStrings #-}
> import Hakyll

The `OverloadedStrings` LANGUAGE pragma is especially convenient and to be
honest I wish it was just defined by default, since I end up including it in
nearly any file I write.  You can set this in the `cabal` file for your project,
but if I'm going to make use of LANGUAGE pragmas I'd rather be explicit so I'll
just include them in every file.  After that, of course, is the Hakyll import.
I'm importing this unqualified in order to make use of the EDSL Hakyll supplies.

I'm also going to import `liftA2` from `Control.Applicative` here.  I use this
as a convenience later.

> import Control.Applicative (liftA2)

Finally, I'll import the other posts in this series.  These supply much of the
actual functionality.

> import Posts
> import Indexing
> import Feed
> import Slides
> import OutdatedURLs

Some simple rules
-----------------

The main entry point to Hakyll takes a set of [`Rules`][hakyllrules] and
returns an `IO` action which generates the site.  `Rules` themselves form a
monad, so assuming we have some simple rules:

> templates, images, css, static :: Rules ()

We can put them together by simply listing them using `do`-notation.

> simpleRules :: Rules ()
> simpleRules = do
>   templates
>   images
>   static
>   pages
>   css
>   slideshows

<div class="sidenote">Note: the `slideshows` rule above will be
defined in future blog posts</div>

The rules themselves govern the compilation and generation of files.  Perhaps
the simplest of these is `templates`, which compiles all files found in the
`templates` directory and any subdirectories, but doesn't actually need to
output those files anywhere -- instead it keeps the compiled versions around for
other pages using that template.

> templates = match "templates/**" $ compile templateCompiler

Hakyll provides a [`Pattern`][hakyllpattern] type which, conveniently,
implements `IsString` so our `OverloadedStrings` pragma takes care of the
conversion for us.  The `**` pattern searches in that directory and all
subdirectories.

Next up come the images.  These are also very simple -- simply take the full
path of the images, and copy them to the same place in the output directory.

> images = match "images/*" $ do
>   route   idRoute
>   compile copyFileCompiler

The `route` rule defines the output filename.  `idRoute`, as the name implies,
sets the output filename to match the input filename.  Any rule which generates
output requires a `route` -- any rule without a `route` will be run, but won't
generate any output (like the `templates` rule above).

For CSS files, Hakyll provides a compressor to speed download times.

> css = match "css/*" $ do
>   route   idRoute
>   compile compressCssCompiler

Of course, the `copyFileCompiler` would work just as well, but we might as well
compress the CSS while we're at it.

Occasionally, I just want to put some static files up that don't fit the
structure of the rest of the blog.  This is particularly useful when I want to
upload slides from a talk I've given, for example the [git talk][gits-guts] I
gave a couple of months ago.  The talk itself is maintained in a different
repository, so it's convenient if I can just include that as a submodule and
have its files copied automatically.  I do this by storing all such content in
the `static` directory, and then copying it when generating the site, stripping
the initial `static/` from the output path.

> static = match "static/**" $ do
>   route $ gsubRoute "static/" (const "")
>   compile copyFileCompiler

`gsubRoute` is actually quite powerful, allowing us to change our substitution
based on the matched input, but in this case we just want to substitute for the
empty string every time, so we use `const` to drop the argument.

Tags, and the `Rules` which require them
----------------------------------------

The remaining rules are complicated by the fact that they need access to the
tags for various reasons -- the tag index pages obviously need to list all posts
matching a certain tag, while the posts themselves and the Atom feed list the
tags for a particular post at the bottom of the post.

In order to do this, we first need to generate the tags for the site, and then
we need to pass these into those `Rules` that make use of them.  Generating the
tags is quite easy:

> generateTags :: Rules Tags
> generateTags = buildTags "posts/*" $ fromCapture "tags/*.html"

Here I use `buildTags` to get the tags from all files in the `posts` directory.
The default method of tagging posts is just to include a `tags` field in the
post's metadata, but if I wanted to do it some other way I could use
`buildTagsWith` instead.

`fromCapture` acts sort of like a `Pattern` in reverse; it fills in the capture
(The `*` in `tags/*.html` in this case) with a given string.  We use that to
say, "for every tag read from the posts' metadata, create an index page at
'tags/TAGNAME.html'".

Having generated the tags, we need to pass them into any rules that need them.
We could use `do`-notation as we did for `simpleRules` and simply pass the
`tags` parameter to each entry, but here I'm going to use a little `Applicative`
trick which allows me to keep the function point-free, and I think makes it read
a little more declaratively.

> taggedRules :: Tags -> Rules ()
> taggedRules = posts & outdatedURLs & index & tagIndex & tagCloud & feed
>   where (&) = liftA2 (>>)

This trick exploits the fact that `(->)`, the type of functions, implements
`Applicative` (in fact being applicative is rather their *raison d'être* when
you think about it), so if we lift the Monadic `(>>)` operator to act on
*applications of functions returning a Monad* instead of just Monads, we can
pass the parameter to the function in once and it will be distributed to each of
those functions.  In other words:

```haskell
posts tags >> tagIndex tags >> feed tags ≡ (posts & tagIndex & feed) tags
  where (&) = liftA2 (>>)
```

Because of Haskell's function currying and η-reduction, we can then drop the
`tags` parameter and the brackets entirely and we're left with the definition
for `taggedRules` above.

Putting it all together
-----------------------

Finally we define the entry point to the application.  This simply calls
Hakyll's own `hakyll` function, passing in the rules defined above.
First we call the simple, self-standing rules, then we generate the tags and
pass them to the tagged rules.

> main :: IO ()
> main = hakyll $ do
>   simpleRules
>   generateTags >>= taggedRules

This concludes the introduction to Hakyll and the entry point for the generation
code for this website.  Stay tuned for the next entry, where we'll add the
configuration to actually create the posts themselves!

[gensite]:        /tags/generating%20this%20website
[hakyll]:         http://jaspervdj.be/hakyll
[danielpwright]:  http://twitter.com/danielpwright
[hakyllrules]:    http://jaspervdj.be/hakyll/reference/Hakyll-Core-Rules.html#t:Rules
[hakyllpattern]:  http://jaspervdj.be/hakyll/reference/Hakyll-Core-Identifier-Pattern.html#t:Pattern
[gits-guts]:      /gits-guts
