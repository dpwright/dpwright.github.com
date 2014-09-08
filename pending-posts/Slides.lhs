---
date: 2014-08-16 14:23:00
tags: literate-programs, hakyll, generating this website
title: Generating this website part x: Slideshows
---

Occasionally I give talks for which I want to write up an accompanying blog
post.  Since Pandoc supports various slideshow formats directly, I can actually
use the same system both for my blog posts and for writing my slides -- in fact,
I can use the same source to produce all three: the post, the slides, and any
accompanying code -- using literate programming!

Of course, this won't always work.  Sometimes, the way you present things in a
talk must flow quite differently from the way you present them in a blog post.
But this is a limitation of the presentation media, not a technical limitation,
and so I'd like to support both formats.  Here's an outline of what we're going
to try and achieve:

* If the post is in the `posts/` directory, it will be rendered into a blog post
  as usual.
* If the post has the metadata field `slides`, it will be processed as a
  slideshow of the format specified in that field.  So, if the `slides` field
  contains the string `RevealJSSlides` then the file will generate a `reveal.js`
  slideshow; if it contains `S5Slides` it will be formatted as S5, and so on.
  For a full list of supported slideshow formats, see the [pandoc
  documentation][pandocs-slides].
* The slides compiler will search both the `posts` and `slides` directories, so
  that it is possible to generate slides without generating a post.
* Any files in the `slides/` directory which do not contain the 'slides'
  metadata field will default to `RevealJSSlides`
* Slides will be output into the `slides/` directory, regardless of which
  directory they came from.
* If the file extension is `.lhs`, the file will be treated as literate haskell
  as usual.

All in all, this set of requirements should be fairly simple; the only
potentially tricky part is searching for slides using the posts' metadata rather
than their filename, and even that should just come down to quitting out of the
compiler early in case the `slides` field isn't present.

Preliminaries
-------------

As always, we begin by specifying `OverloadedStrings` and importing `Hakyll`.

> {-# LANGUAGE OverloadedStrings #-}
> module Slides where
> import Hakyll

Monoid's `mappend` operator is also useful to have.

> import Data.Monoid   ((<>))

We'll be making use of a custom pandoc compiler to actually output the slides,
so we'll need to bring the appropriate pandoc options into scope.

> import Text.Pandoc.Options (WriterOptions (..), HTMLSlideVariant(..))

I'm also going to import the `Posts` module as the compiler I use for generating
slideshows will be similar to the one used for generating standard blog posts,
so I'll want to reuse that.

> import Posts

Locating slideshow files
------------------------

The first step is to create some Hakyll `Rules` specifying which files to match
and where to put them.  The requirements outlined above essentially specify two
alternatives:

* Files in the `posts` directory which *must* contain the `slides` metadata
  field.
* Files in the `slides` directory which *may* contain a `slides` field (and if
  not default to `RevealJSSlides`.

Let's approach these in order.  Firstly, the slides in the `posts` directory:

<div class="sidenote">
TODO: The following code doesn't actually work yet, so it is commented out.  I
am still looking for a good way to make Rules depend on metadata rather than
just the filename.
</div>

>-- slideshowPosts :: Rules ()
>-- slideshowPosts = do
>--   ids <- getMatches "posts/*"
>--   forM_ ids $ \ident -> do
>--     slides <- getMetadataField ident "slides"
>--     case slides of
>--       Nothing -> return ()
>--       Just s  -> rulesExtraDependencies [IdentifierDependency ident] $ do
>--         undefined

Next the slides in the `slides` directory.

> slideshows :: Rules ()
> slideshows = match "slides/*" $ do
>   route $ setExtension ".html"
>   compile $ getUnderlying
>     >>= (`getMetadataField` "slides")
>     >>= slidesCompiler . maybe SlidySlides read
>     >>= relativizeUrls

Generating the slideshows
-------------------------

Now that we know which files we should be creating slideshows for, we need a
compiler to do the actual generation!  As usual, we start with the context,
which is very simple -- just attach a date to the default context.

> slidesCtx :: Context String
> slidesCtx = dateField "date" "%e %B, %Y"
>          <> defaultContext

Pandoc supports a variety of different slideshow formats, and I haven't settled
on one in particular, so I'll add support for them individually as I try them
out.  Each slideshow engine requires a different template, so we'll make a quick
lookup for that.

> slidesTemplate :: HTMLSlideVariant -> Identifier
> slidesTemplate RevealJsSlides = "templates/slides/reveal.js.html"
> slidesTemplate S5Slides       = "templates/slides/s5.html"
> slidesTemplate SlidySlides    = "templates/slides/slidy.html"

Note that this is a partial function -- I haven't added support for all the
slideshow engines yet so if you try to use one without a template the Hakyll
build will fail.

Another difference between the various HTML slideshow engines is which HTML they
expect to be working with!  Reveal.js expects HTML 5, while the others I've
tried all work better with HTML 4 source.  I've made a quick lookup for that as
well.

> slidesExpectHTML5 :: HTMLSlideVariant -> Bool
> slidesExpectHTML5 RevealJsSlides = True
> slidesExpectHTML5 _              = False

Finally, the compiler itself!  This takes a standard pandoc compiler, adds the
`readerOptions` and `writerOptions` we defined in the entry on standard
[posts], and then customizes them with slideshow-specific functionality.

> slidesCompiler :: HTMLSlideVariant -> Compiler (Item String)
> slidesCompiler sv = pandocCompilerWith readerOptions slideWriterOpts
>                 >>= loadAndApplyTemplate (slidesTemplate sv) slidesCtx
>   where slideWriterOpts = writerOptions
>                         { writerSlideVariant = sv
>                         , writerHtml5        = slidesExpectHTML5 sv
>                         , writerIncremental  = True
>                         }

Added extras
------------

That's really all there is to it on the Hakyll side!  To make writing slides and
blog posts in the same file easier, I have a couple of CSS `div` classes set up
in the slideshow style files -- for instance, anything inside a `<div
class="notes">` will be shown in the blog post but not in the slideshow.  I have
yet to add the reverse functionality but I can't imagine it would be too
difficult.  All that can be done with a bit of CSS cleverness, though, with no
special support from Hakyll itself.

[pandocs-slides]: https://hackage.haskell.org/package/pandoc-1.13/docs/Text-Pandoc-Options.html#t:HTMLSlideVariant
[posts]:          #todo
