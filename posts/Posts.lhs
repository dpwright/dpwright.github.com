---
date: 2014-09-29 09:57:39
tags: hakyll, literate-programs, generating this website
title: Generating this website part 2
subtitle: Posts
clone: posts/2014/09/29/generating-this-website-part-2-posts/index.html
---

<div class="sidenote">
This is part two of the "generating this website" series.  To read the rest of
the series, go to the series index [here][generating-this-website]
</div>

OK, time to dive into arguably the most important part of the website generator
-- generating the posts themselves!  Really, this is what it all comes down to;
any other features like tags, rss feeds, crossposting and so on are just window
dressing.  Generating posts is not actually that hard, but you'll find it's one
of the most often-tweaked part of your Hakyll setup as most changes you want to
make affect posts in some way.  Let's get started!

Preliminaries
-------------

We start with our usual `OverloadedStrings` definition and `Hakyll` import.
Because we're developing a module now, rather than the main entry point of our
program, we also need a module header.  This was missing from the last post but
will be present in all the remaining modules in this series.

> {-# LANGUAGE OverloadedStrings #-}
> module Posts where
> import Hakyll

Note that since the module is called `Posts`, the file itself must also be
called `Posts.hs`, or rather `Posts.lhs` as it is a literate Haskell document.
This has a bit of a knock-on effect, as Hakyll's default behaviour is to expect
that all files will follow a `date-title` convention for their filenames, so
already we will have to do some extra work to get it to draw that information
from the Pandoc header instead of the filename.

The following imports give us some extra helper functions which we'll use
throughout our program.  These functions (particularly the operators) are so
common I almost wish they were part of the standard prelude, but they aren't so
we'll have to live with a longish import list.  I won't go into too much detail
here but as I've used explicit imports you can see exactly which functions I'm
importing from where.

> import Data.Monoid         	((<>))
> import Data.Maybe          	(fromMaybe)
> import Data.List           	(intercalate, isInfixOf)
> import Data.Char           	(toLower, isAlphaNum)
> import Control.Applicative 	((<$>), (<*>))
> import Control.Monad       	(msum)

I'm going to be making use of a few system/date related functions to handle the
date specified in the header and rename the file appropriately.

> import System.FilePath  	(replaceBaseName, takeDirectory,
>                         	 takeBaseName, splitFileName, (</>))
> import Data.Time.Clock  	(UTCTime (..))
> import Data.Time.Format 	(formatTime, parseTimeM, defaultTimeLocale)

The `Set` module exports function names that clash with those from the standard
prelude for working with lists, so I'll import it qualified here.  In fact, I
only make use of one function from it (`union`), so I could have just imported
this function and had done with it, but it's common form to import data
structures like this qualified, so I'm in the habit of it.

> import qualified Data.Set as S

Finally some more specific imports.  I'll be overriding some of Pandoc's
default options so I'll need to bring those into scope.

> import Text.Pandoc.Definition 	(Pandoc(..))
> import Text.Pandoc.Options    	(ReaderOptions(..),
>                               	 WriterOptions (..),
>                               	 Extension (..),
>                               	 HTMLMathMethod(..), def)

As well as that, I'm going to import the `Crossposting` and `ElasticTabstops`
modules which we'll cover later in the series.

> import Crossposting
> import ElasticTabstops

After all that, we can actually get on with writing some code!  If you're new to
Haskell, don't worry too much about all these imports -- in general you just add
them as you come across functionality you need to use which is defined in
another module, so you don't really need to think too much about them ahead of
time.  Because Haskell encourages breaking things down into small, reusable
components, import lists can get quite long.  This is a good thing!

Pandoc options
--------------

To begin with, I'm going to define the custom version of the Pandoc compiler
we'll use to generate the posts.  Hakyll comes with some reasonable defaults,
but I'd like to tweak it a little to allow support for features specific to my
needs here -- in particular, I want support for:

- Literate Haskell (or you wouldn't be reading this!)
- MathJax
- Syntax Highlighting
- Smart Parsing (conversion of `--` to --, and so forth)

The compiler itself is just a standard compiler with different reader and writer
options, and some pandoc-level transformations:

> customCompiler :: Compiler (Item String)
> customCompiler = 	pandocCompilerWithTransform
>   readerOptions writerOptions pandocTransforms

Those options are defined in terms of Pandoc's defaults, provided by the
`Default` typeclass, which allows you to specify a default definition `def` for
any type.  First we tell the reader to add `readerSmart` to its options:

> readerOptions :: ReaderOptions
> readerOptions = def { readerSmart = True }

The writer options are manipulated in a similar way, adding MathJax support,
syntax highlighting, and literate Haskell.

> writerOptions :: WriterOptions
> writerOptions = def
>   { writerHTMLMathMethod = MathJax ""
>   , writerHighlight      = True
>   , writerExtensions     = extensions
>   }
>   where extensions 	= writerExtensions def `S.union` S.fromList
>                    	[ Ext_literate_haskell
>                    	]

The `MathJax` constructor takes a string to supply the URL to `mathjax.js`, so
that it can embed it in the output HTML, however in this case Hakyll overrides
Pandoc's default HTML output to insert our templates, and it is there that we
import `mathjax.js`, so we can just pass the empty string here.

Defining `extensions` as a union of the default extensions with a single-member
set may seem like overkill, and for only one item it is, but doing it this way
means that if I ever want to add an extension I can just add it to the list.

Finally, we define `pandocTransform`, for our post-processing needs.  This is
just the composition of any pandoc-level filters we want to apply -- see the
posts for those individual filters for more information.

> pandocTransforms :: Pandoc -> Pandoc
> pandocTransforms = elasticTabstops

Generating posts
----------------

Here begins a pattern that you will see a lot of.  In Hakyll, the way you
generate anything is by defining a `Compiler`.  Usually, that `Compiler` is
paired with a `Context` which provides all the variables you may want to make
use of in your template.  Finally, we tie that `Compiler` to a specific set of
inputs using `Rules`, which we covered in the previous post.  Often, people
write all their `Rules` inline in one big `main` function, but to make breaking
the configuration over a number of blog posts easier, I've elected to define
each set of `Rules` as an independent function which I call from `main` in the
first post.

First, then, the `Context`, which simply extracts data from the metadata header
at the top of the file.

> postCtx :: Tags -> Context String
> postCtx tags 	 =  	dateField "date" "%e %B, %Y"
>              	 <> 	tagsField "tags" tags
>              	 <> 	crosspostField "xp"
>              	 <> 	defaultContext

As well as the `defaultContext`, which gives us some common fields such as
`title`, we make use of the `date`, `tags`, and `xp` fields.  The first two
ought to be fairly self-explanatory -- the date is displayed at the top of this
page and the tags are listed at the bottom.

`crosspostField` will be defined in a later post in this series.  It is there
for the case where a post on this blog is also posted on another blog -- usually
I've written it for the other blog but received permission to reproduce it here.
In that case, I want to include a header at the top of the file giving a link to
the original blog post.

<div id="sidenote">I also make use of Hakyll to allow me to write the post
here in markdown and have it automatically uploaded to both my blog and the one
where I'm guesting -- at least if the latter is a Wordpress blog and I have
posting rights there.  But we'll talk about that later.</div>

The `Compiler` follows standard conventions: run the Pandoc compiler (in this
case our `customCompiler` defined above), apply templates, and fix up the URLs.

> postCompiler :: Tags -> Compiler (Item String)
> postCompiler tags = customCompiler
>   >>= loadAndApplyTemplate "templates/post.html"    ctx
>   >>= saveSnapshot "content"
>   >>= loadAndApplyTemplate "templates/default.html" ctx
>   >>= relativizeUrls
>   >>= withItemBody removeIndexHtml
>   where ctx = postCtx tags

Hang on, what's that `saveSnapshot` in the middle there?  I never mentioned
that!  Well, that allows us to save a snapshot of our page *as it currently
stands* at that point in the compilation.  This is handy, because we'll want to
include the content of the post in RSS feeds and the like, but at that point we
only want the content, not the header, footer, and navigation.  It is useful,
then, to be able to save out a copy at the point where that is all we have and
re-use it later, rather than having to recompile the whole thing again when
generating feeds.

Finally, our `Rules` tell Hakyll where to get posts from, how to compile them,
and where to put them.

> posts :: Tags -> Rules ()
> posts tags = match ("posts/*" .||. "pending-posts/*") $ do
>   route $ 	metadataRoute dateAndTitle `composeRoutes`
>           	customRoute simplifyURL
>   compile $ postCompiler tags

This is mostly pretty simple.  You can see we generate posts from both the
`posts` and `pending-posts` directories (the latter are generated, but not
included in the index, so I can preview them because I know the URL but they're
not "published" as such).  We use the `date` and `title` metadata fields to
generate a filename and then from that create a simplified URL.  Finally we
compile it with the `postCompiler` we defined above.

I took the idea (and the code) for the simplified URL route from [Yann
Esposito's Hakyll setup][yehs].  Instead of `post-name.html`, it outputs a
file to `post-name/index.html`, allowing us to drop the `.html` part when
visiting the page in the browser.  It is defined as follows.

> simplifyURL :: Identifier -> FilePath
> simplifyURL ident =
>   takeDirectory p </> takeBaseName p </> "index.html"
>   where p = toFilePath ident

This works nicely, but as Yann points out in his post it leaves a lot of links
with `index.html` at the end of them floating around.  We basically never want
this, so Yann suggests the following code to strip `index.html` from the end
of all links (I've modified it slightly to work with `String`s instead of
`Item`s).

> removeIndexHtml :: String -> Compiler String
> removeIndexHtml body = return $ withUrls removeIndexStr body
>   where
>     removeIndexStr url = case splitFileName url of
>       (dir, "index.html") | isLocal dir 	-> init dir
>       _                                 	-> url
>     isLocal uri = not $ "://" `isInfixOf` uri

That all fits together quite nicely.  There's just one snag... that
`dateAndTitle` function passed to `metadataRoute` doesn't actually exist!
We're going to have to write it.

Extracting the date and title from metadata
-------------------------------------------

As mentioned, Hakyll by default expects the date and title to be included in the
filename and as such it can just change the extension and have done with it.
Because we might be naming files after the modules they define (in the case of
Literate Haskell files), a post such as this one would end up with a URL looking
like <http://dpwright.com/posts/Posts.html>, which would be very odd.  Even
without this, it's quite easy to write a post which accidentally has a subtly
different title to its filename, which would be confusing.

Because of this, we'd much rather pull the `date` and `title` fields out of the
post's metadata, which ensures consistency and reduces repetition.
Unfortunately, Hakyll provides no clear way to do this natively, so we have to
make use of its generic functionality for routing based on metadata.  We do this
using the `metadataRoute` function, above, which takes as its parameter a
function taking a posts `Metadata` and returning the `Routes` associated with
that metadata, and returns `Routes` which can be used by the `Rules` to which is
it passed.  Its type signature looks like this:

```haskell
metadataRoute :: (Metadata -> Routes) -> Routes
```

The function we passed to `metadataRoute` above was `dateAndTitle`, which we'll
define here.

> dateAndTitle :: Metadata -> Routes
> dateAndTitle meta = fromMaybe idRoute $
>   mkName <$> getField "title" <*> getField "date"
>   where 	mkName t d 	= 	setBaseName $ date d </> title t
>         	getField   	= 	(`lookupString` meta)
>         	date       	= 	formatTime defaultTimeLocale
>         	           	  	"%Y/%m/%d" . readTime
>         	title      	= 	map toLower . intercalate "-"
>         	           	. 	map (filter isAlphaNum) . words

There's a lot going on in this definition so we'll go through it carefully.

- We begin with a call to `fromMaybe` passing `idRoute` as the fallback.  This
  means that what follows might fail, and should it fail we'll just use the
  filename as-is (falling back on Hakyll's default behaviour).
- `mkName` is called in applicative style, passing two calls to
  `getField` (defined locally).  We know that `Maybe` forms an `Applicative`,
  and that we are expecting a `Maybe` here as the second parameter to
  `fromMaybe`.  So we can infer what will happen here: it will try to get the
  `title` and `date` fields, and if either of them fail it will return
  `Nothing`, otherwise it will pass them both to `mkName`.
    - If you are familiar with applicative style this will have been immediately
      obvious.  If not, it is worth reading through the previous bullet-point
      and associated code a few times until you get a feeling for what's
      happening.  We've reduced what would have been a lot of sanity checking
      and nested `if` statements into a single line of code which, when you are
      used to this style, reads extremely clearly.  It's a very powerful
      technique.
- Moving onto the local definitions: `getField` is simply a shortcut for
  calling the `lookupString` function in order to get the respective fields out
  of the passed `Metadata`.
- `mkName` takes the title and the date as parameters, calls the `date`
  and `title` functions in order to turn them into strings, and then sticks them
  together with a `/`.  Finally it calls `setBaseName` (defined below), which
  works similarly to Hakyll's native `setExtension` except that, obviously, it
  sets the basename.
- `date` normalises the `date` field to the simple `YYYY/mm/dd` format we want
  to use for our filenames.  This means you can write the date in any of
  Hakyll's supported date formats and the filename will turn out OK.
- `title` splits the title up into words, filters out any strange symbols using
  `isAlphaNum` (which, thankfully, is Unicode-friendly so that Japanese titles
  aren't considered "strange symbols"), and then joins those words back up with
  `-` dashes so that we don't have to worry about `%20`s appearing all over our
  URLs.  Finally, it makes the whole thing lower-case.

Function definitions like the one above are part of the reason I love Haskell
and others might hate it.  There's so much functionality packed into so little
code there, you do have to slow down a little bit and read it carefully to
follow it, at least until you are used to code written in the style used (in
this case, some applicative style and a little bit of point-free notation thrown
in for good measure).  Add to this the expressivity of Haskell which allows for
a number of different styles, so that even once you've got used to the style
used here you may open another codebase and find that the style employed there
is completely different!  There is a not-unreasonable argument that this is
problematic; that encouraging a very particular style at the language level (as
Python does, for example) makes it a lot easier to read unfamiliar code.

I am sympathetic to this argument up to a point.  It does make sense, especially
if you are dealing with large numbers of programmers relatively inexperienced in
the language (thus not exposed to the various styles of programming available),
who regularly have to jump into different codebases (thus run into these
different styles frequently).  However, I do think the benefits outweight the
disadvantages.  Firstly, inexperienced programmers are likely to be limited to
relatively isolated areas of the code, so they will have time to get used to
whatever style is employed there before moving on and learning some new style
along with the next codebase.  Secondly, *once you have learnt* the style, it is
actually dramatically *faster* to read succinct code like this than trudging
through reams of `if` statements and manual `for`-style loops.  It's also harder
to make mistakes, as the code fits more closely with the thing it's trying to
do.

So there is a learning curve, and learning curves cost time and money when
training programmers.  But when the initial hump is traversed[^1], the increase
in productivity is well worth the effort.

OK, after that little detour, let's get back to it!  The `dateAndTitle` function
above made use of two helper functions which haven't actually been defined.  The
first is `readTime`, which we use to normalise the date format.  It takes a date
string and converts it to a `UTCTime` which we can manipulate.

> readTime :: String -> UTCTime
> readTime t = fromMaybe empty' . msum $ attempts where
>   attempts 	= [parseTimeM True defaultTimeLocale fmt t | fmt <- formats]
>   empty'   	= error $ "Could not parse date field: " ++ t
>   formats  	= [ "%a, %d %b %Y %H:%M:%S %Z"
>            	  , "%Y-%m-%dT%H:%M:%S%Z"
>            	  , "%Y-%m-%d %H:%M:%S%Z"
>            	  , "%Y-%m-%d %H:%M"
>            	  , "%Y-%m-%d"
>            	  , "%B %e, %Y %l:%M %p"
>            	  , "%B %e, %Y"
>            	  , "%b %d, %Y"
>            	  ]

The basic idea for the implementation is taken from Hakyll itself, from its
`getItemUTC` which is defined in [`Hakyll.Web.Template.Context`][hwtc].
Unfortunately, the type signature for that function is quite a lot more
complicated than we need, so I've extracted the parts we need into a simple
`String -> UTCTime` function here.  If the date doesn't match any of the
supported formats `readTime` will simply crash with an error -- not the best
error handling but since we're always going to be running this interactively it
doesn't really matter.

`setBaseName` turns a string into a `FilePath`, which it can then manipulate
using Haskell's native `replaceBaseName` functionality.

> setBaseName :: String -> Routes
> setBaseName basename = customRoute $
>   (`replaceBaseName` basename) . toFilePath

Pages
-----

Another form of content on this blog is that of "pages", which are basically
posts except that they don't have a date or tags associated with them and they
are not indexed or included in feeds.  As a result they are super-simple -- we
don't need to save a snapshot, or to parse the date or change the filename.
Instead I can just compile it with a template designed for the purpose and set
the extension.  We'll use the same `customCompiler` as posts for consistency,
but we'll just pass the `defaultContext` as we don't need any of the extra
metadata posts use.

> pageCompiler :: Compiler (Item String)
> pageCompiler = customCompiler
>   >>= loadAndApplyTemplate "templates/page.html"    	ctx
>   >>= loadAndApplyTemplate "templates/default.html" 	ctx
>   >>= relativizeUrls
>   where ctx = defaultContext

The rules for pages are equally simple -- just grab anything from the `pages`
folder, compile it using the `pageCompiler` and set its extension to `html`.
This is expressed below.

> pages :: Rules ()
> pages = match "pages/*" $ do
>   route $ customRoute simplifyURL
>   compile pageCompiler

Conclusion
----------

That's about it for compiling posts!  This is *almost* all you need to get a
Hakyll site up and running -- the only problem is since we still don't have post
indexing you have to know the URL of the post you want to read before you read
it!  This is about as complicated as it gets with Hakyll though -- if you've
followed this post, the rest should be easy!  We'll cover indexing next, after
which we'll go about adding special features one at a time.

[^1]: Anybody who's read code using the `Lens` library will get the joke.

[generating-this-website]: http://www.dpwright.com/tags/generating%20this%20website
[hwtc]: https://hackage.haskell.org/package/hakyll-4.2.2.0/docs/src/Hakyll-Web-Template-Context.html
[yehs]: http://yannesposito.com/Scratch/en/blog/Hakyll-setup/
