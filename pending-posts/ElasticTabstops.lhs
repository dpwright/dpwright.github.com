---
date: 2015-04-30 14:59:26
tags: hakyll, literate-programs, generating this website
title: Generating this website part 6: Elastic Tabstops
---

<div class="sidenote">
This is part six of the "generating this website" series.  To read the rest
of the series, go to the series index [here][generating-this-website]
</div>

Over the New Year holidays this year I redesigned the site, incorporating a
number of changes which (I hope) make it easier to read and nicer-looking to
boot.  These changes include:

- Removing most of the "link clutter" from the header, replacing it with just
  the title and a single <i class="fa fa-question-circle"></i> link, which
  takes you to an "about" page.
- Pulling in the margins, putting the body of the article in a single,
  narrower column, which is easier for the readers' eyes to track.
- Changing the typefaces using a pair of fonts from Donald Knuth's beautiful
  $\LaTeX$ typesetting system: [Computer Modern][cm] for the body and
  [`Concrete Roman`][cr] for preformatted/code blocks.  The webfont versions
  of these fonts were downloaded from [this site][cm-web-fonts].

The particularly perspicacious amongst you might have noticed an issue with
this choice of typefaces, however.  That is, `Concrete Roman` uses
proportional spacing -- it is *not* a monospaced font!  And yet, all the code
samples are aligned nicely.  What dark magic is this?

Elastic tabstops
----------------

[Elastic tabstops][elastic-tabstops] were invented by Nick Gravgaard with the
twin goals of ending the interminable tabs/spaces argument and allowing code
to be lined up nicely even when using proportional fonts.  The basic idea is
simple -- treat tabs not as a simple "jump to the next multiple of N"
shortcut, but more as the delimiter of a table whose columns represent the
layout you want your code to take.  This animated GIF, taken from [his
website][elastic-tabstops], demonstrates the idea neatly:

<center>![](columnblocks_coloured.gif "Elastic tabstops demonstration")</center>

Gravgaard had live editing in mind when he invented the concept, and has
written plugins for a number of popular editors, however it applies just as
well to static *display* of code, such as on a website.  The implementation
here simply does literally what the simple description of elastic tabstops
says: it walks a Pandoc document looking for code blocks, and turns them into
tables delimited by the tab.

Incidentally, there is nothing Hakyll-specific about this implementation -- it
is a post-process transformation on the Pandoc document.  So it should support
any of Pandoc's output formats, in case you want to do something similar with
your next LaTeX paper!

Preliminaries
-------------

The standard opening:

> {-# LANGUAGE UnicodeSyntax #-}
> {-# LANGUAGE OverloadedStrings #-}
> module ElasticTabstops where
> import Prelude.Unicode

Note I didn't need to import *`Hakyll`*.  We don't use it here; this is pure
Pandoc.  We do need that though:

> import Text.Pandoc.Definition
> import Text.Pandoc.Walk

We're also going to be doing some fiddling with lists, so we'll import
some utilities from *`Data.List`* and *`Data.List.Split`*.

> import Data.List       	(delete)
> import Data.List.Split 	(splitOn)

Code overview
-------------

We're looking for a *`Pandoc`* `→` *`Pandoc`* transformation, which will walk
through the tree and, if it finds a code block, "elasticate" it by
transforming tabs into table columns.  Seems simple enough:

> elasticTabstops :: Pandoc → Pandoc
> elasticTabstops = walk $ ifCodeBlock elasticate

`walk` actually expects a function which takes a *`Block`* and returns the
transformed *`Block`*, so we define `ifCodeBlock` to run our function if the
block is a *`CodeBlock`*, and just return it unmodified otherwise.

> ifCodeBlock :: (Attr → String → Block) → Block → Block
> ifCodeBlock f (CodeBlock a s) 	= f a s
> ifCodeBlock _ b               	= b

The `elasticate` function, then, will take an *`Attr`* -- the id, classes, and
key-value pairs associated with a block -- and a *`String`* representing the
code itself, and return a new *`Block`*.  We'll wrap the generated "tables of
code" in a *`Div`* with class `elastic-tabstops` in case we want to do any
styling on it, or any further post-processing.

> elasticate :: Attr → String → Block
> elasticate a s = Div ([], ["elastic-tabstops"], []) $ codeTables a s

Grouping the code
-----------------

Why "tables of code", plural?  Because in order to line the code up sensibly,
we actually need to split it up into groups, and generate a separate table for
each group.  To see what I mean, take another look at the GIF above.  If that
code was all in one table, as the purple column got longer, the cyan column
length would get longer with it!  This would push the innermost block (`if
(isPrime(i))` etc.) way further to the right than it needs to be.

To counteract this problem, we group the code based on the number of tabs in
the line -- consecutive lines containing the same number of tabs will be
grouped together.  We begin by defining a utility function to tell us the
number of tabs in a line:

> countNumTabs :: String → Int
> countNumTabs = length ∘ filter (== '\t')

We can consider a "group" to be a simple tuple containing the number of tabs
in the lines in that group, and a list of the lines themselves:

> type CodeGroup = (Int, [String])

Our `group` function, then, is a simple fold over the lines in the code block,
returning a list of these *`CodeGroup`*s.

> group :: String → [CodeGroup]
> group s = foldr groupMaker [] $ lines s

Given the definition of `foldr`, it is clear what the type of our `groupMaker`
function needs to be.  Because `foldr` associates to the right, you can
consider it as if it's working from the bottom of the code block upward.
It'll take the current line being processed, and the *`CodeGroups`* that have
been identified so far, and return a new set of *`CodeGroups`* with the new
line added appropriately.

What does "added appropriately" mean in this case?  Well, there are three
possibilities:

- On the first line, no *`CodeGroups`* will have yet been identified, so we
  always want to create a new *`CodeGroup`* with the number of tabs set to
  however many tabs are in this line.
- From that point on, we compare the number of tabs in the line with the
  number of tabs in the most recently identified *`CodeGroup`* (that directly
  *beneath* the current line -- remember we are working bottom-up).
    - If they are the same, we simply add this line to that *`CodeGroup`*.
    - Otherwise, we generate a new *`CodeGroup`* with the number of tabs set
      to that of the current line.

This can be represented in Haskell thus:

> groupMaker :: String → [CodeGroup] → [CodeGroup]
> groupMaker l []           	            	= [(countNumTabs l, [l])]
> groupMaker l (g@(n, ls):gs) 	| lts == n  	= (n, l:ls):gs
>                           	| otherwise 	= (lts, [l]):g:gs
>   where lts = countNumTabs l

There's quite a lot of list decomposition and restructuring going on in the
above definition, but hopefully it should be clear enough what's going on.

Generating the tables themselves
--------------------------------

Now we have all we need to split the code into groups, we can use those to
construct the tables themselves.  Before we start, we'll set up a couple of
utilities which will help set up the table.

Firstly, we want all columns to be left-aligned.  We can do this by generating
a list of *`AlignLeft`* of length one greater than the number of tabs.  The tab
character is the delimiter, so the number of columns will always be one
greater than this.

> allLeft :: Int → [Alignment]
> allLeft n = replicate (n+1) AlignLeft

Similarly, we need to specify the widths of the columns.  We don't need to be
precise about this, and in fact it would be very complicated to try and work
them out, and would require hard-coding the choice of typeface in here, which
would be unfortunate.  But we do need to specify *something*, otherwise the
layout won't be as predictable as we need it to be.

The trick is that we always want the right-most column to fill any excess
space.  This will force other columns to be as narrow as they can be, while
still fitting the contained code.  We can represent this as follows:

> columnWidths :: Int → [Double]
> columnWidths n = replicate (n) 0 ++ [1]

Where a value of `0` means 0% and `1` means 100%.

Finally, we define `removeClass`, a utility to remove a certain class from a
*`Block`*.  Pandoc defines the `literate` class on Literate Haskell code
blocks, which puts the leading `>` at the beginning of lines -- obviously we
only want this in the first column of the table, so we need to remove
`literate` from the attributes of subsequent columns.

> removeClass :: String → Attr → Attr
> removeClass c (i, cs, kvs) = (i, delete c cs, kvs)

We are now ready to generate the actual tables.  We begin by splitting the
code into groups and making a table for each of those groups.

> codeTables :: Attr → String → [Block]
> codeTables a = map (makeTable a) ∘ group

"Making a table" is itself a question of making a row for each line of code in
the group, and wrapping that up in a Pandoc *`Table`* construct.

> makeTable :: Attr → CodeGroup → Block
> makeTable a (n, g) 	= constructTable $ map makeRow g where
>   constructTable   	= Table [] (allLeft n) (columnWidths n) []

To make a row, then, we split the line up based on the tab delimiter, then
wrap it in a *`CodeBlock`* constructor.  *`CodeBlock`* expects a set of
attributes, so we pass the attributes of the original *`CodeBlock`* unchanged
in the first column, and then pass a version with the `literate` class removed
for all other columns. Finally we wrap the whole thing in a list.

>   makeRow 	= map (:[]) ∘ zipWith ($) codeRow ∘ splitOn "\t"
>   codeRow 	= map CodeBlock $ a:removeLiterates (repeat a)
>   removeLiterates = map $ removeClass "literate"

And that's it!  Beautifully aligned code, using a proportional font.

Epilogue
--------

There are a couple of issues with using elastic tabstops on this website.

Firstly, while Nick Gravgaard has written plugins for a number of editors,
neither of the editors I use regularly (vim and emacs) are supported.  This is
for the very good reason that they *can't* be -- both of them use characters
as their fundamental building block in terms of layout, so you can't modify
the layout by an arbitrary number of pixels.

As a result, I use a monospaced font when editing the posts and insert the
tabs as I think appropriate.  To see how that will actually affect the layout,
I have to open the page in the browser, which is somewhat inconvenient.  As
well as that, I have apparently-superfluous tabs all over my file!

I have found that I get pretty good results by setting the tabstop length to
1, and making tabs visible.  That way I can use spaces to align as usual, but
use a tab as the last "space".  This way it will look good in my editor but
also on the site, and I can also see what's going on thanks to the visible
tabs.

Another issue is the way post previews come up in RSS feeds.  The reader that
I use, feedly, renders the tables representing my code quite badly -- I end up
with single-character columns with all the code written vertically!  I think
the solution to this is not to do the elastic tabstops transformation when
generating the RSS feed, but I haven't got around to this yet.

All in all, though, I'm pleased with the way it looks -- and it's a testament
both to Haskell and to Pandoc's design that it was so easy to add as a
post-process to my site.  There's something lovely about the fact that the
entry point to this entire blog post is a pure function with type *`Pandoc`* `→`
*`Pandoc`*.  No missiles being launched here!

[generating-this-website]: http://www.dpwright.com/tags/generating%20this%20website
[elastic-tabstops]: http://nickgravgaard.com/elastictabstops/
[cm]: http://en.wikipedia.org/wiki/Computer_Modern
[cr]: http://en.wikipedia.org/wiki/Concrete_Roman
[cm-web-fonts]: http://checkmyworking.com/cm-web-fonts/
