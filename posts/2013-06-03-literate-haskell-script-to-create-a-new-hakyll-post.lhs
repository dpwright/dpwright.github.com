---
date: 2013-06-03 08:30
tags: literate-programs, functional-programming, haskell, hakyll
title: (Literate) Haskell script to create a new Hakyll post
---

This is my first post after quite a long absence.  A lot has happened:

* I got married.
* I've spent a lot of time with functional programming languages, particularly
  [Clojure](http://clojure.org), which I used to create the website we used to
  manage the aforementioned wedding.
* I've switched this blog from [Jekyll](http://jekyllrb.com) to
  [Hakyll](http://jaspervdj.be/hakyll) (And thus its configuration from Ruby to
  Haskell).

It is about this last point that I intend to write now.  I will spare you the
usual introduction about what Haskell is, as there are plenty of resources from
which you could glean that.  I will talk a little bit about why I decided to
make the switch though, before walking through a little helper utility which I
hope will come in handy.

From Jekyll to Hakyll
---------------------

Although I had enjoyed setting up and using Jekyll, I was starting to get a
little fed up of it -- through no fault of its own, really.  In the interests of
getting up and running quickly, I used [Octopress](http://octopress.org) to get
started, downloaded a [nice, minimal theme](https://github.com/kui/k-ui-octopress-theme)
by [k-ui](http://k-ui.jp) (from which my current theme still draws influence),
and started tweaking it to fit my needs.

The problem was, I had downloaded this big system that did everything for me,
and I soon came across problems I didn't know how to solve.  Probably if I'd
taken more time with Jekyll I could have figured it out, but I ended up
neglecting the site for quite a while to work on the wedding site, and by the
time I got back to it I was looking for something new.

I considered writing my own, probably in Clojure, but then I came across Hakyll
and thought it would give me the excuse I've been looking for to give Haskell a
try.  It offers less "out of the box" than Octopress did (not sure about
Jekyll), so I have to set up quite a lot of the basic stuff myself, which means
that I come out understanding it much better.  Well, that's the idea, anyway.

One of the things I did like about Octopress, though, was that it came with some
useful shortcuts.  I don't really want to have to look up the date and write it
out every time I write a new post; I'd rather just say "new post", give it a
title, and start writing.  As far as I can tell, Hakyll doesn't provide this
functionality -- nor should it, necessarily; its job is to compile my site.

So I thought this would be an opportunity to have a go at writing a simple
utility in Haskell that isn't just a glorified configuration file.  Really this
sort of thing would usually be the job of a shell script, so it may be somewhat
outside of Haskell's usual problem domain, but I figured I'd give it a go.

Literate Haskell
----------------

This is one feature of Haskell that is pretty interesting: there is language
support for writing "literate" Haskell files; that is, files which read like
documentation with the occasional code snippet, but those code snippets can be
compiled.

This way of writing code has become fairly popular recently, with Literate
Python and Literate Coffeescript among others, but Haskell supports it natively.
The idea has some heritage; Miranda, an earlier functional language from which
Haskell draws a lot of ideas, supports it, and the idea was first implemented by
Donald Knuth in the form of WEB/CWEB.

Intrigued by the idea, I decided to write the "new post" script in a literate
style, and the result is this blog post!  You can copy and paste this page
directly (either from the html or the markdown source), and the compiler will
ignore the blog post and compile the included code.  Lines beginning with "bird
markers" (> symbols at the start of lines, like quoted messages in an email) are
interpreted by the compiler as part of the source code; anything else is
ignored.

This will seem like a pointless reiteration for anyone familiar with the
concept, but I'm just going to state it as a paragraph on its own for anyone not
paying attention who hasn't quite twigged how cool this is:

Having written this blog post as a literate Haskell script, whenever I want to
write a new blog post, I literally *run this blog post*.

The script
----------

Haskell's literate programming system doesn't allow code to be written
out-of-order, so I will go through this step-by-step.  Bear in mind that this is
my first piece of "proper" Haskell code; it might be really bad!  If you are a
beginner like me you should probably try to find some more informed sources
before you copy any of this stuff.  If you know what you're doing and you spot
any silly mistakes / poor style, please let me know, or even submit a pull
request on github and show me what's wrong with it!  I am writing this post to
learn, not to teach.

With that caveat out the way, first I'll import the necessary modules.

> import System.Environment
> import Data.Char
> import Data.List
> import System.Locale
> import Data.Time.Format
> import Data.Time.LocalTime
> import Text.Printf
> import System.Cmd
> import System.Exit

We need `System.Environment` for getArgs, which returns us a list of
command-line arguments.  The `Data.Char` and `Data.List` libraries will come in
handy when we want to manipulate strings and lists, as we will need to do to
generate titles appropriately.  The `System.Locale`, `Data.Time.Format` and
`Data.Time.LocalTime` modules are required for dealing with dates and times.
Finally I use `Text.Printf` to generate the command I'm going to run to write
the actual post, and `System.Cmd` and `System.Exit` to run it.

The interface I'm aiming for is simple: simply type the name of the script,
followed by the title of the blog post.  The one slightly tricky thing is that
I'd like to be able to write the title either quoted or unquoted, so the
following to examples should be the same:

``` bash
$ new-post An example blog post
$ new-post "An example blog post"
```

In order to do this, we're going to "normalise" the arguments, so that whatever
the input we're working with a list of words, broken up by spaces.  Haskell
provides a couple of helper functions for this: `words` takes a string and
splits it up by spaces (so that you get a list of words out, hence the name),
and `unwords` does the opposite -- it joins a list of strings together, putting
spaces between each one.  To perform the normalisation, we can simply run
`words` followed by `unwords`:

> normaliseArgs = words . unwords

The way this function works is quite clever.  Mathematically speaking, functions
can be [composed](http://en.wikipedia.org/wiki/Function_composition) to create
new functions which are equivalent to running the second function on the output
of the first.  Haskell supports this property directly using the `.` syntax.
Here we define `normaliseArgs` as the *composition* of `words` and `unwords`.
Interestingly, doing it this way we don't have to worry about the inputs and
outputs of the function: we simply declaratively state the relationship between
the functions.

It is generally seen as good form to explicitly state the input and output types
for top-level function definitions, however I have chosen not to here, because
it feels natural that they should match those of `words` and `unwords`.
Experienced Haskellers will be able to tell me if I'm in the wrong.

Having explained the `normaliseArgs` function, we can look at the implementation
of the script itself.  The `main` function gives an overview of how the script
will work:

> main :: IO ()
> main = do
>   args <- getArgs
>   time <- getZonedTime
>   let normalisedArgs = normaliseArgs args
>   let header         = makeHeader normalisedArgs time
>   let filename       = makeFilename normalisedArgs time ".md"
>   launchVim filename header
>   -- or, if you don't use vim: launchEditor filename header
>   return ()

All in all, it's a very basic script.  We read the arguments and the time from
the `IO` monad, generate our header and filename, and then launch our editor of
choice with the header text already prepared.

That `return` at the end is a bit of a *faux amis*.  Our `main` function is
defined as returning `IO ()`, but the command we use to launch our editor will
return an `ExitCode`.  `return` simply lets us set the return value for the `do`
block -- it does *not* exit early as one might expect.

Let's start by generating the filename.  The filename will be the date, followed
by the title (all in lower-case and with hyphens replacing spaces), and finally
the file extension.  These files all belong in the `posts/` directory.

In most languages, I would start by creating local variables containing the
formatted date and title and work from there, but here I'm going to take
advantage of Haskell's `where` form to write in a more declarative style.  We
start by declaring the type information:

> makeFilename :: [String] -> ZonedTime -> String -> String

So `makeFilename` is a function which takes in an array of strings, the current
time, and a file extension, and returns a string -- the filename.  Let's assume
for the moment that we've already dealt with the nitty gritty of actually
generating the string we want to use for our filename.  The thing we'd want to
return in that case is a concatenation of that filename base with the folder
it's meant to be in, `posts`, and the file extension.  Something like this:

> makeFilename args t ext = concat ["posts/", filename_base, ext]

Seems simple enough, but what is that `filename_base`?  Well, let's define that
right now, using the `where` form:

>   where
>     filename_base = intercalate "-" [date, title]
>     date          = formatTime defaultTimeLocale "%Y-%m-%d" t
>     title         = map toLower joinedArgs
>     joinedArgs    = intercalate "-" args

`filename_base` is the string formed by putting "-" between `date` and `title`,
where `date` is the date as formatted by `formatTime`, and `title` is the
arguments joined together and made lower-case.

You'll notice that these variables are declared in pretty much the opposite
order to that which you'd use in most languages.  In fact, the order doesn't
matter: I could shuffle those lines around and it would still work.  In a sense
it's less that you're "putting a value into a variable" and more that you're
defining what that variable *is*, in terms of other variables which may or may
not have been defined yet.

Writing it this way feels strange at first, but it is closer to the way
equations are usually expressed in mathematics so in that sense it feels
natural.  The first paragraph following the code reads like a description of the
variables, whereas the imperative equivalent (first call `formatTime` and put
the result in `date`, then lower-case the title...) reads like a series of
instructions.

Haskell also provides the `let` form which allows you to define variables before
you use them.  Deciding when it would be appropriate to use `where` and when to
use `let` is something that I hope will become easier with experience; for now I
just use what I feel like (which generally means I tend to favour `where` since
it's new and shiny!)

Blog posts have a standard header which lists the date and time they were
created, the tags associated with that post, and the title.  Since both the
date and the title are formatted differently in the header to the filename I
don't feel the need to move the generation of those strings into their own
function; instead we'll just create them locally inside the `makeHeader`
function as we did for `makeFilename`:

> makeHeader :: [String] -> ZonedTime -> String
> makeHeader args t = printf headerFormat date title
>   where
>     headerFormat  = "---\ndate: %s\ntags: \ntitle: %s\n---\n\n"
>     title         = unwords args
>     date          = formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" t

I'm using `printf` to do my string formatting largely because I'm familiar with
it from C.  There are other modules, such as `Text.Format`, which may be more
appropriate, but for now I'm happy using this and it seems to do the job.

`unwords` makes a fresh appearance here to join the arguments back up and form
the title.  It is perhaps wasteful to do this again after having done it as part
of `normaliseArgs`, but not having to keep hold of various forms of the
arguments and the title does keep the code quite simple and easy to read.

Lastly, we need to launch our editor!  You may have noticed in the `main`
function defined above, I had separate `launchEditor` and `launchVim` commands.
We'll start with the general `launchEditor` function, which will launch whatever
is defined in `$EDITOR`:

> launchEditor :: String -> String -> IO ExitCode
> launchEditor filename header = do
>   writeFile filename header
>   system $ printf "$EDITOR \"%s\"" filename

This is fine, and works, but it has two issues which bother me.  Firstly, it
generates the file and then opens it -- meaning that if I change my mind and
decide to quit the editor without saving, the file is left over.  Secondly, the
cursor begins at the start of the file, rather than where I'd like to start
editing.

As it happens, I can fix both of this problems with my editor of choice, vim.
I'm not really sure how I could do this in a general way, since all editors use
a different set of command-line parameters, so I decided to leave the generic
`launchEditor` as is above and write a new, vim-specific one to get the
functionality I want.  The result is as follows:

> launchVim :: String -> String -> IO ExitCode
> launchVim filename header = do
>   system $ printf vimCmdFormat header filename
>   where
>     vimCmdFormat = "vim '+let @c=\"%s\"' '+put! c' '+normal 3G$' \"%s\""

The haskell is pretty much the same; I've just changed the format string I pass
to `printf`, and removed the call to `writeFile` since I'll be sending my
content directly into vim now.  The way I do this is to load the content into
one of vim's registers (`+let @c=...`), then, having launched vim, pasting the
contents of that register back out into the file (`+put! c`).  Finally I
position the cursor using normal-mode keybindings, and then pass the filename.

Caveats and Issues
------------------

As I mentioned, this is my first Haskell program, so I am sure there will be a
number of problems with it.  If you spot anything particularly bad, please 
[contact me](mailto:dani+blog@dpwright.com) and let me know!  You can even
suggest modifications to the article by editing it directly [here][blog-source]
and sending them as a pull request.  Any feedback is greatly appreciated!

The biggest thing that sticks out to me is my frequent use of `printf`,
particularly when it comes to generating commands to pass to `system`.  This
feels a little like overkill, and I'm sure there must be a better way.

Another problem is that punctuation in blog titles won't be handled well.  The
filename generator simply makes everything lower-case and adds hyphens; it
should perhaps strip out any characters which are not URL-friendly, too.  The
call to vim is worse -- since it passes the header text as a command-line
parameter, it must keep the title as-is, but will break if the title contains
quotation marks.  The only solution I can think of for this is to write the
header text to a temporary file and then read it in from vim, but I'll leave
that as an exercise for the reader and just avoid using quotation marks in my
blog posts for now ;-)

In Summary
----------

![Yo dawg I heard you like scripts and blogs so I put a script to make blogs
inside a blog about the script to make blogs so you can blog about scripts while
you script about blogs](http://cdn.memegenerator.net/instances/400x/38355687.jpg)\ 

[blog-source]: https://github.com/dpwright/dpwright.github.com/edit/source/posts/2013-06-03-literate-haskell-script-to-create-a-new-hakyll-post.lhs
