---
date: 2013-08-21 08:51:58
tags: literate-programs, haskell, functional-programming
xp: vitei
xpid: 147
xplastupdate: 2013-08-22 10:41
title: Writing a TCP server in Haskell using proxies and pipes
---

<center>![Haskell Pipes](http://blog.vitei.com/wp-content/uploads/2013/08/haskell-pipes2.jpg)</center>

<div class="sidenote">Since the release of [Pipes 4][pipes-4], this article is now
rather out-of-date.  I leave it up here for posterity, but if you're new to
Pipes you should probably just ignore it and find a more up-to-date
tutorial.</div>

Since my last post, I've really been enjoying Haskell, and in fact a lot of the
functional ideas I had formerly been playing with in Clojure I am now exploring
in Haskell instead.  I will go into more detail on the reasons for that in a
future post -- for now I just wanted to share a neat bit of code I wrote the
other day that I think really demonstrates the kind of concise, readable code
Haskell enables.

<div></div><!--more-->

Problem description
-------------------

This is a really simple, common requirement: the application requires a TCP
server to take requests and perform whatever it is the application does, after
which it will respond with some sort of output.  Everyone's had to do it at some
point, and there's a range of ways to implement it -- from the hack-it-in-quick
interface to some simple utility to the robustness of, say, a web server.

My requirements were fairly rudimentary, and so this implementation is a pretty
simple one.  Still, maybe it'll be a useful starting-point for anyone who wants
to get a server up and running quickly.

The input to the server is a series of single-line commands separated by
Windows-style `\r\n` newlines.  In response to each of these commands, the
server will respond with potentially multiple lines, completed by the special
terminator string: `\r\nEND\r\n\r\n`.

Setting up
----------

This server is built around the `pipes` package, a very nicely-designed
framework which allow you to stream data through some sort of pipeline.  The
`pipes-network` package, originally written by Paolo Capriotti and now
maintained by Renzo Carbonara, provides some useful `Producer` and `Pipe`
functions which read from and write to a TCP socket respectively, so a lot of
the work has already been done for us.  Before we start, we will need to install
the `pipes-network` package:

```bash
$ cabal install pipes-network
```

`pipes-network` delivers and expects to receive its data in `ByteString` form,
rather than a plain `String`.  I wanted to work with `Text` in my own code.
This mix of three different string types can get quite confusing, and I really
don't want to clutter up my code with lots of conversions of literal strings,
so let's make use of the `OverloadedStrings` LANGUAGE pragma to perform those
conversions for us:

> {-# LANGUAGE OverloadedStrings #-}

This won't help us when it comes to converting the actual strings we're working
with, but at least it'll make literal strings in the code look a bit tidier.

Finally, let's import the modules we need:

> import Network
>
> import Control.Proxy
> import Control.Proxy.TCP
>
> import Control.Monad
> import Data.List
> import Data.Maybe
> 
> import qualified Data.Text as T
> import Data.Text.Encoding
> import Data.Text.Encoding.Error

These should mostly be fairly self-explanatory.  We are dealing with network
code, so the `Network` module provides some types (such as `Socket`) which are
going to be useful to us.  `Control.Proxy` and `Control.Proxy.TCP` are provided
by the aforementioned `pipes` and `pipes-network` modules respectively.
`Control.Monad` and `Data.List` provide various useful utilities.  Finally, the
last three imports handle our various string formats.  For my purposes, I am
assuming all interaction with the server will be in UTF-8, and I make use of
`Data.Text.Encoding` to convert to and from `ByteString`.

Structure and Types
-------------------

OK, so now we're set up, how is the server going to work, and how is the rest of
our program going to interface with it?  I wanted the server to be as simple as
possible, and really I just wanted to set it running and then just forget about
it, and let the rest of my program handle commands as they come in.  I decided I
would accept two functions from my program, one to interpret incoming text
(guaranteed to be a single line) and turn it into some command format the
program understood, and another to read in these commands and perform some
action, returning any output from running that action.

The interpreter is a simple, pure function, which simply maps `Text` to some
command type `c` (defined by the program):

> type Interpreter c = T.Text -> c

The handler is slightly more involved.  Firstly, I didn't want it to have to
worry about the terminator string, so I would have the server add those itself.
This posed a problem though: since the output could potentially be multiple
lines, and the action could take a long time to complete, I wanted to stream it
out to the socket as it came in, not all in one chunk at the end.  But in order
to do this, I would have to know whether or not to add the terminator string for
a particular piece of output, as it should only be added at the very end.  My
handler, then, would need to output some information about whether or not it was
complete, along with whatever text it needed to deliver so far.  This can be
accomplished with the following type:

> data Output t = Still t
>               | Done t
>               deriving (Show)

Onto the handler itself then!  This is the first instance of one of the types
defined by the `pipes` library, a `Pipe`.  A `Pipe` is a generalization of the
`Proxy` typeclass defined by the library, which defines a set of types which can
send and/or receive data up or downstream.  The library defines various
instances of these.  They are explained in excellent detail in the
[Control.Proxy Tutorial][proxy-types], but in summary:

- A `Producer` generates values to be streamed *downstream*
- A `Consumer` reads values from *upstream*
- A `Pipe` reads values *upstream* and then sends values *downstream* (think of
  it like a UNIX pipe, where "upstream" is equivalent to reading from `STDIN`
  and "downstream" is equivalent to writing to `STDOUT`)
- A `Client` can send and receive values *upstream*
- A `Server` can send and receive values *downstream*
- A `Proxy` can both send and receive values in either direction
- Finally, a `Session` is formed by composing `Producers`, `Pipes`, and
  `Consumers`, or by composing `Clients`, `Proxies`, and `Servers`.  A `Session`
  is a closed system; it has no upstream or downstream interface.

Our command handler, then, is expected to read in commands from the interpreter
*upstream* and send its output to the socket *downstream* -- look at the list
above again and it should be quite obvious that it is a `Pipe`!  Thus, our
`Handler` definition:

> type Handler c = () -> Pipe ProxyFast c (Output T.Text) IO ()

As we can see, `Handler` is a function which returns a `Pipe` from some command
type `c` (as output by the `Interpreter`), to a `Text` value wrapped in our
`Output` type.  It exists in the `IO` monad as it will need to perform an
action.  Finally, it receives and outputs `()` -- these values are used for
requests from downstream and requests sent upstream, neither of which are
supported by `Pipe`.

There's one annoyance here which I haven't managed to solve -- I am specifying
the proxy `ProxyFast` rather than just accepting any type of class `Proxy`.  I
tried doing the latter, but I couldn't get it to typecheck correctly -- I think
because my use of `runProxy` later forces the server to run under `ProxyFast`,
so if a user tried to pass in a handler of type `ProxyCorrect`, it wouldn't be
compatible.  This seems quite reasonable, but it is nevertheless annoying that
this information about which proxy implementation I am using must leak out of
the module.  The solution is probably to have my handler not implemented as a
proxy, but instead provide it with some interface to which it can send
incremental output (perhaps a `TChan`).  This would mean the client program need
not deal with proxies or the `pipes` library at all!  It complicates the code
somewhat, though, so for now I will use the above interface.

Main Server Backend
-------------------

Now our types are in place, we're almost there!  Let's make a simple function to
start our server up and set it running first, which I'll call `run`:

> run :: Interpreter c -> Handler c -> String -> IO ()
> run interpreter handler port = serve HostAny port go
>   where go (socket,_) = runProxy $ server interpreter handler socket

`run` is our interface to the rest of the program -- we start the server up with
this function, passing in our interpreter and handler functions, as well as the
port we want to run on.  It uses `pipes-network` to start up the server and then
run our `Proxy` for each incoming connection, passing the functions through as
well as the socket to communicate on.

`runProxy` expects a function taking `()` and returning the proxy we want to
run.  In addition to this, we want to feed the `Interpreter`, `Handler`, and
active `Socket` to the proxy to work with.  The type signature for our `server`
function, therefore, must be:

> server :: Interpreter c -> Handler c -> Socket -> () -> Session ProxyFast IO ()

One thing I love about Haskell is how easy it is to work top-down; you can start
with a rough outline of what you want to get done and then fill in the blanks
later.  Here is the implementation of `server`, which can be expressed as a
composition of proxies representing each part of the process:

> server i h s = readSocket >-> interpret >-> handle >-> writeSocket

This is the sort of expressive power the `pipes` library gives us -- it almost
reads like an ASCII diagram describing the required server behaviour!  It only
remains for us to "fill in the blanks" by defining each of these functions.  We
can do this in a `where` clause, and for the most part, each part of the session
is itself a composition of simple proxies.  I'm going to work from the outside
in, beginning with the administrivia of reading data in from the socket and
writing results back out to it, and then dealing with the actual interpretation
and handling of data at the end.  We'll begin with reading in data from the
socket:

>   where
>     readSocket = socketReadS 4096 s >-> decode >-> split "\r\n"
>     decode = mapD $ decodeUtf8With lenientDecode

`readSocket` reads data in from the socket using a `Producer` defined by the
`pipes-network` library.  The number `4096` is just that recommended for general
purposes by the library -- it might need tweaking depending on your needs.
`decode` reads in raw `ByteStrings` and converts them to UTF-8 encoded `Text`
data.  `mapD` is a utility function provided by `pipes` which will take a pure
function and generate a `Pipe` which pipes everything it receives from upstream
through the function and sends the results downstream.  Finally `split` is a
`Pipe` which buffers up text it receives from upstream until it reaches a
certain delimiter, at which point it sends the text up to and including the
delimiter downstream -- its definition appears later.

Writing the data back out is pretty similar -- we receive UTF-8 `Text` and we
want to encode it as a `ByteString` before sending it out through the socket.
This time, though, we don't care about buffering -- we can just send text out as
it comes in -- so we can omit the call to `split`:

>     writeSocket = mapD encodeUtf8 >-> socketWriteD s >-> done

The reason for the `done` at the end there is that it turns out that
`socketWriteD` pipes any data it writes to the socket downstream, to ease
composition.  In order to close off our session, we need a `Consumer` which will
sit there forever, taking data from upstream and ignoring it.  Strictly speaking
this isn't necessary, but it allows us to treat our server as a `Session` rather
than a `Producer`.  Its definition is as follows:

>     done () = forever $ request () >> return ()

We should now have UTF-8 encoded `Text` data being pumped out, line-by-line, to
the interpreter, and we are expecting to get `Text` data back out from the
`handle` function to send on down the pipe.  Time to call back out to our
application's interpreter and handler.

Remember we defined the interpreter as a pure function taking in `Text` and
spitting out `Commands`?  `interpret`, then, merely needs to convert this pure
function into a `Pipe`, using `mapD` again:

>     interpret = mapD i

The handler has the small extra complication of needing to append our terminator
string whenever the application tells us it is `Done`.  We can do this by
composing the handler function (which is already a pipe) with a small utility
function:

>     terminator = "\r\nEND\r\n\r\n"
>     handle = h >-> mapD handleOutput
>     handleOutput (Still t) = t
>     handleOutput (Done t) = t `T.append` terminator

That's it!  That's basically all we need to define a simple, streams-based
server.  Now all our application needs to do is define an `Interpreter` and a
`Handler`, and we're on our merry way!  Well... almost.  We still haven't
defined the `split` function.

Buffering Text
--------------

Our `readSocket` proxy runs its output through a proxy called `split` at the
end, in order to buffer up the text it receives and send it out broken apart by
newlines.  I was surprised not to find something like this defined in
`pipes-network` already -- perhaps it is there and I missed it.  At any rate, I
wrote a definition of it here -- the only `Pipe` in the server code which is not
simply a composition of other proxies.  I am pretty certain there must be a
better way to do this, and it's probably a DDOS risk (you could stream it a huge
line without any linefeeds and it would keep buffering until it ran out of
memory), but it served my purposes.  Comments on better ways to achieve this
would be much appreciated!

`split` takes the delimiter to split on and returns a `Pipe` function, which
runs a loop passing in the current state of the buffer as its parameter.  It
starts, of course, with the empty buffer:

> split :: Proxy p => T.Text -> () -> Pipe p T.Text T.Text IO ()
> split d () = runIdentityP $ loop T.empty

`runIdentityP` there is provided by `pipes` and helps Haskell infer the types
better.

The loop itself begins by requesting the latest data from upstream, and
appending it to our existing buffer:

>   where
>     loop r = do
>       a <- request ()
>       let a' = r `T.append` a

We then check whether the delimiter can be found in the newly received text, and
if it can we split the entire buffer on the delimiter, send the first part (up
to the first delimiter) downstream, and pass the remainder back to the next
iteration of the loop.  If the delimiter can't be found, we just loop again with
the updated contents of the buffer:

>       if d `T.isInfixOf` a
>         then do
>           let l = T.splitOn d a'
>           forM_ (init l) respond
>           loop $ last l
>         else loop a'

This implementation is OK and it does the job, but I am not totally happy with
it.  Apart from the obvious issue that it could buffer the text forever, it just
doesn't "look nice".  My experience with functional programming so far has
taught me that if things don't look nice, they probably aren't -- there is
usually a more elegant, functional way to do them.  I may revisit it in future.

Interacting with our server
---------------------------

In a couple of compact functions, we have a fully-working TCP server!  Let's
write the application-side code to make use of it.  Since this is a single blog
post, we'll just write the application code inline -- of course usually you'd
put the server stuff in a module and import it into your application.  Consider
this the cut-off point -- everything before this paragraph belongs in a module;
everything after it is application code and can go in your `Main` module or
anywhere else.

Our application will need to work with some sort of command type -- this defines
all the actions that our handler knows how to take.  Let's keep it simple and
support just three commands: "add", "echo" and "quit".  If the server receives
anything else it should respond with "Unrecognised command".

"add" will take as its input a list of integers and return the result of adding
them together as its output.  "echo" will simply echo the text back out.  "quit"
will disconnect the client.  These commands can be defined as follows:

> data Command = Add [Integer]
>              | Echo T.Text
>              | Quit
>              | Unknown T.Text

Firstly we'll write our interpreter.  Of course there's all sorts of things you
could do here -- parsing text is one of Haskell's great strengths!  But all I
really want is to put the command, followed by a space, followed by its
parameters, separated by spaces.  A proper interpreter would need to handle
syntax/type errors and the like, but for the sake of simplicity I will assume
only valid input can be received:

> interpreter :: T.Text -> Command
> interpreter = interpret . T.words
>   where
>     interpret ("add":xs) = Add $ map (safeRead 0) xs
>     interpret ("echo":text) = Echo $ T.unwords text
>     interpret ("quit":[]) = Quit
>     interpret other = Unknown $ T.unwords other

The inclusion of `[]` at the end of the deconstruction of "quit" means
that strings such as "quit with extra text at the end" will return "Unrecognised
command" rather than quitting.

`safeRead` is our nod toward handling unexpected input.  Usually, if `read`
receives a string that can't be parsed as the type we're looking for (`Integer`
in our case), it will throw an exception.  What we're going to do here is just
ignore it by returning the default value of `0`.  Its implementation is as
follows:

>     safeRead defval x = maybe defval id (readMaybe x)
>     readMaybe = fmap fst . listToMaybe . reads . T.unpack

Note that `readMaybe` is actually included as part of `Text.Read` in GHC
versions 7.6 and above, so its definition may not be required.  I include it
here to support older versions of the compiler.

Next up is our command handler.  In order to have access to the `respond`
function to pipe output downstream, we have to implement this as a `Pipe`.  For
those commands which simply take a simple input and return a simple response,
though, it would be nicer to define them as a simpler mapping of the form:

> command :: Monad m => Command -> m T.Text

In fact, neither of our currently-defined commands require access to any monad,
but we will define `command` in those terms since in the real world it's very
likely that we *will* want access to `IO` or `STM` or something similar in order
to communicate requests to other parts of the application.

Let's define our currently supported commands in terms of this simpler
interface:

> command (Echo t) = return t
> command (Add xs) = return $ (T.pack . show) $ foldl' (+) 0 xs
> command (Unknown cmd) = return $ "Unrecognised command: " `T.append` cmd

Our handler itself, then, will be a dispatcher, which can send commands to the
appropriate handler if one exists, or pass them off to this simple handler
otherwise:

> commandDispatcher :: Proxy p => () -> Pipe p Command (Output T.Text) IO ()
> commandDispatcher () = runIdentityP loop where
>   loop = do
>     cmd <- request ()
>     case cmd of
>       Quit -> respond (Done "GOODBYE") >> return ()
>       _    -> executeSimple cmd >> loop
>   executeSimple cmd = (lift . command) cmd >>= respond . Done

This function loops until it receives the `Quit` command, at which point it
breaks out of the loop which causes the entire `Session` to be dismantled,
disconnecting the client.

Finally, we add a `main` function to set it all in motion on port "8000".  The
call to `withSocketsDo` is required on Windows, and is a no-op on other systems,
so it's good form to use it whenever we're dealing with network code:

> main :: IO ()
> main = withSocketsDo $ run interpreter commandDispatcher "8000"

If you copy and paste this page into a file and run it with `runhaskell` you
should be able to open another window and telnet into `localhost` on port 8000.
Try typing some commands and see what happens!

[pipes-4]: http://hackage.haskell.org/package/pipes-4.0.0
[proxy-types]: http://hackage.haskell.org/packages/archive/pipes/3.3.0/doc/html/Control-Proxy-Tutorial.html#g:3
