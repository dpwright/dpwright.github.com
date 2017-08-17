---
date: 2015-07-17 08:41:23
tags: haskell, assembly, z80, spectrum, literate-programs
title: Writing a ZX Spectrum game in Haskell
subtitle: introducing the z80 and zxspectrum packages
---

Haskell, [the world's finest imperative programming
language][haskell-imperative], can now be used to write games for the ZX
Spectrum, the world's finest 80s microcomputer.  This post introduces the two
packages that make this possible:

- [z80], a fully-functional Z80 macro-assembler embedded in Haskell, and
- [zxspectrum], a set of utilities and macros to make working with the ZX
  Spectrum specifically easier, including labels for important routines in the
  Spectrum 48k ROM.

These packages wouldn't have been possible without Russell O'Connor's excellent
article in [The Monad.Reader Issue 6][monad-6], "Assembly: Circular Programming
with Recursive do", which was the main inspiration both for those packages and
for this post.

Here's the game we're going to be making.  It's basically a port of the project
pursued over the course of [this great set of ZX Spectrum
tutorials][speccy-tuts], which itself is a clone of the game centipede.  You
can play it directly on this webpage, or you can download the `.tap` file for
playing in an emulator [here][tapfile].

<script src="/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/jdataview.js"></script>
<script src="/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/jsspeccy-core.min.js"></script>
<script>
  function go() {
    var scaleFactor = 1.5;
    var startResizingWidth = 700;
    var resizeFactor = startResizingWidth * (1.0 / scaleFactor);
    var windowWidth = window.innerWidth
    if(windowWidth < startResizingWidth) {
      scaleFactor = windowWidth / resizeFactor;
    }
    var jsspeccy = JSSpeccy('speccy', {
      'autostart': false,
      'autoload': true,
      'scaleFactor': scaleFactor,
      'loadFile': '/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/lambdaman.tap'
    });
  }
</script>

<center><figure>
<div id="speccy"><img src onerror="go()" /></div>
<figcaption>Movement: QAOP/HJKL Fire: Space<br />
Powered by [jsspeccy]</figcaption>
</figure></center>

I made some of my own customisations along the way, but credit for most of the
actual assembly code must go to those tutorials, which were an excellent
resource for me while I was testing these packages too.  Thanks!

OK, without further ado, let's get started!  This is a literate Haskell post,
so when it is executed the lines prefixed with `>` are compiled and run, and
the .tap file which is embedded above is generated.

Preliminaries
-------------

The initial pragmas and imports to most `zxspectrum` projects in Haskell will look
similar to this.  Firstly, we need the following language extensions:

> {-# LANGUAGE RecursiveDo #-}
> {-# LANGUAGE OverloadedStrings #-}

`OverloadedStrings`, as always, is just a convenience -- but a very useful one as
any strings you want to print to the ZX Spectrum screen are bytestrings, so it's
nice just to be able to input them as strings.  `RecursiveDo` allows us to define
labels after they are used, i.e. to jump to code which comes later in the file --
a common idiom in assembly programming.

The following language pragma and import are optional -- they give us access to
Unicode symbols which I think happen to look nicer on this blog.

> {-# LANGUAGE UnicodeSyntax #-}
> import Prelude.Unicode

Now we can move onto the imports.  Of course the two most common imports give
us the assembler itself and speccy utilities:

> import Z80
> import ZXSpectrum

The [z80 package][z80] tries to define an assembler as close as possible to the
original Z80 assembly language -- some of the syntactic differences I will
cover in a moment -- but the result of this is that some names from the Prelude
are overloaded.  Here we hide those names so we don't get clashes.  If you
really need access to the original functions, you can always re-import the
Prelude qualified.

> import Prelude   	hiding (and, or)
> import Data.Bits 	hiding (xor, bit)

We don't actually use anything from *`Data.Bits`* in this tutorial, so the
import is redundant in this case -- I include it here so you can see all the
names you may have to hide when you make these standard imports.

Finally we import *`Data.ByteString`* so that we can do things like test the
length of strings we embed into our program.

> import qualified Data.ByteString as BS

This particular program will all be embedded within a single `main` function,
however it does make use of one Haskell datatype, which we use in a
code-generating macro.  Types can't be defined inline, so let's define it up
here so we can use it later.

> data Direction = DUp | DDown | DLeft | DRight

For convenience, I'm also going to put the title screen into a global constant
since it's rather a long filename.  You can download the title screen (a result
of my unparalleled skill in art, as I'm sure you'll agree), [here][titlescreen].
You'll need a ZX Spectrum SCR viewer to view it, though.

> titleScreen = "static/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/lvtc.scr"

`main` function and static data
-------------------------------

The first line of our program proper sets off the assembler on the rest of the
code.

> main = defaultMain "lambdaman" titleScreen . org 0x6000 $ mdo

`defaultMain` is provided by the [zxspectrum] package and right now is fairly
unsophisticated: it takes the name of the program, the filename of its loading
screen (in the ZX Spectrum standard `.scr` format), and an `ASMBlock`, and
outputs (as a side effect) a `.tap` file ready to load in an emulator.  Future
versions may include some command-line parsing so things like the loading
screen are optional, but for now it's very simple.

The `z80` package provides a type called `Z80` which represents the state of
the assembler -- the functions representing Z80 instruction code form an EDSL
inside this monad.  To get from a `Z80` to an `ASMBlock`, you need to pass it
to `org`, which will set the origin location, from which all further labels are
offset.  This is slightly different from standard assembler, where `org` was
just an assembler directive which could be called at any time.

Finally the `mdo` above opens a `RecursiveDo` block.  If you're not familiar
with Recursive Do, it's basically the same as `do` except that you can use
values before they are defined.

The loader code generated by `defaultMain` will, by default, assume that the
entry point to your application matches the first line of assembly -- i.e.  the
address you pass to `org`.  However, we often want to define a block of static
data that we will modify in our program first, and begin execution after this.
This is made possible by the `beginExecution` directive, which sets the entry
point to the address at which it is called.  It may be used at most once in a
program, and if it is never used the entry point is assumed to be at the start.

This is what allows us to define the following data without worrying about the
Spectrum trying to execute it!

>   let 	lambdamanScoreText 	= "lambdaman "
>       	centipedeScoreText 	= " centipede"
>   lmName 	← labelled $ defb lambdamanScoreText
>   cpName 	← labelled $ defb centipedeScoreText

Here `lambdamanScoreText` is a *Haskell* variable which we use immediately
below to define `lmName`, an area of memory in our output binary containing the
value in `lambdamanScoreText`.  We do this because later we will want to query
it for its length, so it is useful to store it in a variable rather than using
a literal.  The same goes for the centipede.  These represent the labels which
appear at the bottom of the screen.

Note the use of the `labelled` directive, which simply returns the location of
the thing you define in its argument.  For reference, here's what this would look
like in standard assembler:

```
lmName: defb "lambdaman "
cpName: defb " centipede"
```

The Haskell is a bit more verbose here, but we make up for that later with its
macro-defining power.  Onwards!

>   plx  	← labelled $ defb [0]    	-- player's x coordinate.
>   ply  	← labelled $ defb [0]    	-- player's y coordinate.
>   pbx  	← labelled $ defb [0xff] 	-- player's bullet coordinates.
>   pby  	← labelled $ defb [0xff]
>   dead 	← labelled $ defb [0]    	-- flag - player dead when non-0.

Here we define the state relating to the player.  `plx` and `ply` represent the
player's own co-ordinates, while `pbx` and `pby` represent the co-ordinates of the
bullet the player fires.  `dead` simply tells us if the player has been killed.

<div id="sidenote">
The original tutorials on which this is based swap the *x* and the *y*
co-ordinates, so that *x* describes how far down the screen we are and *y* how
far across.  I found this too confusing, so I have a more traditional
co-ordinate system here where *x* is horizontal, *y* is vertical, and the
origin is at the top-left of the screen.
</div>

Another syntactic difference here is the use of Haskell list notation when
passed to `defb`.  `defb` takes either a `ByteString` or a list of `Word8`s,
and puts the values into the final binary file as-is.

>   lambdamanScore 	← labelled $ defb [0, 0]
>   centipedeScore 	← labelled $ defb [0, 0]

Fairly obviously, here we keep track of the scores.

>   -- Table of segments.
>   -- Format: 3 bytes per entry, 10 segments.
>   -- byte 1: 255=segment off, 0=left, 1=right.
>   -- byte 2 = x (vertical) coordinate.
>   -- byte 3 = y (horizontal) coordinate.
>   let numseg = 10
>   segmnt   	← labelled $ defb (replicate (fromIntegral numseg * 3) 0)
>   segsLeft 	← labelled $ defb [numseg]

Here we set up some space for the centipede segments.  The comments describe
the format -- the co-ordinates of that segment and whether it is going left or
right (or has already been destroyed).

User-Defined Graphics
---------------------

Finally, let's define our "assets" -- graphic and sound data that will be used in
the game.  Back in the day, this is how you used to define new graphics for the
ZX Spectrum:

<center><img src="udg.png" /></center>

The [zxspectrum] package defines a macro called `udg` which can perform this
calculation for you.  We use it here to define our four main single-charater
sprites:

>   udgs ← labelled $ do
>     udg 	[ 	"        "
>         	, 	" ss     "
>         	, 	"   s    "
>         	, 	"   s    "
>         	, 	"   ss   "
>         	, 	"  s s   "
>         	, 	" s   s  "
>         	, 	" s    s "]
>     udg 	[ 	"        "
>         	, 	"        "
>         	, 	"   ss   "
>         	, 	"  ssss  "
>         	, 	" ssssss "
>         	, 	"   ss   "
>         	, 	"        "
>         	, 	"        "]
>     udg 	[ 	"        "
>         	, 	"   ss   "
>         	, 	"  ssss  "
>         	, 	" ssssss "
>         	, 	" ssssss "
>         	, 	"  ssss  "
>         	, 	"   ss   "
>         	, 	"        "]
>     udg 	[ 	"   ss   "
>         	, 	"   ss   "
>         	, 	"   ss   "
>         	, 	"   ss   "
>         	, 	"   ss   "
>         	, 	"   ss   "
>         	, 	"  ssss  "
>         	, 	" s ss s "]

As you can see we take a list of 8 strings, each 8 characters long.  Each space is
considered "off", while anything else (here I use the `s` character) is "on".  From
this we can automatically generate the bytes representing that character.

These UDGs are defined one after another in memory.  In a moment, we'll put the
location of the beginning of this block of memory in the special memory address
*`UDG_LOC`*, which will tell the Spectrum ROM to look here when printing UDG
characters.

Sound Effects
-------------

Sound on the ZX Spectrum -- at least the original 48K version -- is handled through
the use of a simple square-wave beeper which can be set to oscillate at a certain
frequency for a certain length of time (during which execution is blocked).  As such
sound effects are less "data" than they are functions which you call, setting the
appropriate registers and executing the ROM routines which cause the beeper to
oscillate (appropriately named *`BEEPER`*).

However, the [zxspectrum] package provides a couple of macros to help with
this.  The one we use here, `playSeq`, takes a list of pairs specifying notes
and their durations, and generates a function which will play the notes in
sequence.  Execution is blocked, so for in-game sound effects we have to keep
them fairly short so they don't interfere with gameplay too much.  The sound
effects below were designed and tracked by my good friend [Alex May][\@atype808].

>   sfxStart ← labelled $ do -- Start game
>     playSeq 	[ (note F_  	4, 	0.111)
>             	, (note AS_ 	4, 	0.222)
>             	, (note C_  	5, 	0.111)
>             	, (note D_  	5, 	0.222)
>             	, (note C_  	5, 	0.111)
>             	, (note AS_ 	4, 	0.222)
>             	, (note F_  	5, 	1.000) ]
>     ret
>   sfxHitC  ← labelled $ do -- Hit centipede segment
>     playSeq 	[ (note E_  	4, 	0.033)
>             	, (note G_  	4, 	0.033)
>             	, (note C_  	5, 	0.066) ]
>     ret
>   sfxHitM  ← labelled $ do -- Hit mushroom
>     playSeq 	[ (note F_  	3, 	0.033)
>             	, (note FS_ 	3, 	0.033) ]
>     ret
>   sfxLost  ← labelled $ do -- Lambdaman died
>     playSeq 	[ (note GS_ 	4, 	0.083)
>             	, (note C_  	5, 	0.083)
>             	, (note A_  	4, 	0.083)
>             	, (note B_  	4, 	0.083)
>             	, (note GS_ 	4, 	0.083)
>             	, (note AS_ 	4, 	0.083)
>             	, (note G_  	4, 	0.083)
>             	, (note A_  	4, 	0.083)
>             	, (note FS_ 	4, 	0.083)
>             	, (note GS_ 	4, 	0.083)
>             	, (note F_  	4, 	0.083)
>             	, (note G_  	4, 	0.083)
>             	, (note E_  	4, 	0.748) ]
>     ret
>   sfxWon   ← labelled $ do -- Centipede died
>     playSeq 	[ (note G_  	5, 	0.222)
>             	, (note F_  	5, 	0.111)
>             	, (note D_  	5, 	0.222)
>             	, (note C_  	5, 	0.111)
>             	, (note D_  	5, 	0.222)
>             	, (note F_  	5, 	0.333)
>             	, (note AS_ 	4, 	0.777) ]
>     ret

The comments describe what each effect does.  `sfxHitC` and `sfxHitM` are both
run during gameplay, so they have been kept short and sweet.  The other effects
happen at a point where the game is halted for some reason anyway, so we can
afford to play something longer.

Since these are functions and not really data, of course, they must end with
`ret` to return exeution to whatever called them -- otherwise the program counter
will just keep on running past them!

Our "data" defined, we are now ready to start the game.  We tell `z80` to set
the entry point for the program here:

>   beginExecution

`z80` syntax
------------

Before we get too heavily into the code, I'd like to say a brief word about the
differences in syntax between traditional Z80 assembly code and the embedded
assembler exposed by the [z80] package.

As far as possible I've tried to keep the two very similar.  The names of all
operations are identical, with the exception of `in` which is a keyword in
Haskell, so I have had to rename it to `in_`.  Other than that, there are just
a few differences to bear in mind if you are already familiar with Z80 assembly
and you'd like to start using the Haskell z80 package:

1. Operands are separated by spaces, not commas, as per Haskell function syntax.
   Sometimes this means you have to surround expressions in parentheses where
   none would be required in z80asm.  This is OK though, because,
2. Pointer dereferencing is done using square brackets `[]` instead of round
   parentheses `()`, as there is no sensible way to detect the presence or
   absence of the latter in Haskell.  Instead, I hijack list syntax to represent
   dereferencing.  Using the empty list or a list with more than one entry
   obviously won't work here, so... don't do that.
3. Traditionally z80asm is case-insensitive.  Haskell is not.  Personally, I
   find having a separate namespace (i.e. capital letters) for registers and
   ROM memory locations to be useful, but if you want to be able to use
   lower-case labels for your registers (`ld a 4` as opposed to `ld A 4`) you
   can; just add `import Z80.Operands.LowerCase` to the top of the file.
4. z80asm has one way to define labels: put a string with a colon at the end in
   the first column.  [z80] has three:  As well as the `labelled` directive
   you've already seen, there's also `label`, which is the closest equivalent to
   a standard labelling mechanism, and `withLabel` which introduces a *scoped*
   label; allowing you to use the same name for multiple labels in your program
   (useful for loops and such!)

That's about it -- everything else falls naturally out of standard Haskell
syntax.  As we go on, I may show some examples of the two side-by-side so you
can get a better idea.

Initialisation
--------------

There are two simple bits of initialisation we'll do once before the game
starts, and then never need to touch again.  The first is to set up the UDG
location to print our "sprites", and the second is to play the "game start"
sound which will alert the user that the game has finished loading and they
should get ready to play.

>   ldVia HL [UDG_LOC] udgs
>   call sfxStart

`ldVia`, as you will have guessed, is not standard z80 assembler.  It is
probably the simplest example of a macro, abstracting the common pair of
operations:

```haskell
       ld HL udgs
       ld [UDG_LOC] HL
```

This is because you can't just arbitrarily load a number into memory; you have
to load it into a register first and then load it from that register into the
memory area you wanted.  `ldVia` is so common that I have included it in the
[z80] package so you can use it out of the box.

<div class='sidenote'>
`ldVia` also gives us a nice example of how, by using Haskell, we can make our
macros 'type-safe'.  Here's its type signature:

```haskell
ldVia :: (Load a c, Load b a) ⇒ a → b → c → Z80ASM
```

What this says is, "given an `a` that can be loaded into from `c`, and a `b`
that can be loaded into from `a`, as well as the initial `c`, produce the
assembly code to load `c` into `a` and then load that `a` into `b`".
*`Z80ASM`* is just a type alias for *`Z80 ()`*.

Whether this form of type-safety is actually _useful_ is up for debate.  With a
traditional macro assembler, the macro would just be expanded and then assembly
would fail from there.  And the type gymnastics I had to do to make this look
like Z80 assembler mean that the error messages when you get it wrong can be
less than useful.  But it's interesting!  And the type signatures are still
useful, if just as documentation.
</div>

After that, we play the intro sound by just calling the function we defined
above to play it.

Setting up the play area
------------------------

That sets us up ready to start the game -- the next step is to clear the screen
(it currently has the loading screen on it) and set up the play area.  We'll do
this between each round of the game, so let's make a label we can jump back to
when the player wins or loses.

>   gameStart ← label

The first step is to set up our default colours and clear the screen to those
colours.  This is accomplished using a couple of helper functions from the
[zxspectrum] package.

>   setBorderColour Black
>   setAttrs AttrDefault NoFlash Bright (Paper Black) (Ink White)
>   call CL_ALL

The first two lines here are just macros which poke the appropriate data into
the right bit of memory based on their parameters.  The last line calls the
*`CL_ALL`* routine, which is defined in the Spectrum ROM and clears the entire
screen.

Next, let's write the scores on the screen.  This is a bit of a diversion from
traditional centipede; in this game you are battling against the centipede to
see who can destroy their opponent the most times (spoiler: it's probably the
centipede).  Every time you play, either lambdaman gets hit and dies,
incrementing the centipede's score counter, or lambdaman destroys all ten of
the centipede's segments, incrementing the lambdaman score counter.  Since
scores are only modified between rounds we can get away with rendering them
once at the beginning of the round and then leaving them untouched, rather than
re-rendering them every frame, which will save us precious cycles.

>   ld A 2
>   call CHAN_OPEN
>   setCursorPos (0, 21)
>   
>   ld DE lmName
>   ld BC . fromIntegral $ BS.length lambdamanScoreText
>   call PR_STRING
>   ld BC [lambdamanScore]
>   call OUT_NUM_1
>   
>   let centipedeScoreStartX =
>         fromIntegral $ 31 - 3 - BS.length centipedeScoreText
>   setCursorPos (centipedeScoreStartX, 21)
>   ld HL centipedeScore
>   call OUT_NUM_2
>   ld DE cpName
>   ld BC . fromIntegral $ BS.length centipedeScoreText
>   call PR_STRING

We make use of a number of standard Spectrum ROM routines to accomplish this.
*`PR_STRING`* prints a string formed by taking the address stored in the *`DE`*
register, and *n* bytes after that, where *n* is the number stored in the
*`BC`* register.

*`OUT_NUM_1`* and *`OUT_NUM_2`* are both for printing numbers; the former
prints the number in the *`BC`* register, while the latter uses the *`HL`*
register and, more importantly, right-aligns the number.

All this faffing about with what registers are used for what should convince
you that just because we are writing Haskell, we are not writing high-level
code here.  This is still bona-fide z80 assembler; the Haskell is just there to
give us a powerful, extensible macro system.

The next step is to restore the player and centipede state.  From the second round
onwards this will have been modified by playing the game, so we need to
initialise it to decent starting values.

>   xor A
>   ld [dead] A
>   ldVia A [segsLeft] numseg
>   ldVia HL [plx] $ coords (15, 20)

`xor` performs an XOR between whatever you pass it and the accumulator `A`,
putting the result back in `A`.  `xor A`, therefore, is just a really cheap way
to set `A` to 0, since anything XOR'd with itself is 0.

We use this trick to set the `dead` state to 0, meaning that the player is
alive.  We also set the number of remaining segments to equal the total number
of segments, restoring any that were destroyed in the previous round.  Lastly,
we put lambdaman in the player's starting position, at the middle of the screen
on the bottom row.

>   ld HL segmnt
>   decLoopB 10 $ do
>     ld [HL] 1 	-- start off moving right.
>     inc HL    	
>     ld [HL] B 	-- use B register as x coordinate.
>     inc HL    	
>     ld [HL] 0 	-- start at top.
>     inc HL    	

Here we set the initial state of the segments, which may have been scattered
around during the previous round.  We make use of `decLoopB`, a macro which
puts the number you pass it into the *`B`* register and then decrements it
every iteration, using the Z80 `djnz` operation to drop out when it reaches 0.

The final bit of initialisation is to fill the play area with randomly
scattered mushrooms.

>   setAttrs AttrTemp NoFlash Bright (Paper Black) (Ink Green)
>   decLoopB 50 $ do
>     printVal AT   	-- control code for AT character.
>     call random   	-- get a 'random' number.
>     and 0x0f      	-- want vertical in range 0 to 15.
>     printA        	
>     call random   	-- want another pseudo-random number.
>     and 0x1f      	-- want horizontal in range 0 to 31.
>     printA        	
>     printVal 0x91 	-- UDG 'B' is the mushroom graphic.

First we set the attributes such that the *`INK`* is set to green, the colour
of mushrooms the world over.  Then we loop down from 50, placing mushrooms each
iteration.  The code looks complicated, but it's actually quite simple: first
we print the *`AT`* metacharacter which has to be followed by *y* and *x*
co-ordinates, in that order.  The co-ordinates, then, are randomly generated
using the `random` function, which we'll define later, and their range is
limited using some binary arithmetic.  We print those, and then finally print
our mushroom sprite, which is UDG value `0x91`.

The main loop
-------------

We're now ready to run the main loop of our game!  Traditionally, you would
define a label here, and then put an unconditional jump to that label at the
end of your loop, but we have a macro to do that:

>   loopForever $ do

Here, "forever" is a bit of a stretch; naturally as this is assembly I can jump
out of the loop to any other part of the program at any time -- all the world's
a `GOTO`!

<div class="sidenote">
Astute readers may have noticed that I use `do` here instead of `mdo`.  We
don't define any labels within this sub-block, or need to use those labels
before their definition, so we don't need to make use of recursive do.  We get
all the labels from the surrounding scope, which *is* recursive, regardless.
</div>

The main loop is mostly just a list of calls to other functions, which we'll
define below.  The first thing we want to do every frame is delete the player
sprite, so we can redraw it in its new position if it moved.

>     setCursorPos ([plx], [ply])
>     call wspace

Here we first set the cursor position to the player's position, and then use
`wspace` to draw a whitespace character at the current cursor position.  We are
now ready to handle input.

>     call input
>     call vimput

I have two routines for input handling, though only one is necessary.  The
first uses the standard ZX Spectrum controls of Q, A, O, P and space, while the
second uses vim-style controls of H, J, K, L, and enter.  Both are always
active, so you can play with whichever you prefer.

The above functions will have updated the player's position if a move key was
pressed.  We are now ready to redraw the player at their current position.

>     setCursorPos ([plx], [ply])
>     call splayr

Again we set our co-ordinates to the player's position, but this time instead
of using `wspace` to draw an empty character we use `splayr` to draw the player
sprite.

Next up is bullet handling.  Rather than going into lots of detail here, I'll
just leave the inline comments intact.  We'll take a proper look at these
functions later.

>     call bchk    	-- check bullet position.
>     call defbull 	-- delete bullets.
>     call moveb   	-- move bullets.
>     call bchk    	-- check new position of bullets.
>     call pbull   	-- print bullets at new position.

The last thing that needs updating is the centipede segments, which we delegate
to the function `processSegments`

>     call processSegments

Finally, the end-of-frame handling.  We put in an artificial delay because
otherwise the game is unplayably fast:

>     halt

And then we check our win/lose conditions.  First, we check the `dead` flag
to see if it is non-zero -- if it is, we were killed so we should jump to
the `gameOver` routine.

>     ld A [dead]
>     and A
>     jp NZ gameOver

Otherwise, we see if the number of remaining centipede segments is zero -- if
it is, we killed the centipede, so we can go to `gameWon`.

>     ld A [segsLeft]
>     cp 0
>     jp Z gameWon

If neither of these is true, we are still playing, so we can simply loop.  The
`loopForever` macro handles this for us, so we can simply end the block here.

Handling input
--------------

The following two routines handle input.  I'm going to do the vim ones first because
they demonstrate something quite interesting about how the ZX Spectrum keyboard works.

>   vimput ← labelled $ do
>     ld BC KEYS_HJKLret  	-- vim keys
>     in_ A [C]           	-- see what keys are pressed.
>	 
>     rra                 	-- Enter to fire
>     push AF             	-- remember the value.
>     call NC fire        	-- it's being pressed, fire
>     pop AF              	-- restore accumulator.
>     rra                 	-- shift the next bit (l).
>     push AF             	-- remember the value.
>     call NC mpr         	-- it's being pressed, move right.
>     pop AF              	-- restore accumulator.
>     rra                 	-- get the next bit (k).
>     push AF             	-- remember the value.
>     call NC mpu         	-- being pressed, so move up.
>     pop AF              	-- restore accumulator.
>     rra                 	-- and the next bit (j)...
>     push AF             	-- remember the value.
>     call NC mpd         	-- being pressed, so move down.
>     pop AF              	-- restore accumulator.
>     rra                 	-- finally, the next bit (h).
>     call NC mpl         	-- it's being pressed, move left.
>     ret

The keyboard on the Spectrum was divided into groups of five keys, which could
be queried at once and each individual key accessed by checking one bit at a
time from the resulting value.  As it happens, H, J, K, L and enter formed one
of these groups, so checking the vim keys can be done with a single query!  We
then just rotate the accumulator one bit at a time, calling the function to
handle that particular bit of input each time that bit shows the key is
pressed.

The standard Spectrum controls, Q, A, O, P, and space, are all from different
groups on the keyboard, but they are generally the first or second key from
that group, so although we have to do more queries, we don't have to keep
bitshifting the accumulator so much.

>   input ← labelled $ do
>     ld BC KEYS_TREWQ
>     in_ A [C]
>     rra
>     call NC mpu
>     ld BC KEYS_GFDSA
>     in_ A [C]
>     rra
>     call NC mpd
>     ld BC KEYS_YUIOP
>     in_ A [C]
>     rra
>     push AF
>     call NC mpr
>     pop AF
>     rra
>     call NC mpl
>     ld BC KEYS_BNMsssp
>     in_ A [C]
>     rra
>     call NC fire
>     ret

A real game would probably have user-mappable keys and much more complicated
keyboard handling routines as a result, but for our purposes this is perfectly
adequate.

Lambdaman
---------

We're ready to start defining the actual gameplay entities, beginning with
our hero, the player -- Lambdaman.

The following routine sets the cursor to the player position.  It's used to
delete the character at the old position (by setting the cursor prior to moving
the player, and drawing whitespace), and to draw the character into the new
position (by setting the cursor after moving the player, and drawing the
character).

>   basexy ← labelled $ do
>     setCursorPos ([plx], [ply])
>     ret

Player drawing is done by the `splayr` routine.  `0x90` here is ASCII code
for User Defined Graphic 'A', which is where we put our lambdaman sprite
right at the start.

>   splayr ← labelled $ do
>     setAttrs AttrTemp NoFlash Bright (Paper Black) (Ink Cyan)
>     printVal 0x90
>     ret

The majority of player code is spent implementing the functionality enabled by
the keyboard mappings above.  First, though, we're going to make a little
utility function to check for the presence of a mushroom in the direction we
want to move.  Lambdaman can't stand on top of mushrooms, so if there is a
mushroom we want to return early -- before we move the character.

>   let checkMushroom direction = do
>         let 	move DLeft  	= dec C; 	move DRight 	= inc C
>             	move DUp    	= dec B; 	move DDown  	= inc B

>         ld BC [plx]     	-- current coords.
>         move direction  	-- move to the position we want to check.
>         call atadd      	-- address of attribute at this position.
>         cp 0x44         	-- mushrooms are bright green (0x44).
>         ret Z           	-- there's a mushroom - return early.

The interesting thing to note here is that `checkMushroom` is a *Haskell*
function, not a function in our program (i.e. a labelled block).  What does
this mean?  Essentially, it acts like a macro in a traditional macro assembler
-- the code in the function will be generated at the call-site.  Crucially,
this means that the `ret Z` above doesn't return from the function
`checkMushroom` (there is no such function in the output assembly).  Instead,
it returns from the calling function.  This, it turns out, is exactly what we
want: to drop out of the calling function before the move has been enacted, in
the case that the move would result in Lambdaman standing on a mushroom.

In case it's not clear from looking at the code, the way we check for mushrooms
is simply to poll the attributes of the character.  If it's green, we know it's
a mushroom, because there are no other green objects in the game.  This
extremely primitive approach to collision detection was actually quite common
in early Spectrum games!

>   mpl ← labelled $ do
>     ld HL plx           	-- plx is "player x".
>     ld A [HL]           	-- what's the current value?
>     and A               	-- is it zero?
>     ret Z               	-- yes - we can't go any further left.
>     checkMushroom DLeft 	-- check for mushrooms.
>     dec [HL]            	-- decrement x position.
>     ret

OK, so here's the first actual movement function, `mpl` (for "move player
left").  The principle is fairly simple, and we'll see this pattern repeated
over the next few functions.  The basic idea is to load the current value,
check it's not 0 (the very left of the screen), and return if it is, check
there's no mushroom there, and return if there is, and finally -- if we've
made it this far -- decrement the player's $x$ co-ordinate, thus moving it
left.

>   mpr ← labelled $ do
>     ld HL plx
>     ld A [HL]
>     cp 31
>     ret Z
>     checkMushroom DRight
>     inc [HL]
>     ret

This function should look familiar.  Instead of checking for 0, we compare
against 31 -- the rightmost column -- and instead of decrementing we increment.
Otherwise it's basically the same.  I could have extracted this pattern out
into a helper function like I did with `checkMushroom`, but I actually found
there were just enough differences that writing out the code for each case
ended up being clearer than passing the parts which change as parameters to a
function.

The following functions do the same for the up and down directions.

>   mpu ← labelled $ do
>     ld HL ply
>     ld A [HL]
>     cp 0
>     ret Z
>     checkMushroom DUp
>     dec [HL]
>     ret
> 
>   mpd ← labelled $ do
>     ld HL ply
>     ld A [HL]
>     cp 20
>     ret Z
>     checkMushroom DDown
>     inc [HL]
>     ret

The only other routine left to define is missile firing.  We keep this simple
by only supporting one missile on screen at a time -- that missile's position
is stored globally along with the player, as `pbx` and `pby` (player bullet
x/y).  When the missile is off-screen (i.e. there is no missile), we set the
$y$ value to 255.

>   fire ← labelled $ do
>     ld A [pby]
>     inc A
>     ret NZ
>     ld HL [plx]
>     dec H
>     ld [pbx] HL
>     ret

We can check the existence of a missile by putting its $y$ position in a
register and incrementing it.  If it has position 255 it will wrap around to 0.
If not, we know that it must be on the screen so we return early without
spawning.  To spawn the missile, we simply take the player's position and
increment the $y$ value, placing the missile immediately above the player.

Missiles
--------

OK, so now the player's fired a missile, we need to make it do something!
Missiles move steadily up the screen, which can be achieved by simply
decrementing the $y$ co-ordinate every frame the missile is on the screen.  We
first perform an increment to check whether the missile is on-screen.  If not,
we drop out immediately; if it is on-screen we subtract 2 -- 1 to undo the
increment we just performed and 1 more to actually move the missile.  Finally,
we load the register value back into `pby`.

>   moveb ← labelled $ do
>     ld A [pby]
>     inc A
>     ret Z
>     sub 2
>     ld [pby] A
>     ret

Next, we need to handle collision detection between the missile and the
mushrooms (we'll deal with centipede collision in the next section). The `bchk`
routine checks the position of the missile and, if it hits a mushroom, destroys
it.  We do this by first checking the $y$ co-ordinate to make sure there is a
missile on screen at all, and if there is, checking the attribute at that
location.  If the character is green, that means it is a mushroom, so we can
call the `hmush` function to destroy it.  Otherwise, we simply return.

>   bchk ← labelled $ do
>     ld A [pby]
>     inc A
>     ret Z
>     ld BC [pbx]
>     call atadd
>     cp 0x44
>     jr Z hmush
>     ret

`hmush` is very simple -- we simply replace the character with whitespace and
play the appropriate sound effect.  We also set the missile's $y$ position back
to 255 to signify that it is "off screen".  This last step is done within the
`kilbul` label, which we can jump to from elsewhere in the program, but as
`hmush` contains no `ret` statement it will run straight from `call sfxHitM` to
`ldVia A [pby] 0xff`.  You can think of it like a fall-through case in C switch
statements, if you like (since the labels in a switch statement are essentially
`goto` labels, this is actually a pretty accurate way to think of it).

>   hmush ← labelled $ do
>     setCursorPos ([pbx], [pby])
>     call wspace
>     call sfxHitM
>   kilbul ← labelled $ do
>     ldVia A [pby] 0xff
>     ret

To draw the missile, we follow the same pattern as when we drew the player.
`bullxy` sets the cursor position to the current position of the missile (which
might be off screen).

>   bullxy ← labelled $ do
>     setCursorPos ([pbx], [pby])
>     ret

The `pbull` routine checks whether the missile is on screen, and if it is,
prints the sprite `0x93`, or UDG 'D', at that location.

>   pbull ← labelled $ do
>     ld A [pby]
>     inc A
>     ret Z
>     call bullxy
>     printVal INK
>     printVal YELLOW
>     printVal 0x93
>     ret

We clear the missile using the `defbull` routine.  This is as distinct from
`kilbul`, which logically "kills" the bullet, by setting its position to 255;
here we are deleting the bullet's sprite from its current position on the
screen, but not changing the location of the bullet within the game logic at
all.

>   defbull ← labelled $ do
>     ld A [pby]
>     inc A
>     ret Z
>     call bullxy

As with `kilbul` above, we run straight into the next function, `wspace`, to do
the actual clearing of the sprite at the current position.  This function is
called from a number of places in code, but `defbull` has the advantage of
being defined right next to it, so it can fall through to it without paying the
cost of a function call.

>   wspace ← labelled $ do
>     setAttrs AttrTemp NoFlash Bright (Paper Black) (Ink White)
>     printVal $ chr ' '
>     ret

Note that we set the colours to white-on-black.  This may not seem important,
since we're drawing an empty space so there are no foreground (white) pixels to
display -- but remember, collision detection is done using colour attributes!
If we left the colours as they were, the space would continue to act as an
invisible missile, or mushroom, or whatever was there before!

The centipede
-------------

Next, we deal with the centipede.  Being composed of a number of segments, the
centipede is actually fairly complex -- though by and large the segments work
independently, even while they appear to be connected together.  We begin with
a helper routine, `segxy`, which performs the same job as `basexy` and `bullxy`
above: it sets the cursor to the position of the current segment.

>   segxy ← labelled $ do
>     setCursorPos ([IX+1], [IX+2])
>     ret

Now that we have this, we want to go through each segment (of which there are
ten) and process them, one by one.  We do this using the trusty `decLoopB`
macro we made use of earlier.  Each segment contains three bytes: the segment's
current status, and its $x$ and $y$ co-ordinates.  Current status has three
possible values: 0 for "moving left", 1 for "moving right", and 255 to disable
the segment once it's been destroyed.  The loop below goes through each segment
and checks whether the status is 255 -- if not, it runs `proseg` to process
that segment, otherwise it skips over that and proceeds to the next one.

>   processSegments ← labelled $ do
>     ld IX segmnt        	-- table of segment data.
>     decLoopB 10 $ do
>       push BC
>       ld A [IX]         	-- is segment switched on?
>       inc A             	-- 255=off  increments to zero.
>       call NZ proseg    	-- it's active, so process segment.
>       pop BC
>       ld DE 3           	-- 3 bytes per segment.
>       add IX DE         	-- get next segment in ix registers.
>     ret

The crux of the centipede's processing code happens here, in `proseg`.  As with
the main loop, this is quite a high-level routine that delegates most of its
actual processing to a few purpose-built routines, defined below.

We begin by calling `segcol` to check for collisions with this segment.  If
there was a collision, `segcol` will have turned off the segment, so we can
check for that and drop out early if that's happened.  Otherwise, we clear the
segment's previous position, move the segment into its new position with
`segmov`, and perform another collision check.  Again, we need to check whether
the segment was destroyed by the collision.  If it wasn't, we draw the UDG
character 'C', which contains our segment graphic.

>   proseg ← labelled $ do
>     call segcol             	-- segment collision detection
>     ld A [IX]               	-- check if segment was switched off
>     inc A                   	-- by collision detection routine.
>     ret Z                   	-- it was  so this segment is now dead.
>     call segxy              	-- set up segment coordinates.
>     call wspace             	-- display a space  white ink on black.
>     call segmov             	-- move segment.
>     call segcol             	-- new segment position collision check.
>     ld A [IX]               	-- check if segment was switched off
>     inc A                   	-- by collision detection routine.
>     ret Z                   	-- it was  so this segment is now dead.
>     call segxy              	-- set up segment coordinates.
>     setAttrs AttrTemp NoFlash NotBright (Paper Black) (Ink Red)
>     printVal 0x92
>     ret

The remaining functions therefore fall into two categories: segment movement,
and collision detection and handling.  We'll deal with movement first.

>   segmov ← labelled $ do
>     ld A [IX+1] 	-- x
>     ld C A
>     ld A [IX+2] 	-- y
>     ld B A
>     ld A [IX] 	-- status
>     and A
>     jr Z segml

The segment is moving either left or right.  We begin by loading the current
co-ordinates into registers, and then checking which way it's moving.  If it's
moving left, we jump to the `segml` routine.  If right, we fall through to the
next routine, `segmr`.

>   segmr ← labelled $ do
>     ld A [IX+1]
>     cp 31
>     jr Z segmd
>     inc A
>     ld C A
>     call atadd
>     cp 0x44
>     jr Z segmd
>     inc [IX+1]
>     ret

The two movement routines are very similar.  First we check if we're at the
edge of the screen, and if so jump to `segmd` to move down one line.  If not,
we check the attributes of the space one to the right, to see if there's a
mushroom there (Remember, mushrooms are bright green: `0x44`).  Again, if there
is we jump to `segmd`.  If we passed both of these tests, there are no
obstacles, so we can increment the $x$ position to move the segment to the
right.

>   segml ← labelled $ do
>     ld A [IX+1]
>     and A
>     jr Z segmd
>     dec A
>     ld C A
>     call atadd
>     cp 0x44
>     jr Z segmd
>     dec [IX+1]
>     ret

`segml` is almost identical.  We use `and A` again to check if we're at the
left edge.

Next, we define `segmd`, which the previous two routines used to move the
segment down in the case of an obstacle or the edge of the screen.

>   segmd ← labelled $ do
>     ld A [IX]
>     xor 1
>     ld [IX] A
>     ld A [IX+2]
>     cp 20
>     jr Z segmt
>     inc [IX+2]
>     ret

Whenever the segment moves down, it should reverse direction, so that's the
first thing we do here.  After that, we check if we're at the bottom of the
screen -- if we are, we jump to `segmt` to move us back up to the top.
Otherwise, we move down -- regardless of whether there are any mushrooms or
other obstacles in the way.  This prevents a bug from happening whereby the
mushrooms form a sort of "bucket" that can trap a segment.

>   segmt ← labelled $ do
>     xor A
>     ld [IX+2] A
>     ret

This is the final movement function.  All it does is to set the $y$ co-ordinate
to 0.  The $x$ co-ordinate will be wherever the segment was on the bottom line
-- usually the left or right edge, unless it happened to hit a mushroom.

Finally, we handle collision detection for centipede segments.  We've already
dealt with mushrooms, which cause the centipede to turn and move down the
screen.  The remaining items a segment could collide with are the player,
killing him and scoring a point for the centipede, or the missile, which turns
the segment into a mushroom and destroys the missile that hit it.  First, the
player.

>   segcol ← labelled $ do
>     ld A [plx]
>     cp [IX+1]
>     jr NZ bulcol
>     ld A [ply]
>     cp [IX+2]
>     jr NZ bulcol

`segcol` first compares the segment's position with the player's.  If either
the $x$ or the $y$ positions differ, it jumps to `bulcol` to try for missile
collisions instead.  If they are both the same, it falls through to the
following routine, `killpl`, which sets the player's `dead` flag.

>   killpl ← labelled $ do
>     ld [dead] A
>     ret

Checking for a collision with the missile is similar to the collision check
above -- we simply compare $x$ and $y$ co-ordinates.  If either of them is
different we know there is no collision, so we can return.

>   bulcol ← labelled $ do
>     ld A [pby]
>     inc A
>     ret Z
>     cp [IX+2]
>     ret NZ
>     ld A [pbx]
>     cp [IX+1]
>     ret NZ

If we get this far, there has been a missile collision.  We call `defbull` to
delete the missile graphic, and then replace the segment's own graphic with a
mushroom.  Finally, we call `kilbul` to "logically" kill the bullet, setting
its $y$ co-ordinate to 255, and we also flag the segment as inactive.  If there
are still segments left, we play the "segment hit" sound effect, otherwise we
don't need to play any effect because the game over sound will play.

>     call defbull
>     printVal AT
>     ld A [pby]
>     inc A
>     printA
>     printVal [pbx]
>     printVal INK
>     printVal GREEN
>     printVal 0x91
>     call kilbul
>     ld [IX] A
>     ld HL segsLeft
>     dec [HL]
>     push IX
>     call NZ sfxHitC
>     pop IX
>     ret

And, that's it for the centipede!

Utilities
---------

There are a couple of utilities we've made use of in this game, both of which
are taken from the same set of tutorials where I got most of the code for the
rest of the game, but which would be useful in any game, not just this one.
The first is a pseudo-random number generator which cleverly uses the Spectrum
ROM itself as its source for randomness!  It simply treats the seed as an
offset into the ROM memory.  You can see a fuller description on [the original
page][random], but here's the code.

>   random ← labelled $ do
>     ld HL [seed]
>     ld A H
>     and 31
>     ld H A
>     ld A [HL]
>     inc HL
>     ld [seed] HL
>     ret
>   seed ← labelled $ defb [0,0]

The other utility which has come up a lot is `atadd`, which calculates, from a
pair of $(x, y)$ co-ordinates, the location in memory for that co-ordinate's
attributes.  This is extremely useful for polling co-ordinates for their
attributes, which was the basis of all our collision detection.  The original
utility, with comments, can be seen [here][atadd].

>   atadd ← labelled $ do
>     ld A B
>     rrca
>     rrca
>     rrca
>     ld E A
>     and 3
>     add A 88
>     ld D A
>     ld A E
>     and 0xe0
>     ld E A
>     ld A C
>     add A E
>     ld E A
>     ld A [DE]
>     ret

End conditions
--------------

We're just about ready to wrap up our game!  The last thing remaining is to
deal with the two end conditions -- either the centipede killed Lambdaman,
or Lambdaman killed the centipede.  In either case, we play a sound effect,
increase the appropriate score, and jump back to `gameStart` to begin the
next round.

>   gameOver ← labelled $ do
>     call sfxLost
>     ld HL $ centipedeScore + 1
>     inc [HL]
>     jp gameStart
>   gameWon ← labelled $ do
>     call sfxWon
>     ld HL lambdamanScore
>     inc [HL]
>     jp gameStart

Out game is done, but because of the way labels work in `z80`, it is common for
the last line in any program to be a variable definition (in this case
`gameWon`).  This, of course, is not allowed in Haskell, so `z80` defines `end`
as an alias for `return ()`, giving us an expression to end on.

>   end

With that, we can compile and run the program, which should spit out a file called
`lambdaman.tap` which you can load into your favourite emulator.

[haskell-imperative]: https://www.microsoft.com/en-us/research/wp-content/uploads/2016/07/mark.pdf
[z80]:                https://github.com/dpwright/z80
[zxspectrum]:         https://github.com/dpwright/zxspectrum
[monad-6]:            https://wiki.haskell.org/wikiupload/1/14/TMR-Issue6.pdf
[speccy-tuts]:        https://chuntey.wordpress.com/2012/12/18/how-to-write-zx-spectrum-games-chapter-1/
[titlescreen]:        lvtc.scr
[tapfile]:            lambdaman.tap
[jsspeccy]:           http://jsspeccy.zxdemo.org
[\@danielpwright]:    https://twitter.com/danielpwright
[\@atype808]:         https://twitter.com/atype808
[random]:             https://chuntey.wordpress.com/2013/02/28/how-to-write-zx-spectrum-games-chapter-4/
[atadd]:              https://chuntey.wordpress.com/2013/02/28/how-to-write-zx-spectrum-games-chapter-5/
