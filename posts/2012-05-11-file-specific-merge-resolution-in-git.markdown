---
title: File-specific merge resolution in git
date: 2012-05-11 14:18
tags: git, subversion, version control
---

I've been using git-svn for a while now so that I can take advantage of the
power and convenience of git without having to bother everyone else on the
project (who are all happily using SVN). It was a bit frustrating at first, but
once I got the hang of the basic workflow and settled into a routine I started
to really like it, and now I don't think I could ever go back to using the SVN
client directly.

One annoyance I've come across multiple times using both SVN and Git is that of
checked in binary files. Ideally, we'd only check in the source files and build
off that, but in practice this is problematic for a number of reasons. We'd like
the non-programmers on the team to be able to build the project without having
to have the compiler toolchain installed; we'd like the version of our
executable everyone is running to be guaranteed to be the same; and we'd like it
to be possible to checkout the project and start running it straight away
without having to do a build.

The problem with this is that we get a lot of merge conflicts on the binary
files we're checking in. Take the executable; programmers rebuild this every
time they hit "compile". It is almost certain that each time you pull, there'll
be a new copy of the executable in the repo that clashes with yours.

The solution is simple: we always want to accept our version of the executable
file. That way the timestamp will be older than the new source files we've
received from the repo, so when we run make it will pick up on that and rebuild
the executable with the new code we've just pulled in.

The problem is, I've never found a way to automate that for specific files in
the repo. With SVN, the majority of the team use TortoiseSVN as their interface,
which doesn't seem to offer that sort of flexibility. With git, I was aware of
the "ours" and "Xours" merge strategies but didn't know how to apply them only
to specific files in a merge. I'd read some tips about adding
`merge=ours` into the .gitattributes file, but it didn't seem to
work.

Well, after a little bit of digging around on stackoverflow, I found [this
question][1], the first answer to which explains how to do exactly that simply
and easily. Basically, you have to create a script, which acts as a custom merge
driver. Since git's default behaviour is to leave your copy as-is during a
binary merge, all that driver has to do is `exit 0`:-

``` bash
# I want to keep MY version when there is a conflict
# Nothing to do: %A (the second parameter) already contains my version
# Just indicate the merge has been successfully "resolved" with the exit status
exit 0
```

As a convenience, I added a "keep theirs" driver to go with it:-

``` bash
# I want to keep THEIR version when there is a conflict
# Copy their version over ours and report success
cp -f $3 $2
exit 0
```

Once those scripts are in place, it's simply a matter of defining them in your
`.git/config` file, and then setting which files should use them
using `.gitattributes`:-

``` ini
[merge "keep-mine"]
	name = Always keep mine during merge
	driver = git-merge-keep-mine.sh %O %A %B
[merge "keep-theirs"]
	name = Always keep theirs during merge
	driver = git-merge-keep-theirs.sh %O %A %B
```
``` bash
*.exe -crlf -diff merge=keep-mine
*.tga -crlf -diff merge=keep-mine
```

And that's it! My merges have become *much* more pleasant since I set this up.

If anybody knows how to do this sort of thing using [Tortoise]SVN, I'd love to
hear about it in the comments!

(Note: All the above code samples were copied almost verbatim from the
previously mentioned [stackoverflow entry][1]. I don't claim credit for any of
it.)

[1]: http://stackoverflow.com/questions/928646/how-do-i-tell-git-to-always-select-my-local-version-for-conflicted-merges-on-a-s
