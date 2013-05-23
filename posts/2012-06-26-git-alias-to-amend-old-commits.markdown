---
layout: post
title: Git alias to amend old commits
date: 2012-06-26 19:50
comments: true
tags: alias, git
---

`git commit --amend` is a useful little command for fixing mistakes in log
messages just after you’ve made a commit, but sometimes you don’t realise your
error until a few commits down the line, by which time it’s too late. You have
to reset to the earlier version, amend the commit message, and then rebase all
your commits since then on top of the new, amended commit.

Earlier today, in #git on freenode, somebody who went by the name of constant
mentioned that they wanted to do this in one command. Another member of the
chat, frogsonwheels, suggested a solution which essentially did as described
above in a series of git commands strung together using `&amp;&amp;`.

I decided to tidy it up a bit and put it into a git alias, thus:

``` ini
amend-commit = "!f() { START=`(git symbolic-ref -q HEAD || git rev-parse HEAD) | cut -d"/" -f 3`; git checkout -q $1 && git commit --amend && git rebase --onto HEAD $1 $START; }; f"
```

Note that this is still doing the rebase mentioned above, it’s just automatin
the steps a little bit. These means that all the usual warnings regarding rebase
apply: don’t run this command on a commit which has already been published.
Also, this rebases the currently checked out branch/revision, which means if any
other branches have been made based off the amended commit *or any commit
since*, those branches won’t be rebased and you’ll have to rebase them
automatically once you’ve done the amend-commit. You can usually tell if this
is the case pretty easily by inspecting the output of `git log --graph --oneline
--decorate`.

