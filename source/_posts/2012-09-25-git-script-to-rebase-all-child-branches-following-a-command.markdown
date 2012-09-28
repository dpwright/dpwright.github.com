---
layout: post
title: "Git script to rebase all child branches following a command"
date: 2012-09-25 10:27
comments: true
categories:
  - alias
  - git
  - git-svn
  - ruby
  - scripts
---

This script looks at the current status of the DAG to find the children of the
current branch, runs an action, then rebases those children.  It is particularly
useful for users of `git-svn`, who may find themselves having to rebase all
topic branches (and sub-topics which build off those) every time they `git svn
rebase` or `git svn dcommit`.

For pure git projects, this is considered by many to be bad form, so use with
discretion.  People who like a linear history might like it.

I expect the script, in its current state, will fail in cases where the rebase
can't be done automatically, but for simple day-to-day operations it makes
`git-svn` that bit less painful to use :-)

{% gist 3779324 git-rar.rb %}

I have an alias set up to invoke it with `git rar` ("Run and Rebase"), so that I
can type, for example, `git rar svn rebase`.
