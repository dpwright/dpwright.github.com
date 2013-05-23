---
layout: post
title: Invoking ex commands on a subset of files
date: 2012-04-17 10:02
comments: true
tags: ex, sed, shell, vim
---

I use `sed` a lot for modifications I need to make to a number of
files, which is great, but as it happens relatively infrequently I need to
remind myself how to do certain things as and when I come across them.

On the other hand, I use `vim` every day for my general editing, and
am making increasingly heavy use of its command syntax to automate some of my
editing tasks.  As a result, I am probably more familiar with `vim`'s
command syntax than `sed`.

Yesterday I had a simple problem to solve: for every file containing string
SEARCH, I wanted to delete any line matching LINE.  Here's a couple of ways you
could do it using sed:

``` bash
grep -Rl "SEARCH" . | while read FILE; do sed -i "" "/LINE/d" $FILE; done
grep -Rl "SEARCH" . | while read FILE; do sed "/LINE/d" $FILE > tmp; mv tmp $FILE ; done
```

The first is non-portable and works with BSD `sed`; for GNU
`sed` you'd have to remove the "" after the -i.  The second is a
little more portable but requires a temporary file.

Just for kicks, though, I thought I'd do it in `ex` this time. 
Here's how that looks:

``` bash
grep -Rl "SEARCH" . | while read FILE; do (echo "g/LINE/d"; echo 'wq') | ex -s FILE; done
```

Admittedly, for a simple task like this it's probably pointless, but there have
been times where I've repeated a command in `vim` over a series of
files where this might come in ha
