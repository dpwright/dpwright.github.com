---
layout: post
title: Git alias to get a git commit sha1 from an SVN revision number in git-svn
date: 2012-08-01 19:00
comments: true
tags: alias, git, git-svn, subversion
---

This little snippet is useful if you use git-svn -- it gives you an alias to get
a git commit ID from an SVN revision number. If you work with others who say
things like, "I think this problem was introduced in revision 10342" it can come
in pretty handy! Copy and paste it into the `[alias]` section of your global
`.gitconfig` or your project-specific `.git/config`

``` ini
svn-ref = "!f() { git log --grep \\"git-svn-id.*@$1\\" --pretty=%H; }; f"
```
