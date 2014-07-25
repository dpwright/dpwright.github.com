---
title: Git alias to open all diffs in vim tabs
date: 2012-06-05 15:46
tags: alias, git, vim
---

This is a handy little alias I've been using recently to open all unstaged diffs
in a single instance of vim, one tab per file. Add the following to the
`[alias]` section of your `~/.gitconfig`:

``` bash
dt = "!f() { vim -p $(git diff --name-only) +\"tabdo Gdiff $@\" +tabfirst; }; f"
```

Known issues:

 * It requires tpope's [vim-fugitive][1] plugin to work.
 * It only works with unstaged diffs -- it would be nice to be able to pass an
   arbitrary range of commits and see the diffs between them.

Both of the above could be resolved by writing a little script to open all the
diffs in vim rather than using fugitive's `:Gdiff` command, but for now this
little alias does the job nicely.

[1]: https://github.com/tpope/vim-fugitive
