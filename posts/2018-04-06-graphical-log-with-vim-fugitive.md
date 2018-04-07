---
title: Graphical log with vim-fugitive
date: 2018-04-06 20:43:00
thumbnail: /posts/2018/04/06/graphical-log-with-vimfugitive/fugitive-lg.gif
tags: vim, git
---

Tim Pope's excellent [`vim-fugitive`][vim-fugitive] is the best interface I've
encountered for working with git.  I love using `:Gdiff` not just to compare
changes, but to stage and unstage hunks to the index, looking up the origin of
a particular change with `:Gblame`, and being able to fetch, merge, and commit
changes from within the editor.

One feature I miss from other tools, however, is an easy way to browse the
history of the repository as a whole, rather than on a file-by-file basis.
`:Glog` loads the history of the current file into the change list, which is a
great way to see the changes to that file over time, but the changes are
limited to a single file.  You can use `:Glog --` to load the history of the
entire repository, but it will still get loaded as a single, flat list, and
since it is now much longer it can be quite unwieldy to navigate.

I have long had the following line in my git aliases, which displays a
nicely-formatted graph of the repository:

```ini
	lg = log --graph --pretty=format:'%Cred%h%Creset - %Cgreen(%ad)%C(yellow)%d%Creset %s %C(bold blue)<%an>%Creset' --abbrev-commit --date=local
```

I can't remember where I originally copied this from.  It looks very similar
to [this one][coderwall-lg], although the date is at the start, and absolute
rather than relative.  Perhaps I customised it.  This is the output it
produces:

<center>![](/posts/2018/04/06/graphical-log-with-vimfugitive/git-lg.png "Output of the git-lg command")</center>

I've always liked this visualisation, but one problem with it is that it's
pretty static---you just get the list of commits, with no way to "select" a
commit and see its contents.  For this, I've tended to use [`tig`][tig], or
some graphical git client like `gitk`.  I don't find `tig` nearly as readable
as `git lg`, though, and having to switch to a whole graphical client just to
read the log feels like overkill.  And, apart from anything else, what I
usually want to do once I've *found* the commit I'm interested in is to edit it
in vim!

Well, it turns out it's easy to get this integrated into vim using fugitive.
As a first pass, I simply created a `Glg` command by adding the following to
my `.vimrc`:

```
command -nargs=* Glg Git! log --graph --pretty=format:'\%h - (\%ad)\%d \%s <\%an>' --abbrev-commit --date=local <args>
```

Alternatively if you already have the `git lg` alias set up in your `.gitconfig`
you can just call that:

```
command -nargs=* Glg Git! lg <args>
```

This gives us the graph visualisation we want, but some syntax highlighting
would be nice.  Fugitive's `:Git!` command sets the filetype of the output
buffer to `git`, so I created the file `~/.vim/after/syntax/git.vim` and added
the following highlighting rules to it. 

```
syn match gitLgLine     	/^[_\*|\/\\ ]\+\(\<\x\{4,40\}\>.*\)\?$/
syn match gitLgHead     	/^[_\*|\/\\ ]\+\(\<\x\{4,40\}\> - ([^)]\+)\( ([^)]\+)\)\? \)\?/ contained containedin=gitLgLine
syn match gitLgDate     	/(\u\l\l \u\l\l \d\=\d \d\d:\d\d:\d\d \d\d\d\d)/ contained containedin=gitLgHead nextgroup=gitLgRefs skipwhite
syn match gitLgRefs     	/([^)]*)/ contained containedin=gitLgHead
syn match gitLgGraph    	/^[_\*|\/\\ ]\+/ contained containedin=gitLgHead,gitLgCommit nextgroup=gitHashAbbrev skipwhite
syn match gitLgCommit   	/^[^-]\+- / contained containedin=gitLgHead nextgroup=gitLgDate skipwhite
syn match gitLgIdentity 	/<[^>]*>$/ contained containedin=gitLgLine
hi def link gitLgGraph    	Comment
hi def link gitLgDate     	gitDate
hi def link gitLgRefs     	gitReference
hi def link gitLgIdentity 	gitIdentity
```

This is all pretty rudimentary, but it seems to do the job!  If you change the
format of `git lg`, these syntax highlighting rules would need to be updated as
well.  Here's the end result in action:

<center>![](/posts/2018/04/06/graphical-log-with-vimfugitive/fugitive-lg.gif "The :Glg command in action")</center>

Because the temporary buffer `vim-fugitive` creates for the output of `:Git!`
commands is of filetype `git`, we get some of fugitive's global shortcuts for
free.  This allows us to open the commit under the cursor with `<C-w>f`, and
from there inspect the diffs for each file within.

It's not perfect---some shortcuts are inexplicably unavailable, and it would
be nice if `<C-w>f` worked anywhere in the line, rather than having to move
the cursor over the SHA1 hash itself---but it's pretty good considering how
easy it was to set up!

[vim-fugitive]: https://github.com/tpope/vim-fugitive
[coderwall-lg]: https://coderwall.com/p/euwpig/a-better-git-log
[tig]:          https://github.com/jonas/tig
