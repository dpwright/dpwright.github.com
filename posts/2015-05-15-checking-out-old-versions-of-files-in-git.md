---
date: 2015-05-15 11:36:27
tags: git, alias
title: Checking out old versions of files in git
---

Here's another useful alias if you're using git.  Sometimes you want to recover
a file which has been deleted, or restore an old version of a single file.  Or,
you might want to grab a file from a different branch.  This alias will let you
do that:

```ini
[alias]
    get-file = "!f() { git show $1:$2 > $2; }; f"
```

Use it like this:

```
# Get the version of README.md from commit 456e17b
$ git get-file 456e17b README.md

# Get the version of foo.cpp from the 'stable' branch
$ git get-file stable path/to/foo.cpp
```

Note that this will write the file out to its actual location (overwriting
whatever's there), so don't run it if you have local modifications to that file,
unless you don't mind those modifications getting overwritten!

If you are using [git-media], or something similar, you need to pipe the file
through the filter.  I set up another alias to do this:

```ini
    get-media = "!f() { git show $1:$2 | git-media filter-smudge > $2; }; f"
```

[git-media]: https://github.com/alebedev/git-media
