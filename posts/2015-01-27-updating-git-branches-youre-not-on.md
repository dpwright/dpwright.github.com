---
date: 2015-01-27 18:12:23
tags: git, alias
title: Updating git branches you're not on
---

Do you use `git`?

Have you ever been on a feature branch and wanted to pull all the upstream
changes into your `master` branch before rebasing onto that? Of course you have.

If you're sick of doing this:

```
$ git checkout master
$ git pull
$ git checkout feature-branch
$ git rebase master
```

Add the following lines to your `.gitconfig`:

```ini
[alias]
    remote-for-branch = "!f() { git for-each-ref --format='%(upstream:short)' `for b in $@; do echo refs/heads/$b; done` | sed 's:/.*$::'; }; f"
    sync = "!f() { for b in $@; do git fetch `git remote-for-branch $b` $b:$b; done }; f"
```

Now you can do this!

```
$ git sync master
$ git rebase master
```

Thanks to [this Stack Overflow answer][source] for the technique, which I just
wrapped up in an alias to make it easier to use.

[source]: http://stackoverflow.com/a/9753364/1004609
