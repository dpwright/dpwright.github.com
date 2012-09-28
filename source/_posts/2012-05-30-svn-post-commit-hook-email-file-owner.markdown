---
layout: post
title: "SVN post-commit hook: email file owner"
date: 2012-05-30 14:36
comments: true
categories:
  - bash
  - scripts
  - subversion
---

It's a common enough situation: we want SVN to email a notification to
developers when somebody modifies a file they are in control of. SVN post-commit
hooks let us send an email when the file is modified; but how do we determine to
whom that email should be sent?

I came up with two solutions. Neither is very clever, but they both give a
little clue at least. The first looks through the log and determines who has
committed changes to that file the greatest number of times. The second looks at
the blame log for the file to determine who has modified the most *lines* in the
file. My script just emails both people

Just copy the script below and call it from your `post-commit` file to get
started. You'll want to change the value of DOMAIN to match the domain you want
to email notifications to. The third parameter is the name of the project, so
you can use the script for multiple repositories.

I just threw this script together so it's nothing special -- if you'd like to
make improvements why not [fork the gist][1]?

{% gist 2833953 svn-notify-hook.sh %}

[1]: https://gist.github.com/2833953
