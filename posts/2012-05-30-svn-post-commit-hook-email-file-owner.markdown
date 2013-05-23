---
layout: post
title: SVN post-commit hook: email file owner
date: 2012-05-30 14:36
comments: true
tags: bash, scripts, subversion
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

``` bash
#!/usr/bin/env bash

LOOK=/usr/bin/svnlook
REPOS="$1"
REV="$2"
PROJECT="$3"

AUTHOR=$($LOOK author $REPOS -r $REV)

DOMAIN=domain.com

OWNERS=""
for FILE in $($LOOK changed $REPOS -r $REV | awk '{ print $2 }')
do
        OWNER_LOG=$(svn log "file://$REPOS/$FILE" 2> /dev/null | grep "^r[0-9]* | " | awk '{ print $3 }' | sort | uniq -c | sort | tail -n 1 | awk '{ print $2 }')
        if [[ "$OWNER_LOG" != "$AUTHOR" && "$OWNERS" != *" $OWNER_LOG "* ]]
        then
                OWNERS="$OWNERS $OWNER_LOG "
        fi

        OWNER_BLAME=$(svn blame -x "-w --ignore-eol-style" "file://$REPOS/$FILE" 2> /dev/null | awk '{ print $2 }' | sort | uniq -c | sort | tail -n 1 | awk '{ print $2 }')
        if [[ "$OWNER_BLAME" != "" && "$OWNER_BLAME" != "$AUTHOR" && "$OWNERS" != *" $OWNER_BLAME "* ]]
        then
                OWNERS="$OWNERS $OWNER_BLAME "
        fi
done

if [[ "$OWNERS" != "" ]]
then
        RECIPIENTS=$(for PERSON in $OWNERS; do echo -n "--to $PERSON@$DOMAIN "; 
done)

        /usr/local/bin/svnnotify                          \\
            --repos-path     "$REPOS"                     \\
            --revision       "$REV"                       \\
            --subject-prefix "[$PROJECT-svn]"             \\
            --attach-diff                                 \\
            --diff-switches  '-x "-w --ignore-eol-style"' \\
            --user-domain    $DOMAIN                      \\
            $RECIPIENTS
fi
```

[1]: https://gist.github.com/2833953
