---
title: "Bash oneliner: watch a filesize"
date: 2012-06-25 11:16
tags: bash, oneliners, shell
---

Sometimes, when running a long operation modifying a single file, I want to
monitor its progress by watching that file's size increase. Here's a simple
one-liner which will print the incrementing filesize until Ctrl-C is pressed:

``` bash
while true; do echo -e -n "\r`ls -l -h FILENAME | awk '{ print $5 }'`"; done
```
