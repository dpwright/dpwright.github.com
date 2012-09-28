---
layout: post
title: "Bash oneliner: watch a filesize"
date: 2012-06-25 11:16
comments: true
categories:
  - bash
  - oneliners
  - shell
---

Sometimes, when running a long operation modifying a single file, I want to
monitor its progress by watching that file's size increase. Here's a simple
one-liner which will print the incrementing filesize until Ctrl-C is pressed:

{% gist 2986038 filesize.sh %}
