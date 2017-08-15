---
title: "OGRE #include tree"
date: 2012-04-10 10:54
tags: c++, graphs, ogre, visualisation
---

<div class="page"><div>![OGRE include tree](ogre-include-tree/ogre-includes-th.png)</div></div>

My [include graph generator script][1] being run on ogre.h from the [OGRE][2]
rendering engine.  The graph is far too complicated to read, but idly scrolling
around is quite mesmerising.  These files are ALL included the moment you type
`#include <OGRE/Ogre.h>`.

This isn't really a criticism of the design of OGRE (which is actually not all
that bad), just a general interest thing.  Of course you can avoid including
quite so many files by only including the specific classes you need.  With some
libraries, that doesn't help you much.

(Full resolution image [here][3])

[1]: https://github.com/vitei/generate-include-graph.rb
[2]: http://www.ogre3d.org/
[3]: ogre-include-tree/ogre-includes.png
