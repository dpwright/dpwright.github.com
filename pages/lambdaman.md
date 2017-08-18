---
title: Lambdaman vs. The Centipede
---

This is an embedded player for the game implemented in ["Writing a ZX Spectrum
game in Haskell"][post].  Originally it was embedded within the blog post
itself, but as a number of people complained of a loud and unpleasant noise on
certain platforms, I've moved it out to a separate page.  Enjoy!

<script src="/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/jdataview.js"></script>
<script src="/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/jsspeccy-core.min.js"></script>
<script>
  var jsspeccy;
  var audioState = true;
  function go() {
    var scaleFactor = 1.5;
    var startResizingWidth = 700;
    var resizeFactor = startResizingWidth * (1.0 / scaleFactor);
    var windowWidth = window.innerWidth
    if(windowWidth < startResizingWidth) {
      scaleFactor = windowWidth / resizeFactor;
    }
    jsspeccy = JSSpeccy('speccy', {
      'autostart': false,
      'autoload': true,
      'scaleFactor': scaleFactor,
      'loadFile': '/posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell/lambdaman.tap'
    });
  }
  function toggleSound() {
    if(typeof jsspeccy !== 'undefined') {
      audioState = !audioState;
      jsspeccy.setAudioState(audioState);
    }
  }
</script>

<center><figure>
<div id="speccy"><img src onerror="go()" /></div>
<figcaption>Movement: QAOP/HJKL Fire: Space<br />
Powered by [jsspeccy]<br />
<a onClick="toggleSound()">Toggle Sound</a></figcaption>
</figure></center>

[post]:     /posts/2015/07/17/writing-a-zx-spectrum-game-in-haskell
[jsspeccy]: http://jsspeccy.zxdemo.org
