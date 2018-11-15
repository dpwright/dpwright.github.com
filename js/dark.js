document.addEventListener("DOMContentLoaded", function(event) {
  // Add the dark mode toggle button
  // (Hidden by default so as not to bother users with JS disabled)
  var toggle = document.getElementById("darktoggle");
  if(toggle) {
    toggle.innerHTML = "◑";
  }

  var darkMode = window.matchMedia('(prefers-color-scheme: dark)').matches;
  var altMode = darkMode ? "light" : "dark";

  var location = new URL(window.location);
  if(location.searchParams.has(altMode)) {
    // Set the body id to altMode
    document.body.id = altMode;

    // Find all internal links and add "?alt" to them
    var links = document.getElementsByTagName("a");
    var linksCount = links.length;
    var host = window.location.hostname;
    for(i = 0; i < linksCount; i++) {
      href = new URL(links[i].href);
        if(href.hostname == host) {
          links[i].href += "?" + altMode;
        }
    }

    // Set the toggle to turn alternate mode off
    toggle.href = "?";
  } else {
    // Set the toggle to turn alternate mode on
    toggle.href = "?" + altMode;
  }
});
