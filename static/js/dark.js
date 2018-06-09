document.addEventListener("DOMContentLoaded", function(event) {
  // Add the dark mode toggle button
  // (Hidden by default so as not to bother users with JS disabled)
  var toggle = document.getElementById("darktoggle");
  if(toggle) {
    toggle.innerHTML = "◑";
  }

  var location = new URL(window.location);
  if(location.searchParams.has("dark")) {
    // Set the body id to "dark"
    document.body.id = "dark";

    // Find all internal links and add "?dark" to them
    var links = document.getElementsByTagName("a");
    var linksCount = links.length;
    var host = window.location.hostname;
    for(i = 0; i < linksCount; i++) {
      href = new URL(links[i].href);
        if(href.hostname == host) {
          links[i].href += "?dark";
        }
    }

    // Set the toggle to turn dark mode off
    toggle.href = "?";
  } else {
    // Set the toggle to turn dark mode on
    toggle.href = "?dark";
  }
});
