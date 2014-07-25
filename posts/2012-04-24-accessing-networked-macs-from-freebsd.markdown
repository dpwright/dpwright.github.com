---
title: Accessing networked Macs from FreeBSD
date: 2012-04-24 10:27
tags: .local , avahi , bonjour , freebsd , mac , networking , zeroconf
---

Macs on a network automatically find each other using [Bonjour][1], to avoid the
need to add them to the hosts file.  If you're trying to access a host that ends
in ".local" and it's not working, chances are you need to set up Zeroconf in
FreeBSD.  The open equivalent of Bonjour is [Avahi][2], which is available in
ports.

[This link][3] gives an easy guide to setting that up, so you can easily
communicate with other computers on the local network from your FreeBSD box.</p> 

[1]: http://en.wikipedia.org/wiki/Bonjour_(software)
[2]: http://en.wikipedia.org/wiki/Avahi_(software)
[3]: http://www.endeavoursofanengineer.com/blog/2010/05/08/installing-avahi-on-freebsd-2/
