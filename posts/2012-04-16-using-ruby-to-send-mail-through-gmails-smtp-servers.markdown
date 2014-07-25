---
title: Using Ruby to send mail through GMail's SMTP servers
date: 2012-04-16 09:59
tags: gmail, ruby, scripts, smtp
---
A few days ago, I ran into a problem.  I have a script set up on my Dreamhost
server which occasionally sends me notification emails.  It had been running
fine for a couple of weeks, but suddenly they stopped coming through.  I checked
the logs and saw I'd been getting an "Access Denied" error from the SMTP
server:

> 554 5.7.1 &lt;mail@address.com&gt;: Recipient address rejected: Access denied

Up until this point I'd simply been using 'localhost' as my SMTP server, but I
guess they might not like me doing that.  I'm not sure whether the servers they
use to manage their users' email are the same as their hosting servers, and
either way I'd switched to using Google Apps to manage my email years ago.  The
easiest solution, it seemed, was to switch to using Google's SMTP server.

Just switching the server addresses and adding username/password info wasn't
enough though -- the connection was refused:


> Connection refused - connect(2) (Errno::ECONNREFUSED)

This is because Google mail only supports TLS/SSL connections (quite sensibly). 
Unfortunately, it seems the version of Ruby installed on Dreamhost's servers by
default (1.8.7) doesn't support TLS connections out of the box.

Thankfully, there's a gem for that!  If you <code>gem install tlsmail</code>,
the enable_tls function becomes available and you can communicate with Google
Mail's servers.

I did all this, but there was still one gotcha.  Using my previous code directly
but replacing the servers with Google's resulted in the following error:

> 555 5.5.2 Syntax error. yw3sm17774749obb.7 (Net::SMTPFatalError)

Note that this is an SMTP error, rather than a Ruby one.  A little googling
turned up [this thread](http://www.ruby-forum.com/topic/185075), which suggested
that the latest ruby version automatically adds angled brackets to the email
address you supply to Net::SMTP's send_message function.  I hadn't upgraded my
Ruby version, but I had made use of the tlsmail gem, so maybe that had done it.

I modified the call so that it specified the email address alone, without the
name or angled brackets.  I left the headers in the message itself intact.  It
worked!  Here's the resulting code:

``` ruby
#!/usr/bin/env ruby

require 'rubygems'
require 'tlsmail'

msg=<<EOF
From: Test Sender <sender@domain.com>
To: Test Recipient <recipient@domain.com>
Subject: test

This is a test
EOF

Net::SMTP.enable_tls(OpenSSL::SSL::VERIFY_NONE)
Net::SMTP.start('smtp.gmail.com', 587, 'domain.com', 'sender@domain.com', 'password', :login) do |smtp|
        smtp.send_message msg, "sender@domain.com", "recipient@domain.com"
end
```

Note this works with mail provided by Google Apps as well as plain vanilla
Google Mail. In the latter case, specify "gmail.com" as the domain, otherwise
use your own.
