---
title: How to extract an intention-revealing name using Vim
date: 2012-05-03 17:14
tags: code, vim
---

> This method has a [magic number][1]:
>
> ``` ruby
>    def wait_time
>      @env[QUEUE_WAIT_HEADER].to_i / 1000
>    end
> ```
>
> Let's extract that to an [intention-revealing name][2]. We'll type
>
> ``` vim
>    /1000<Enter>                           # Find the number we want to extract
>    cwmilliseconds_per_second<Esc>         # Replace the number with a variable name
>    O<Ctrl+A> = <Esc>p                     # Assign the replaced number to the variable
> ```
>
> The result:
>
> ``` ruby
>    def wait_time
>      milliseconds_per_second = 1000
>      @env[QUEUE_WAIT_HEADER].to_i / milliseconds_per_second
>    end
> ```
>
> thoughtbot, [How to extract an intention-revealing name using Vim][3]

Nice vim tip for getting rid of magic numbers in code.

[1]: http://c2.com/cgi/wiki?MagicNumber
[2]: http://c2.com/cgi/wiki?IdentifiersRevealIntent
[3]: http://robots.thoughtbot.com/post/22258289125/how-to-extract-an-intention-revealing-name-using-vim
