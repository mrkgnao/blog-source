---
layout: post
header: Stupid Heroku log parsing with zsh, sed, cut, and friends
title: Stupid Heroku log parsing with zsh, sed, cut, and friends
lead: A few days back, I posted Refinator to /r/math and Hacker News and I got a pretty huge uptick in visitors. Here's a quick-and-dirty solution.
tags: unix, zsh, short
---

The standard Heroku log format looks like this:

    2016-12-24T17:11:32.675741+00:00 heroku[router]: at=info method=GET path="/" host=refinator.herokuapp.com request_id=0e3564cc-63df-4866-9dbe-638593bbf8dc fwd="203.0.113.0" dyno=web.1 connect=1ms service=401ms status=200 bytes=27075

(No, [that's not your IP](https://en.wikipedia.org/wiki/Reserved_IP_addresses)[^mr].)

After trying to wrangle something with `jshon` and other things, I came up with the idea of misusing the `eval` shell builtin. In short, if you do something like

    test_str='a=5 b="6"'
    eval $test_str
    
then `5` is assigned to `a`, and so on. You will notice that the log format is almost perfectly suited to this, barring the UTC timestamp and the name of the source of the log entry (`heroku[router]`) in the example.

[^mr]: I learned this from reddit discussions of the first Mr. Robot episode.
