--
layout: post
title:  "Reson devlog"
date:   2025-02-13 03:37
modified_date: 2022-05-08 22:21
categories: devlog
---
This is going to be a devlog of reson, which I want to get on with making. It will be a cross-platform audio player with simultaneous audio streams capability, and also the ability to keep track of and play from rss feeds (podcasts!) in C++/Qt.

Qt has frustrated me in past projects; the locked-in feeling and having to work around bugs and find alternative ways of doing what you're trying to do. You can't do what you want directly, because you're not in control of that part of the code. It is really annoying when maintaining a project and there is a long-standing bug you want to fix but all you can do is try to find a workaround. I have mostly used it from Python — is there any greater freedom in C++? I expect not. It is probably an inescapable part of using any framework. Even if it were open licence to modify it for your purposes, you'd probably still not want to do it other than as a last resort, as without some sort of ability to externally patch it, you'd have to distribute your own version of the framework.

Given its wide use in the Linux ecosystem, and the ease with which it allows you to make cross-platform applications, I think Qt is the best choice, bar writing lower level code myself for higher performance applications more adapted to their use with the freedom to change anything. I have experience with writing the latter for Windows with its 90s API—which was really fun and I enjoyed (would have been cool if I had made a devlog... and there is rather a lot to write about all the pitfalls)—, but not for Linux and certainly not macOS. I do want to try it in future, but probably with a simpler project. With this one I want to get on as fast as possible to a working prototype, because I need this software for my own use. There is also, I expect, a lot more for me to learn about Qt, and it might help me understand it better, which would be useful for my other Qt projects. I do not want to sound ungrateful!

That is too much talking. It's time to get on with it.

Blank window + create a repo.

Next, get it to play a hardcoded audio file upon launch.