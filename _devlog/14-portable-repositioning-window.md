---
layout: post
title: 14 — Portable repositioning window
date: 2026-04-16 15:37
modified_date: 2026-04-18 05:18
categories: xcb win32
lang: en
redirect_from: /devlog/14
wip: true
---

## Linux - X
There are two options for X, Xlib (sometimes called just X11) and XCB. The former is older and written by hand, the latter is generated from the specs. Technically there could be others, because rather than a C API like in win32, the X server uses a [network protocol](https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html), and the aforementioned are like C bindings for it.

XCB is preferred:
It's more verbose / a little bit harder to get started, but it gives you more control, ability to make non-blocking requests (the gains in speed can be enormous when you need to send many requests at once), and has a more logical and consistent interface (less idiosyncrasies).

TODO maybe show the same thing in both of them

I stayed up late reading [this xcb tutorial](https://www.x.org/archive/X11R7.5/doc/libxcb/tutorial/index.html)

I have experience in win32 and it seems pretty familiar to me, other than
- having to pass a connection object around
- having to subscribe to events you want rather than receiving all of them and deciding which ones to handle (the tutorial made a point of how commonly people forget this, and indeed in my searches I came across several people confused by why they are not receiving an event)
- (there is also the huge difference that there are no "base controls" to use like a button, text edit, list view)

[This series of articles by Chuan Ji](https://jichu4n.com/posts/how-x-window-managers-work-and-how-to-write-one-part-i/) about window managers is also useful. Notes:
- The difference between X and macOS and Unity is exaggerated, it's not really true that a program couldn't function independently of the window manager, menus are not a critical function.
- First diagram: There doesn't have to be a GUI framework between X and an application.
- "In X terminology, all top level windows and all UI elements within are _windows_. In other words, a _window_, is any rectangular area that is an unit of user interaction and/or graphic display." -- this is the same in win32.
- There was meant to be more but he abandoned after part 3 (but there was a gap of 3 years between part 2 and 3, so maybe? but it's been 9 years since then)

## Linux - Wayland
maybe impossible?

## Windows

tested on windows 10 and windows 98

## macOS

{% include fin.html %}
