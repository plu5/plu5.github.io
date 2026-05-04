---
layout: post
title:  "Attempt to make Eisenhour into a browser extension"
date:   2025-02-12 21:02
modified_date: 2022-05-08 22:21
categories: devlog
---
This devlog is going to document my attempts to make an existing React + Node.js application ([Eisenhour](https://github.com/plu5/eisenhour)) into a browser extension. Ideally I want to put it in the same repo and for it to still be possible to use it the way it currently is (by running a server and client yourself).

It should be mentioned I am not 100% sure it's possible, this might just end in failure. I am not going to host a server online, so for this to work, the server-side has to be possible to do in the extension somehow.

I'll start by making a new branch for it