---
layout: post
title:  "Devlog: Making an existing React app into a browser extension"
date:   2025-06-14 08:08
modified_date: 2025-06-14 18:32
categories: devlog web
---

Writing through my attempts to make my React (and Node.js backend) time-management application, [Eisenhour](https://github.com/plu5/eisenhour), into a WebExtensions extension, which I want to do to make it more convenient to use.

## Day 1: Developing a basic extension separately
A bit worried to start with because I don't know if something about Eisenhour makes it impossible to function as an extension. In fact, it doesn't do anything complex server-side, it's just timers that get synced with Google Calendar as calendar events.

My first idea then to test the feasibility of this is to try to make an extension that syncs something to Google APIs. Idea: A sort of bookmark-cataloguing service where the data gets saved to a Google Sheet. I think that would be quite useful in itself, for example saving interesting Wikipedia articles or StackExchange questions, with the ability to store some data about each link and do data analysis on the sheet afterwards.

I started with MDN [your first extension](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_first_WebExtension) and [your second extension](https://developer.mozilla.org/en-US/docs/Mozilla/Add-ons/WebExtensions/Your_second_WebExtension). The first one is barely any code so a good place to start, and the second one sets up a button and panel so it's useful, but it's a really contrived example for an extension, I have to say ("beastify").

I'm pleasantly surprised that you don't have to restart the browser to test your extension. It gets loaded from `about:debugging#/runtime/this-firefox` → `Load Temporary Add-on...` with your extension's manifest (or another file in the folder where it's located), and can be refreshed with the little "Reload" button underneath it after each time you make changes. You can also set up hot-reloading with [`web-ext`](https://extensionworkshop.com/documentation/develop/getting-started-with-web-ext/) but I don't care enough at the moment to take the time to do this. It might make it easier later to build or something, so leaving a note.

Now I have an extension that is just toolbar button that opens a panel. It's just two files: the manifest, and an html page for the panel (the "popup").

## Day 2: Putting a React app in the extension panel
The next step is to make the panel be a React app.

{%comment%}{%raw%}
{% include figure.html file="p/" %}
{%endraw%}{%endcomment%}

{% include fin.html %}