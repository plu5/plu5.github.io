---
layout: post
title: 31 — retype line edit UX
date: 2026-09-22 13:12
modified_date: 2026-09-24 15:53
categories: retype
lang: en
redirect_from: /devlog/31
wip: true
---

## What
This is actually the issue I was referring to at the end of devlog 28. I noticed it while testing the builds for 1.7.2 and after I saw it I couldn't understand how I didn't notice it before.

There are UX problems with the line edits in the customisation dialog library paths widget. The PathDelegate editors.

1. **Width issues:** Instead of truncating the text, the editor grows in width to fit it, so if there is a long path the browse button goes out of view (hard to notice, but it doesn't clip; there is a horizontal scrollbar).
2. **Invisibility:** This issue doesn't appear on the build, but when running from sources, when you activate the editor the text behind the editor is still visible, i.e. the background of the editor is invisible, you can see through it, thus leading to two texts overlapping and hard to see what you're editing. We can't/shouldn't hardcode a background colour, so maybe just hide the thing behind it when it's active?
3. **Manual confirm:** After selecting a path with the browse button, you have to press enter to confirm and exit out of the editor in order for the path to actually be added and to be able to save configuration. It would be better if it confirms automatically after browse.

{% include fin.html %}
