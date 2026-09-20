---
layout: post
title: Repositories
date: 2026-09-19 15:17
modified_date: 2026-09-20 00:26
categories: list
lang: en
---

## Main projects
- [`retype`](https://github.com/plu5/retype)

## Shelved projects
- [`eisenhour`](https://github.com/plu5/eisenhour)
  + js timer-based time-management application that syncs with google calendar, served me for half a decade. doesn't work anymore due to js ecosystem obsolescence and i quit using timers and calendars
- [`mwin`](https://github.com/plu5/mwin)
  + a tool for automatic resizing and repositioning of windows. there were many versions of it made before this one. it started as a clone of [grismar's shiftwindow](https://grismar.net/shiftwindow/) in cpp and vcl which i thought would take a few weeks, tried to improve the ui but only made it worse, got fed up with vcl and rewrote it in pure win32 (this is the version on the repo), moved os and lost motivation. i want to rewrite it in c and cross-platform and compatible back to win98, but this is not an important project in my life it is more like a délire

## Game mods
- [`p-borderlands`](https://github.com/plu5/p-borderlands)

## Game attempts
- [`cwebfps`](https://github.com/plu5/cwebfps) (raylib)
  + this is just one of the raylib demos that i simplify and got compiling for the web so even calling it an attempt is overselling it

## Text
- [`site`](https://github.com/plu5/plu5.github.io)
- [`wog-quiz-guide`](https://github.com/plu5/wog-quiz-guide) (steam guide)

## Small tools and code snippets
- [`tool`](https://github.com/plu5/tool)
- [`algs`](https://github.com/plu5/algs)

## Actions
- [`automatic-releases-with-sha-action`](https://github.com/plu5/automatic-releases-with-sha-action) (fork)
- [`macos-setup-python-action`](https://github.com/plu5/macos-setup-python-action)
- [`run-script-in-workflows-dir-action`](https://github.com/plu5/run-script-in-workflows-dir-action)
- [`cache-anything-new-in-container-action`](https://github.com/plu5/cache-anything-new-in-container-action) (fork)

## Emacs packages/themes
- [`emacs-fsff-theme`](https://github.com/plu5/emacs-fsff-theme)
- [`smart-mode-line-powerline2`](smart-mode-line-powerline2)
- [`braille.el`](https://github.com/plu5/braille.el)
- [`emacs-doentry`](https://github.com/plu5/emacs-doentry)

## Blender old abandoned shit
- [`p-layout-search`](https://github.com/plu5/p-layout-search)
- [`blender-scripts-and-configuration`](https://github.com/plu5/blender-scripts-and-configuration)

## Conf
- [`dotfiles`](https://github.com/plu5/dotfiles)
- [`emacsd`](https://github.com/plu5/emacsd)
- [`ublock-filters`](https://gist.github.com/plu5/84c50fd061f844210a7042ed3fc223a1)

## Misc customisation
- [`tree-style-tab-modo-theme`](https://github.com/plu5/tree-style-tab-modo-theme)
- [`pyhuntsman`](https://github.com/plu5/pyhuntsman)

## User scripts
- [`ghtextareas`](https://gist.github.com/plu5/dbbe0c3ba407c1e8ed432342edcd70e8) for github
- [`blwb`](https://gist.github.com/plu5/acac697b0bc172d4905179f284bffdf1) for bilibili

## Sandbox or experimentation
- [`testing-actions-github`](https://github.com/plu5/testing-actions-github) (fork)
- [`testing-staticx`](https://github.com/plu5/testing-staticx)
- [`git-filters-test`](https://github.com/plu5/git-filters-test)

## Contributions: Accepted
- imenu-list (emacs)
  + i still use that package daily so it's cool to have contributed a tiny thing, which was iirc just to prevent the face for nested entries from extending over the entire line instead of just the text
- bl-sdk.github.io
  + to add my borderlands mods to the registry
- ebooklib
  + just to fix a little warning. i am so proud to be in the list of authors and also in the list of projects that use ebooklib :-D

## Contributions: Rejected/ignored
- smart-mode-line (emacs)
  + options to make it possible to customise it to look more like powerline (like rearranging some elements)
- PythonSDK
  + attempts to improve the documentation, and an attempt to fix a mod that was included (the result of which was reminding the maintainers it even exists and resulting in its deletion...)
- file_watchtower
  + path handling on windows
- mpv-handler-queue ([`#1`](https://github.com/gabreek/mpv-handler-queue/pull/1))
  + a fix for newer versions of mpv where the format of the loadfile command changed

{% include fin.html %}
