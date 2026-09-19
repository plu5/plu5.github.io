---
layout: post
title: 29 — retype centreAroundCursor issues
date: 2026-09-19 01:52
modified_date: 2026-09-19 11:58
categories: retype
lang: en
redirect_from: /devlog/29
wip: true
---

## What
retype centres the view around the cursor with this simple function:
```python
def centreAroundCursor(self):
    # type: (BookDisplay) -> None
    viewport_height = self.viewport().rect().height()
    cursor_height = self.cursorRect(self._cursor).height()
    cursor_relative_y = self.cursorRect(self._cursor).y()
    scrollbar = self.verticalScrollBar()
    scrollbar.setValue(int(
        scrollbar.value() + cursor_relative_y -
        viewport_height/2 + cursor_height/2))
```
In rare cases, when this is called right after `setDocument`, the resulting position is too high.

Firstly, while investigating this I noticed that we can do a little optimisation in `gotoCursorPosition` to avoid calling `setChapter` if we are already on the right chapter. This is how I found it was related to `setDocument`, because now all the calls to `gotoCursorPosition` are fine if we are in the right chapter.

Once I find a position where it happens, I can reproduce it consistently there, but only with certain window and font sizes. It mostly happens when the cursor is positioned near the end of the document, around 75% or more of the way through. I thought there was a relation to the length of the document, like that it happens over a certain length, but that does not seem to be the case.

I also confirmed it is not to do with wordwrapping, as in one of my reproducing documents there is none (it's a table of contents).

Connecting the `centreAroundCursor` call to a 0 ms timer doesn't help, but 1 or more does resolve it. It introduces a little flash though, and it bothers me to have to do this every time when most documents are not affected by the bug.
```python
f = book_view.display.centreAroundCursor
f()                      # bug happens
QTimer.singleShot(0, f)  # bug still happens
QTimer.singleShot(1, f)  # bug "resolved"
```

Comparing at the values of various variables (viewport height, cursor rect, document size, viewport size, scrollbar values) between each of these calls, the only one I found that differs is `scrollbar.maximum()`. It is higher after 1 ms.

Could connect `centreAroundCursor` to the scrollbar's `rangeChanged`, but the range changes multiple times and it's not clear when to stop. It wouldn't satisfy me anyway, because I want to understand why. What it is about documents where it happens vs ones where it doesn't.

TODO debug with a breakpoint in a scrollbar range change event handler

TODO workaround for now: try checking scrollbar max and if it's higher than the value calculated schedule another call. this risks an infinite loop however

{% include fin.html %}
