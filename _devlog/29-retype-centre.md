---
layout: post
title: 29 — retype centreAroundCursor issues
date: 2026-09-19 01:52
modified_date: 2026-09-29 20:36
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

## Workaround propositions
Could connect `centreAroundCursor` to the scrollbar's `rangeChanged`, but the range changes multiple times and it's not clear when to stop. It wouldn't satisfy me anyway, because I want to understand why. What it is about documents where it happens vs ones where it doesn't.

Try checking scrollbar max and if it's higher than the value calculated schedule another call? This risks an infinite loop however

or maybe just do the 1 ms delay because although it's unsatisfying it's the simplest way to fix it. and the cursor being centred on the wrong position is a lot more annoying than the flash. though we have to be subjected to the flash every time while the centring issue is somewhat rare...

I'm afraid that if I commit that I will never want to touch it again.

## Stepthrough debugging
Debugging with PDB with a breakpoint in a scrollbar range change event handler by adding the following to the end of BookDisplay's init:
```python
        from qt import QtCore

        def onRangeChanged(minimum, maximum):
            print(f"\n\n@@@@ scrollbar range\n{minimum} {maximum}")
            QtCore.pyqtRemoveInputHook()
            breakpoint()

        self.verticalScrollBar().rangeChanged.connect(onRangeChanged)
```

Comparison when opening a book where the problem occurs and a book where it doesn't

happens (document 1):

1. 0 37007
2. 0 5432
3. 0 5308
4. 0 6045

doesn't happen (document 2):

1. 0 31950
2. 0 32350
3. 0 28318

range changes only 3 times

Not reliably. Now when I open document 1, same position still, it only changes twice:

1. 0 5308
2. 0 6045

and the problem happens

document 2 is still:

1. 0 31950
2. 0 32350
3. 0 28318

and the problem doesn't happen

This doesn't really help me. And I can't tell what's provoking each call. The document size changes, but why this happens more times in some documents I can't tell.

*Does it* change more times? Let's check that.

Add at the end of BookView's setSource:
```python
        def onDocumentSizeChanged(size):
            print(f"\n\n@@@@ document size\n{size}")

        self.display.document().documentLayout().documentSizeChanged.connect(
            onDocumentSizeChanged)
```

document 1 (problem happens)

1. 0 5308
2. 0 6045
3. PyQt5.QtCore.QSizeF(613.0, 6645.0)

document 2 (problem doesn't happen)

1. 0 31950
2. 0 32350
3. PyQt5.QtCore.QSizeF(1556.0, 2636.0)
4. 0 28318
5. PyQt5.QtCore.QSizeF(1556.0, 28918.0)

The scrollbar range changes does not follow a document size change, it seems to be independent of it, though usually followed by it.

In document 1 centreAroundCursor gets called between 1 and 2 and that is why there is a problem, scrollbar max isn't right at that point. in document 2 it gets called between 3 and 4, so the scrollbar max changes again too, but gets lower rather than higher.

I'm no closer to solving this, I am only getting confused. It's also not very good data because I am only testing on two documents.

## Problem with timers
I was ready to give up and just add a 1 ms delay, but this doesn't resolve the problem in all cases. In my test document if I load it while I am in BookView already (on the same or another document), 1 ms does the job. If I am in ShelfView however, the problem still occurs. And that even with 2-9 ms. At 10 ms, it doesn't happen anymore. I was already unsatisfied with this solution, and now it's even more annoying.

maybe open an issue and ask for help

{% include fin.html %}
