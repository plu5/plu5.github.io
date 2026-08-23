---
layout: post
title: 27 — retype advance chapter crash investigation
date: 2026-08-15 17:47
modified_date: 2026-08-23 04:24
categories: retype debugging python
lang: en
redirect_from: /devlog/27
wip: true
---

## What
I discovered a bug in retype 1.7.1 today while typing a book I had not typed before. A crash after typing the last line of a chapter.

(A chapter here is referring to an individual HTML page in the epub)
```sh
[pm@pos retype]$ bin/retype
16:52:43.500 [root] INFO: retype 1.7.1
16:52:43.500 [retype.controllers.safe_config] INFO: Read config: /media/Windows/Users/pm/dev/retype/config.json
16:52:44.594 [retype.controllers.library] INFO: Read save: /media/Windows/Users/pm/dev/retype/save.json
16:53:36.895 [retype.controllers.library] INFO: Loading book 6: Aide-mémoire C++
16:53:36.895 [retype.controllers.library] INFO: Save data: None
16:55:44.58 [retype.ui.book_view] WARNING: line_pos out of range
[..same line 159 more times..]
16:55:44.162 [retype.ui.book_view] WARNING: line_pos out of range
Traceback (most recent call last):
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/console/highlighting_service.py", line 64, in _handleHighlighting
    self._maybeAdvance(v, text, not self.auto_newline)
    ~~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/console/highlighting_service.py", line 69, in _maybeAdvance
    if text == v.current_line or text == nrspacerstrip(v.current_line):
                                         ~~~~~~~~~~~~~^^^^^^^^^^^^^^^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/extras/space.py", line 56, in nrspacerstrip
    ns = nspacerstrip(s)
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/extras/space.py", line 50, in nspacerstrip
    ns = spacerstrip(s[0:-1])
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/extras/space.py", line 40, in spacerstrip
    ns = _spacerstrip(s)
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/extras/space.py", line 39, in _spacerstrip
    return s.rstrip().rstrip(''.join(effectively_space))
           ~~~~~~~~^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/extras/str_subclasses.py", line 345, in rstrip
    return self.strip(chars, directions=[-1])
           ~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/extras/str_subclasses.py", line 323, in strip
    new = deepcopy(self)  # type: UserString | str
  File "/usr/lib/python3.14/copy.py", line 157, in deepcopy
    y = _reconstruct(x, memo, *rv)
  File "/usr/lib/python3.14/copy.py", line 240, in _reconstruct
    state = deepcopy(state, memo)
  File "/usr/lib/python3.14/copy.py", line 131, in deepcopy
    y = copier(x, memo)
  File "/usr/lib/python3.14/copy.py", line 202, in _deepcopy_dict
    y[deepcopy(key, memo)] = deepcopy(value, memo)
                             ~~~~~~~~^^^^^^^^^^^^^
  File "/usr/lib/python3.14/copy.py", line 131, in deepcopy
    y = copier(x, memo)
  File "/usr/lib/python3.14/copy.py", line 202, in _deepcopy_dict
    y[deepcopy(key, memo)] = deepcopy(value, memo)
                             ~~~~~~~~^^^^^^^^^^^^^
  File "/usr/lib/python3.14/copy.py", line 157, in deepcopy
    y = _reconstruct(x, memo, *rv)
  File "/usr/lib/python3.14/copy.py", line 240, in _reconstruct
    state = deepcopy(state, memo)
  File "/usr/lib/python3.14/copy.py", line 131, in deepcopy
    y = copier(x, memo)
  File "/usr/lib/python3.14/copy.py", line 202, in _deepcopy_dict
    y[deepcopy(key, memo)] = deepcopy(value, memo)
                             ~~~~~~~~^^^^^^^^^^^^^
RecursionError: maximum recursion depth exceeded
Aborted                    (core dumped) bin/retype
```
The last line in that chapter is an image. `v.current_line` at this point is 0xFFFC (object replacement character).

## Searching for a simpler reproducer
I wonder if this would be reproductible with no matter which chapter that ends with an image. Many books have "chapters" that have only an image, like the title page, so if there is only an image in a chapter this doesn't happen, but situations where there is text and the last line is an image is what triggers it?

I think it's more likely it's nothing to do with the context and more with the contents of this individual line, because in the stacktrace it's ManifoldStr's implementation of `strip` that is causing infinite recursion.

```python
print(f'@@ "{v.current_line}"')
# ->
@@ "￼ 
"
```
It seems that there is not only an object replacement character, but also a space and line feed.

A ManifoldStr is constructed with a normal string and a replacements dictionary. The replacements are substrings that will be considered equivalent to a given substring of the same length. A test could look like this:
```python
    def test_strip_space_fffc(self):
        ms = ManifoldStr('￼ \r', {'\ufffc': ' '})
        assert ms.strip() == '\r'
```
Sadly, it passes, it does not reproduce the bug.

Maybe it's not exactly the same string. We could try to make the debug print display the escaped unicode characters.
```python
print(f'@@ "{str(v.current_line.encode("unicode_escape").decode("latin1"))}"')
# -> @@ "\ufffc \n"
```
Let's try this then:
```python
    def test_strip_space_fffc(self):
        ms = ManifoldStr('\ufffc \n', {'\ufffc': ' '})
        assert ms.strip() == '\n'
```
Still passes with no issues.

Looking again at the stacktrace, what actually gets called is `s.rstrip().rstrip(''.join(effectively_space))`, where `effectively_space` is:
```python
# Characters that are effectively space but not detected by isspace
effectively_space = ['\ufeff', '\u180e', '\u200b', '\u000a']
```
None of these characters are present in our case.

If I take the previous test and replace strip by rstrip, it no longer passes since there are not spaces on the right, but it doesn't trigger infinite recursion either. And this passes with no issues:
```python
    def test_strip_space_fffc(self):
        ms = ManifoldStr('\ufffc \n', {'\ufffc': ' '})
        assert ms.rstrip() == '\ufffc \n'
```
And even `ms.rstrip().rstrip()`. And just to verify the `effectively_space` characters don't have an impact here:
```python
    def test_strip_space_fffc(self):
        ms = ManifoldStr('\ufffc \n', {'\ufffc': ' '})
        from retype.extras.space import effectively_space
        assert ms.rstrip().rstrip(''.join(effectively_space)) == '\ufffc \n'
```
Passes.

Looking a bit further up the stacktrace: `ns = spacerstrip(s[0:-1])`. So it is not the entire `v.current_line`. Let's add a print above the `s.rstrip().rstrip(''.join(effectively_space))` to see exactly what the strip is operating on.
```python
print(f'@@ "{str(s.encode("unicode_escape").decode("latin1"))}"')
```
I see repetitively this over each one of the 161 warnings:
```python
@@ "\ufffc \n"
@@ "\ufffc "
@@ ""
20:04:36.703 [retype.ui.book_view] WARNING: line_pos out of range
@@ "\ufffc \n"
@@ "\ufffc "
@@ ""
20:04:36.705 [retype.ui.book_view] WARNING: line_pos out of range
@@ "\ufffc \n"
@@ "\ufffc "
@@ ""
20:04:36.706 [retype.ui.book_view] WARNING: line_pos out of range
..
```

With added prints at the head of each concerned function, and running with `-l DEBUG`:
```python
f: HighlightingService.advanceLine
f: BookView._setLine
00:24:31.456 [retype.ui.book_view] DEBUG: Skipping empty line
f: HighlightingService.advanceLine
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
f: BookView._setLine
00:24:31.487 [retype.ui.book_view] WARNING: line_pos out of range
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
f: BookView._setLine
00:24:31.489 [retype.ui.book_view] WARNING: line_pos out of range
f: HighlightingService._handleHighlighting
..
```
So it is not spacerstrip that causes the infinite recursion, which I was tempted to think, because it's got a while loop.

`_handleHighlighting` doesn't recurse. It gets triggered when text changes in the console input. So why does it get called repetitively here?

Book view's `_setLine` function:
```python
    def _setLine(self, pos):
        # type: (BookView, int) -> None
        if self.tobetyped_list:
            if self.line_pos is not None and \
               self.line_pos > len(self.tobetyped_list):
                return logger.warning("line_pos out of range")
            if self.rdict:
                self.current_line = ManifoldStr(
                    self.tobetyped_list[pos],
                    self.rdict)  # type: str | ManifoldStr
            else:
                self.current_line = self.tobetyped_list[pos]

            if isspaceorempty(self.current_line):
                logger.debug("Skipping empty line")
                self.advanceLine()
        else:
            logger.error("Bad tobetyped_list; {}".format(self.tobetyped_list))
```
In our recursion hell this just returns without doing anything, because line pos is out of range.

`advanceLine`:
```python
    def advanceLine(self):
        # type: (HighlightingService) -> None
        v = self.book_view

        if v.cursor_pos is None or v.persistent_pos is None or \
           v.line_pos is None:
            logger.error('_advanceLine: Unexpected None. cursor_pos: '
                         f'{v.cursor_pos}, persistent_pos: {v.persistent_pos},'
                         f' line_pos: {v.line_pos}')
            return

        # Get out of here if there is no line to advance to
        if v.onLastChapter():
            if len(v.tobetyped_list)-1 == v.line_pos:
                v.markComplete()
                return logger.debug("On last line, marking complete")
            elif len(v.tobetyped_list)-1 < v.line_pos:
                return logger.error("line_pos ({}) larger than the list ({})\
 for some reason  ".format(len(v.tobetyped_list), v.line_pos))

        v.line_pos += 1

        # Compensate
        len_typed = v.cursor_pos - v.persistent_pos
        difference = len(v.current_line) - len_typed
        v.cursor_pos += difference

        v.persistent_pos = v.cursor_pos

        # Reached last line of this chapter, move to next one
        if len(v.tobetyped_list) == v.line_pos:
            v.nextChapter(True)

        # Set the line that needs to be typed next
        try:
            v._setLine(v.line_pos)
        except Exception as e:
            logger.error('can’t advance line {}/{}\n\
error: {}'.format(v.line_pos, len(v.tobetyped_list), e))
            return

        self.updateHighlighting()
        self._console.clear()
        v.display.centreAroundCursor()
        v.updateProgress()
```
I guess the problem is that `len(v.tobetyped_list) == v.line_pos` doesn't trigger?

line pos keeps incrementing
```python
@@line pos 10
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
f: BookView._setLine
17:52:39.853 [retype.ui.book_view] WARNING: line_pos out of range
@@line pos 11
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
f: BookView._setLine
17:52:39.854 [retype.ui.book_view] WARNING: line_pos out of range
@@line pos 12
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
f: BookView._setLine
17:52:39.855 [retype.ui.book_view] WARNING: line_pos out of range
@@line pos 13
```
and the end of advanceLine is also never reached, I had to put this print before `self._console.clear()` or it is never called. The clear is a console text change which triggers `_handleHighlighting` which triggers `_maybeAdvance`.

`_maybeAdvance` sees that the line is "empty" (equivalent to just spaces) and advances to the next line. Because line pos is already beyond the available lines in the chapter, it keeps going to the "next line" indefinitely.

`advanceLine` should return without doing anything and announce the problem if line pos is too large.
```python
        # Get out of here if line pos is out of range (this shouldn't happen)
        if len(v.tobetyped_list) < v.line_pos:
            return logger.error(f'line pos too large {v.line_pos}'
                                f' (range {len(v.tobetyped_list)})')
```
This is just bandaid, we need to understand why line pos is too large to begin with.

I expected it to get stuck in place after this, but it actually advances to the next chapter.
```python
@@line pos 7
f: HighlightingService.advanceLine
f: BookView._setLine
18:15:46.86 [retype.ui.book_view] DEBUG: Skipping empty line
f: HighlightingService.advanceLine
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
f: BookView._setLine
18:15:46.100 [retype.ui.book_view] WARNING: line_pos out of range
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "\ufffc \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "\ufffc "
f: space._spacerstrip
@@ ""
f: HighlightingService.advanceLine
18:15:46.101 [retype.console.highlighting_service] ERROR: line pos too large 10 (range 9)
@@line pos 10
f: BookView._setLine
f: BookView._setLine
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "Table des mati\xe8res \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "Table des mati\xe8res "
f: space._spacerstrip
@@ "Table des mati\xe8res"
@@line pos 0
f: HighlightingService._handleHighlighting
f: HighlightingService._maybeAdvance
@@ "Table des mati\xe8res \n"
f: space.nspacerstrip
f: space.spacerstrip
f: space._spacerstrip
@@ "Table des mati\xe8res "
f: space._spacerstrip
@@ "Table des mati\xe8res"
@@line pos 0
```
I'm confused why we get the "@@line pos 10". The print is after the return so it should not be reached. Something else calls advanceLine after?

## pdb with PyQt
This is getting beyond my ability to debug with just print statements, let's try [pdb](https://docs.python.org/3/library/pdb.html).

I added `breakpoint()` after the line pos print. PyQt really doesn't like it and spams `QCoreApplication::exec: The event loop is already running`.

(after this happens you can enter `quit` and `y` to get out; panicked keyboard interrupts will get you nowhere)

> A practical side problem comes up when you are trying to debug using pdb: the Qt main loop is hooked into the pdb prompt, which makes it so that you are trying to re-enter the main loop when you do something like `pdb.set_trace()`. You will get an error like "QCoreApplication::exec: The event loop is already running". To be able to use pdb, you can wrap it in `QtCore.pyqtRemoveInputHook()` and `QtCore.pyqtRestoreInputHook()`, which will let you disconnect Qt from the input loop, freezing it and letting you debug.  
—[*Azendale, 2013*](https://stackoverflow.com/a/16661987/18396947)

It works, but I'm not sure where I should be restoring the hook. If I'm only looking at the breakpoint then I can restore it after, but if I want to continue stepping through then the same problem occurs.

Ended up just not restoring it

After some time spent stepping through, I think the line pos is correct when we reach the last line, and the real problem then is-- well clearly not because `len(v.tobetyped_list) == v.line_pos` would have passed and it would have moved on to the next. how come that doesn't happen? somehow it skips two? something to do with the last line being "empty"? ("Skipping empty line" logic)

```python
(Pdb) str(v.current_line)
'￼ \n'
(Pdb) v.line_pos
8
(Pdb) len(v.tobetyped_list)
9
(Pdb) v.tobetyped_list[-1]
'￼ \n'
```

(it's one less but that's normal since len is 1-based, and the `len(v.tobetyped_list) == v.line_pos` only happens after line pos is incremented)

... it does actually pass that and nextChapter gets called. but then advanceLine gets called again
```python
-> v.nextChapter(True)
  /media/Windows/Users/pm/dev/retype/retype/ui/book_view.py(566)nextChapter()
-> self.setChapter(pos, move_cursor)
  /media/Windows/Users/pm/dev/retype/retype/ui/book_view.py(526)setChapter()
-> self._controller.console.clear()
  /media/Windows/Users/pm/dev/retype/retype/console/console.py(97)clear()
-> super().setText('')
  /media/Windows/Users/pm/dev/retype/retype/ui/line_edit.py(73)setText()
-> self.edit.setPlainText(text)
  /media/Windows/Users/pm/dev/retype/retype/ui/line_edit.py(82)_emitTextChanged()
-> self.textChanged.emit(text)
  /media/Windows/Users/pm/dev/retype/retype/console/highlighting_service.py(65)_handleHighlighting()
-> self._maybeAdvance(v, text, not self.auto_newline)
  /media/Windows/Users/pm/dev/retype/retype/console/highlighting_service.py(75)_maybeAdvance()
-> self.advanceLine()
```

But despite nextChapter being called, we are still on the same line, and the same `tobetyped_list`
```python
(Pdb) str(v.current_line)
'￼ \n'
(Pdb) v.tobetyped_list[-1]
'￼ \n'
```

Is setChapter not supposed to change this?

Yes, but it does console clear first. Which triggers handleHighlighting, which triggers maybeAdvance, which triggers advanceLine, and the line is still the same.

And advanceLine also clears the console at the end, we have a lot of excess handleHighlighting calls here, we should maybe have a way to clear the console that doesn't trigger anything. But I think this is just inefficiency rather than the crux of the issue. Or is it? It could be exactly the crux of the issue.

That line in setChapter was added recently by the way, in [c3d50d4](https://github.com/plu5/retype/commit/c3d50d47b1a487847c36fe3d9458dfac90e4d40f) from June for release 1.7.0. This was due to problems with console commands that change cursor position, if there is text in the console it can cause cursor position desync.

But if we clear console without updating highlighting, couldn't it cause other problems? In some cases yes, but when we do clear at the end of advanceLine or setChapter, it is superfluous at best to call it again.

As for why we saw the "@@line pos" print even after returning, it's because there are nested calls to advanceLine here, it recurses because of it clearing console that triggers updateHighlighting again.

That also explains why after our patch the chapter advanced; although console clearing causes advanceLine to get called again and line pos to get incremented, we're eventually back to the setChapter and it resets everything and goes to the next chapter. At least I think that's the reason, it's really confusing.

## Empty last line highlighting service unit test
Let's remove the "Get out of here if line pos is out of range" patch for now so that we get infinite recursion again, and try to write a test that reproduces it. Based on all of the above, it should be a chapter with the last line "empty or space".

In `test_highlighting_service.py`, there is a test like this:
```python
    def test_skipEmptyLines(self):
        (console, _, service, cursor) = _setup(SAMPLE_CONTENT2)

        console.setText("")
        assert cursor.position() == 1

        t = "begins with an empty line"
        console.setText(t)
        assert cursor.position() == 1 + (len(t) - 1) + 3
```
where `SAMPLE_CONTENT2` is:
```python
SAMPLE_CONTENT2 = '''<html><body><br/>
begins with an empty line
<span>  </span><br/>
followed by a line of just spaces</body></html>'''
```
so to start with, I attempted:
```python
    def test_ends_with_empty_line(self):
        SAMPLE = '''<html><body>ends with an empty line<br/>
<span>  </span></body></html>'''
        (console, v, service, cursor) = _setup(SAMPLE)

        assert len(v.tobetyped_list) == 2
        assert v.tobetyped_list[0] == "ends with an empty line\n"
        assert v.tobetyped_list[1] == "  \n"
```
The last assert doesn't pass, the last line is `"\n"` instead of `"  \n"`

Let's instead make the last line match our real-life situation. Also going a bit further with the test:
```python
    def test_ends_with_empty_line(self):
        SAMPLE = '''<html><body>ends with an empty line<br/>
<span>\ufffc \n</span></body></html>'''
        (console, v, service, cursor) = _setup(SAMPLE)

        assert len(v.tobetyped_list) == 2
        assert v.tobetyped_list[0] == "ends with an empty line\n"
        assert v.tobetyped_list[1] == "\ufffc \n"

        console.setText("")
        assert cursor.position() == 0
        assert v.chapter_pos == 0
        assert v.line_pos == 0

        console.setText("ends with an empty line")
        assert v.chapter_pos == 1, "chapter pos should advance"
```
The last assert fails. But this could be due to issues with my testing environment, I am actually using a mock book view, and none of my other tests thus far have advancing chapter, they all take place in a single chapter. But it doesn't appear nextChapter even gets called.

I can use the debugger again. Add a breakpoint at the top of advanceLine:
```python
        from qt import QtCore
        QtCore.pyqtRemoveInputHook()
        breakpoint()
```
and run only the particular test: `pytest -s tests/test_highlighting_service.py -k test_ends_with_empty_line`

Step through using `n`, see stack using `w`, go up and down the stack with `d` and `u`, and you can run any python expression to verify the value of things. See [pdb docs](https://docs.python.org/3/library/pdb.html) for more.

`console.setText("ends with an empty line")` triggers textChanged, which triggers handleHighlighting, which calls maybeAdvance, which calls advanceLine. Line pos increases, ~~clearing console triggers handleHighlighting again, which calls maybeAdvance again. But it doesn't call advanceLine this time. Why not?~~

I'm actually mocking the console also and the mock's clear function does nothing. We can't reproduce the bug with this.

We need a test that uses the real console. I'm sure there was a reason why I did it this way, and that I'm about to enter into a world of pain again.

Or not. I just tried the same setup except the real console instead of FakeConsole with no ill effects. It's unclear why I mocked it in the first place. All the existing tests pass. It's supposed to be a unit test though and it could still be that something is subtly broken that would make these tests not fail as expected when they should, so I will leave it as is and put my highlighting+console tests in a separate file, `tests/test_highlighting_plus_console.py`.

With the same `test_ends_with_empty_line` as before, now with the real console, the result is the same, the last assert doesn't pass, and nextChapter never gets called. I still have a fake BookView and a fake book, and in this fake book there is only one chapter, so it could be that which is preventing it from being called.

TODO

{% include fin.html %}
