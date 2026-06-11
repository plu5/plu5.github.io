---
layout: post
title: 19 — retype customisable keyboard shortcuts
date: 2026-05-20 12:57
modified_date: 2026-06-05 06:51
categories: retype
lang: en
redirect_from: /devlog/19
---

## Existing shortcuts
retype has some keyboard shortcuts hardcoded in `menu_controller.py`:
- Alt+F4 : Quit
- Ctrl+1 : Switch to Shelf View
- Ctrl+2 : Switch to Book View
- Ctrl+O : Show Customisation Dialog

and also in `book_view.py`:
- [QKeySequence.StandardKey](https://doc.qt.io/archives/qt-5.15/qkeysequence.html#standard-shortcuts).ZoomIn : Zoom in
- QKeySequence.StandardKey.ZoomOut : Zoom out

I would like to make them customisable.

## Qt shortcuts editor
In PyQt adding a keyboard shortcut is just a matter of calling setShortcuts on a QAction, where the shortcuts are a list of strings, like `['Ctrl+1']`.

(or QKeySequence.StandardKeys, or Qt.Key keycodes)

Is there also a widget where you can assign them by pressing a key combination on the keyboard? Otherwise I could use an abstract list model and delegate where the user can input a comma-separated list, like for the replacements customisation. I do something like this already for the stenography game customisation (kdict). Actually I think I'm going to have to do that one way or another, if there is a keyboard bindings widget it would just be the editor the delegate creates.

[Qt6 Shortcut Editor Example](https://doc.qt.io/qt-6/qtwidgets-widgets-shortcuteditor-example.html) uses QKeySequenceEdit. [It existed also in Qt5](https://doc.qt.io/archives/qt-5.15/qkeysequenceedit.html).

## Decorator for each widget to define its shortcuts
I had the thought that defining the shortcuts could be done with a decorator like how I do theming, that each widget can define its own bindable commands and defaults for them, since any widget could create a QAction and define a shortcut for it, it's not limited to the menu.

This is what the theme decorator looks like:
```python
def theme(selector, props_obj):
    # type: (str, C) -> Callable[[type[T]], type[T]]
    def decorator(cls):
        # type: (type[T]) -> type[T]
        props_obj.name = selector
        Theme.selectors[selector] = props_obj
        return cls
    return decorator
```
In use:
```python
@theme('BookView.BookDisplay',
       C(fg='black', bg='#F6F1DE', sel_bg='grey', t_border='white',
         b_border='white', l_border='white', r_border='white'))
@theme('BookView.BookDisplay.Cursor', C(fg='red'))
class BookDisplay(QTextBrowser):
    keyPressed = pyqtSignal(object)
    fontChanged = pyqtSignal()

    def __init__(self,  # type: BookDisplay
                 c_highlight,  # type: C
                 font_family,  # type: str
                 font_size=default_font_size,  # type: int
                 parent=None  # type: QWidget | None
                 ):
        # type: (...) -> None
        super().__init__(parent)
  # [..]
```

C (passed in `props_obj`) is just an object to store the properties we need. Since the themes get saved to file as pseudo-qss, each colour is in pseudo css properties fg, bg, outline, l/r/t/b border, sel, sel_bg, which each store information in an instance of this object:
```python
class ThemeCProp():
    def __init__(self, qss_property, value=None):
        # type: (ThemeCProp, str, str | None) -> None
        self.used = value is not None
        self.qss_property = qss_property
        self.value = value
        self.c = QColor(value)

    def __call__(self):
        # type: (ThemeCProp) -> QColor
        return self.c

    def set_(self, value):
        # type: (ThemeCProp, str) -> None
        self.value = value
        self.c = QColor(value)
```
C instantiates them like this:
```python
self.fg = ThemeCProp('color', fg)
```
And that way we can later on access the qss property name and the QColor.

As you can see in BookDisplay's `__init__` in the code snippet higher up, each widget takes in the C it needs, and then it can use them by calling a particular property: `qp.setBrush(self.c_highlight.bg())` since `__call__` is overidden to return the QColor (for convenience; you could easily just get it from a property instead).

For keyboard shortcuts, what is the equivalent we need to do? What do we need to store?

The equivalent of QColor for our case is QKeySequence.

Let's consider the command Quit on the menu controller. It could be defined like this on the MenuController class:
```python
@keymap('Menu',
        K('Quit', ['Alt+F4']))
```
I guess we're going to have to get it by string rather than properties because we don't know the commands needed ahead of time. This is problematique, because some of them could have long names. How to handle this (how else to identify them)?

## Command arguments and entries
The UI to modify the shortcuts can be populated with the defaults, and each one needs to have the edit for the key combination and another edit for any arguments to pass in (also populated with the defaults or blank), and then there would be an add button that allows adding another version of it where you could assign a different shortcut and/or pass different arguments.

Are we going to have a keymap file then rather than storing the shortcuts in the configuration? Analogous to theme files. Kind of like in Blender. Why do we need this rather than saving in the configuration? I don't know, I think the fact the user can have an arbitrary number of them and create their own makes me think it should be a separate file.

Another thing to consider is deleting entries. Like should you be able to delete the defaults? You can in Blender.

## More on the issue of identification
If you can have many shortcuts for one command, and different entries calling it with different arguments, how can we differentiate one from the other? It came up at the end of the decorator section, the issue of identification.

There are several kinds of identification here. In the decorator, each command should have one identity, having multiple shortcuts is represented as different entries in the list of shortcuts passed on to K. Different entries calling it with different arguments will still be this same identity. The UI design I invoked before suggests each such command has almost like its own little section with a button to create other variants of it. However, if we are able to delete any of them like Blender, then it can't really be like that.

In Blender each command has an identifier, like `wm.quit_blender`. It's used like a string. It doesn't seem to have much of a hierarchy.

Which made me think that maybe we don't need it to be on "Menu". There can be just a single object with all of the shortcuts. It's not like the colours where they are unique to a particular widget. But there is only one widget that sets them so in that sense they are unique, plus for example the Book View shortcuts work only in Book View. So it *does* belong to the widget. We do need a hierarchy I think. Book View will never be concerned with what the shortcut for Quit is. It might need to provoke quit, but that has nothing to do with the keyboard shortcut.

And actually they do have an hierarchy in Blender, at least in the settings UI each mode has its own collapsible section (cf [screenshot on SE](https://blender.stackexchange.com/questions/107406/important-blender-shorcuts-for-object-modeling)).

I think it would be simplest to keep it analogous to themes. It will be easier for me to implement and wrap my head around.

I have an idea though for how it could work as one object: that the identifiers will contain the "paths", like quit as "Menu.Quit", then Customisation Dialog will know to put it in a Menu section (if needed. for a start it is maybe best off just left flat given we only have a few shortcuts).

But since to be analogous to themes we create an object in the decorator, it does need to be a separate object for each decorator use.

I think I'm going to name them in camel case, which is the convention for functions in PyQt and in retype's codebase (though it's not the convention in Python, but that ship has long sailed).

## Functional decorator
So at first let's try to get this decorator working:
```python
@keymap('Menu',
        K('quit', ['Alt+F4']))
class MenuController(QObject):
# [..]
```
And to make this run, create `services/keymap.py`:
```python
from qt import QObject, pyqtSignal


class K(QObject):
    changed = pyqtSignal()

    def __init__(self,  # type: K
                 name,  # type: str
                 shortcuts  # type: list[str]
                 ):
        # type: (...) -> None
        QObject.__init__(self)
        self.name = name
        self.shortcuts = shortcuts

    def __call__(self):
        # type: (K) -> list[str]
        return self.shortcuts

    def set_(self, value):
        # type: (K, list[str]) -> None
        self.shortcuts = value


class Keymap:
    selectors = {}  # type: dict[str, K]


def keymap(selector, props_obj):
    # type: (str, K) -> Callable[[type[T]], type[T]]
    def decorator(cls):
        # type: (type[T]) -> type[T]
        props_obj.name = selector
        Keymap.selectors[selector] = props_obj
        return cls
    return decorator
```
Not sure if the things on Keymap should be called "selectors". I copied it from the way it's done in `services/theme.py`. It essentially means that "quit" is a selector. I don't have an idea for a better name, so we'll go with that.

Now I'm wondering how to represent alternative entries for the same selector. We could easily have different shortcuts for the same thing, because it's in a list of strings `shortcuts`, but what about arguments? They can be simply in another property `arguments`, because each needs its own separate `shortcuts`. So I guess we need another class for `K` to contain, kind of like how, in theme, `C` contain `ThemeCProp`s.

That suggests that `K`'s initialiser needs to be changed too, in order for it to be possible to define shortcuts for different arguments in the decorator if necessary.

Let's then turn our attention to a command that could have arguments: BookView's gotoCursorPosition. It can take an argument, `move`, that if provided moves the cursor position to your position instead of the other way around. Let's say we wanted to have bindings for both variants.
```python
@keymap('BookView',
        K('gotoCursorPosition', {'': ['Alt+C'], 'move': ['Alt+Shift+C']}))
```

Another complication here is this exists also as a console command. Maybe the console commands service needs to never define its own shortcuts, instead just calling actions on other widgets of the application. It's currently does that, kind of; it just calls BookView's function. But it is _the console command_ that can be called with either no arguments, or with "move" (or "m"), the BookView function just has a boolean `move`, but at no point do you pass "move", so it's maybe kind of weird to represent the argument as "move"? It's something I do with the console command in mind. The _console commands_ are the ones that pass arguments as strings like that, real functions don't.

We also need a way to define commands that don't have any bindings by default, like currently `gotoCursorPosition` doesn't have any bindings. Something like this:
```python
@keymap('BookView',
        K('gotoCursorPosition'))
```

Which makes me ever more confused about how arguments are going to work. Does it need a way to define also possible arguments?

It's just like arguments only exist from a console commands perspective. QActions don't have arguments in that sense, they only do one thing.

Currently the toolbar action for `gotoCursorPosition` does _two_ things, because it checks if Ctrl is held down and does the same as if it was called with `move=True` in that case:
```python
    def gotoCursorPosition(self, move=False):
        # type: (BookView, bool) -> None
        if self.chapter_pos is None:
            return
        if move or (self._keyboardModifiers() == Qt.KeyboardModifiers(
                Qt.KeyboardModifier.ControlModifier)):
            self.setChapter(self.viewed_chapter_pos, True)
            self.updateProgress()
        else:
            self.setChapter(self.chapter_pos)
            self.setCursor()
            self.display.centreAroundCursor()
```
We can't really get rid of that and replace it with a keymap binding, because there is no way to say in the binding "click on the toolbar button" or "Ctrl click on the toolbar button".

Let's not complicate it. Let's just say we can define arbitrary arguments and call them whatever, and the widget that defines the shortcuts decides what to do with it. The whole thing is as a string. Like the arguments is just a string. '' or 'move' or 'move=123' or 'move=123 now' or whatever you want. The widget can handle it, or not handle it. The user can define their own entry for a command giving whatever in the arguments string and whatever bindings, and it might do nothing if the arguments string is not something that exists and not handled by the widget (it could print an error or something in that case if it wants).

So we need to move the shortcuts onto another object, KeymapEntry:
```python
class KeymapEntry:
    def __init__(self, arguments, shortcuts):
        # type: (KeymapEntry, str, list[str]) -> None
        self.arguments = arguments
        self.shortcuts = shortcuts

    def __call__(self):
        # type: (KeymapEntry) -> list[str]
        return self.shortcuts

    def set_(self, value):
        # type: (KeymapEntry, str) -> None
        self.shortcuts = value
```
and K can instead take a map of args:shortcuts:
```python
class K(QObject):
    changed = pyqtSignal()

    def __init__(self, name, entries_information_map):
        # type: (K, str, dict[str, list[str]]) -> None
        QObject.__init__(self)
        self.name = name
        self.entries_map = {}
        for a, s in entries_information_map.items():
            self.entries_map[a] = KeymapEntry(a, s)
```

But hang on a minute, what's the point of KeymapEntry now? We could just store the plain args:shortcuts map, we don't need an object to store them. I think the reason it exists in theme is so that you could do for any selector something like `c.bg()` to get the background colour, the possible "props" are known for any C in advance. But for K, this is not the case.

If we just have:
```python
class K(QObject):
    changed = pyqtSignal()

    def __init__(self, name, entries_map):
        # type: (K, str, dict[str, list[str]]) -> None
        QObject.__init__(self)
        self.name = name
        self.entries_map = entries_map
```
then for the previous example, `K('gotoCursorPosition', {'': ['Alt+C'], 'move': ['Alt+Shift+C']})`, to get the shortcuts out for no arguments it's `k.entries_map.get('')`, or for "move" `k.entries_map.get('move')`, which is obviously not good interface, and you also have to check that it exists before being able to use it, because the `get` could return `None`, even though _we know it exists because we create it in the decorator just over the class_. But, remember, there are also the arguments for entries defined by the user, which we don't know in advance, so in reality it needs to be more like a switch over the different "arguments" that are in there.

And it also becomes clear that we need to have a better name for it then "arguments" because it's plural where in reality it refers to a single string, and we need to refer sometimes to a plural of _that_. Like argstr to be able to say argstrs, but I don't really like that name. `arguments_string` is too verbose.

In Windows shortcuts, the input where we can supply the arguments to give the program is called "Target" (cf [screenshot on SE](https://superuser.com/questions/358565/adding-command-line-switches-to-windows-shortcuts)), but it also contains the path to the executable, hence why it's called that. I suppose that instead of having name and arguments as separate fields, it can just be in name (which we could rename target) and split on space.

[PCGamingWiki has other examples of this sort of thing in other programs](https://www.pcgamingwiki.com/wiki/Glossary:Command_line_arguments), with screenshots.
- On Battle.net it's called "Additional command line arguments".
- On Epic Games Store it's called either "Additional Command Line Arguments" or "Launch Options".
- On GOG Galaxy 2.0 it's called "Arguments"
- On Steam it's "LAUNCH OPTIONS"
- On Ubisoft Connect it's "Game launch arguments"

That doesn't help. I hoped one of them would have a singular name.

I'm going to call it argsstr. (I prefer to avoid underscores in short names.)

K could have a method like this:
```python
    def entries(self):
        # type: (K) -> dict_items
        return self.entries_map.items()
```
In BookView to get them out for `gotoCursorPosition`, we'd loop over the entries, handling the argsstrs we want to support. I guess what the "handling" would entail is just setting the shortcuts on a QAction.

Alternatively, it could get directly the list of shortcuts for the argstr it wants, if we define on K a method like this:
```python
    def s(self, argstr):
        # type: (K) -> list[str]
        """Get shortcuts list for an entry"""
        res = self.entries_map.get(argstr)
        assert type(res) is list
        return res
```
so BookView will be able to do `a.setShortcuts(k.s())`.

Two questions come to my mind:
1. Does a shortcut on a QAction still function if the QAction is not on the UI somewhere?
2. Can one QAction have multiple shortcuts?

For 2 it's a yes, QAction has a method `setShortcuts` that allows you to pass a list (cf [docs](https://doc.qt.io/archives/qt-5.15/qaction.html)). It has the concept of Primary Shortcut, which is the first item on that list. Calling the method `shortcut` returns just the Primary Shortcut, and `setShortcut` (singular) sets only the Primary Shortcut.

For 1 it's a no:

> Note that an action must be added to a widget before it can be used; this is also true when the shortcut should be global (i.e., Qt::ApplicationShortcut as Qt::ShortcutContext).  
—[*Qt docs: QAction Class*](https://doc.qt.io/archives/qt-5.15/qaction.html#details)

but you can do `addAction` on any QWidget, not just toolbars and menus.

I want to also be able to pass to K a single entry with no arguments, so we could still use the decorator as simply as:
```python
@keymap('Menu',
        K('quit', ['Alt+F4']))
```
where we have no need for argstrs. We could change hte initialiser to take "primary" and "other" shortcuts separately:
```python
    def __init__(self, name, primary_shortcuts, other_shortcuts=None):
        # type: (K, str, list[str], dict[str, list[str]] | None) -> None
        QObject.__init__(self)
        self.name = name
        self.entries_map = {'': primary_shortcuts}
        if other_shortcuts is not None:
            self.entries_map = {**self.entries_map, **other_shortcuts}
```

And change `s` `argstr` argument to `''` by default, so that we don't need to give an argument at all to get the primary shortcuts.
```python
    def s(self, argstr=''):
        # type: (K) -> list[str]
        """Get shortcuts list for an entry"""
        res = self.entries_map.get(argstr)
        assert type(res) is list
        return res
```

## Get K in Customisation Dialog
We have worked out how to define the shortcuts, and how the widgets concerned can get them out to assign them. Let's now look at how to get them out in the Customisation Dialog to be able to start working on the interface to customise them.

On CustomisationDialog, we have a categories widget with the different tabs for each "page" of customisation added like this:
```python
        catw = CategorisedWidget()
        catw.add("Filesystem", "Paths", self._pathSettings())
        catw.add("User interface", "Icons", self._iconsSettings())
        catw.add("User interface", "Theme", self._themeSettings())
        catw.add("User interface", "Console", self._consoleSettings())
        catw.add("User interface", "Book View", self._bookviewSettings())
        catw.add("User interface", "Window geometry", self._windowSettings())
        catw.add("Behaviour", "Line splits", self._sdictSettings())
        catw.add("Behaviour", "Replacements", self._rdictSettings())
        catw.add("Games", "Learn Stenography", self._stenoSettings())
        catw.postAddingCategories()
```
Keymap should be after Theme, and likewise be under "User interface":
```python
catw.add("User interface", "Keymap", self._keymapSettings())
```
With this bare minimum method to make it run:
```python
    def _keymapSettings(self):
        # type: (CustomisationDialog) -> QWidget
        pkm = QWidget()
        return pkm
```

Looking back at theme again for reference, this is what `_themeSettings` do to get the data:
```python
        # [..]
        themes = QComboBox()
        # [..]
        user_dir = self.getUserDir()
        populateThemes(getStylePath(), getStylePath(user_dir))
        prefix_to_exclude = THEME_MODIFICATIONS_FILENAME.rsplit('.', 1)[0]
        for t in Theme.themes:
            if not t.startswith(prefix_to_exclude):
                themes.addItem(t)
        themes.model().sort(0)
        themes.setCurrentIndex(0)

        self.theme = ThemeWidget(user_dir)
        self.theme.changed.connect(self.themeUpdate)
        # [..]
```
where `THEME_MODIFICATIONS_FILENAME` is `__current__.qss`. Before loading themes from disk, `Theme.themes` is an empty dictionary, but the selectors and default colours are in `Theme.selectors`. For us, similarly, the decorator adds the Ks to it:
```python
def keymap(selector, k):
    # type: (str, K) -> Callable[[type[T]], type[T]]
    def decorator(cls):
        # type: (type[T]) -> type[T]
        k.name = selector
        Keymap.selectors[selector] = k
        return cls
    return decorator
```
(I also took the opportunity to rename `props_obj` to `k`)

{% include note.html content='
> [!NOTE]
> `k.name = selector` is a mistake.
' %}

I actually forgot my own code and I'm confused about how there is not a single `Theme.selectors` in customisation dialog, how does it populate the current colours in the widgets?
(in the customisation dialog widgets I mean, the ones that show the current colours in little boxes that you can click on to modify)

[Future me: It calls the static method `Theme.getValuesDict` which gives the values in `Theme.selectors` in the format it expects]

But for now just to test that this is working, we can just do:
```python
        print(Keymap.selectors)
        print(Keymap.selectors['Menu'].name)
        print(Keymap.selectors['Menu'].s())
```
and get:
```python
{'Menu': <retype.services.keymap.K object at 0x7f2dde9b7ad0>}
Menu
['Alt+F4']
```

Hang on a minute. Where is "quit"?

Keymap has a map `selectors`, one entry with the key "Menu" and the value an instance of K, and it's instantiated with `K('quit', ['Alt+F4'])`. What have I done wrong here?

Ah, we need to remove the line `k.name = selector` on the decorator.

Let's attempt using QKeySequenceEdit.
```python
    def _keymapSettings(self):
        # type: (CustomisationDialog) -> QWidget
        pkm = QWidget()
        lyt = QFormLayout(pkm)
        editor = QKeySequenceEdit()
        editor.setKeySequence(Keymap.selectors['Menu'].s()[0])
        lyt.addRow(Keymap.selectors['Menu'].name, editor)
        return pkm
```
It's just an edit where if you press any keys with it focused it will input it followed by "..." for a couple of seconds. You can even insert Emacs-like key combinations, up to 4 chords. It _is_ a bit annoying. I personally don't use a mouse, controlling everything instead with the keyboard, and I can't seem to navigate away, it inserts junk into it (even if I wait after the listening period). Tab does work though.

I wonder if I should just use text inputs. I think not, because then we would have to deal with validation. I guess instead we need to have it set up like many programs have it, that first you press a button to bring up the editor, then after you finish inserting the hotkey it will close out of it, or alternatively just wait for a few seconds then close it.

Little proof of concept:
```python
    def _keymapSettings(self):
        # type: (CustomisationDialog) -> QWidget
        pkm = QWidget()
        lyt = QFormLayout(pkm)
        k = Keymap.selectors['Menu']
        s = k.s()[0]
        editor = QKeySequenceEdit()
        editor.setKeySequence(s)
        btn = QPushButton(s)

        def updateS(s):
            t = s.toString()
            if t:
                btn.setText(t)

        editor.keySequenceChanged.connect(updateS)

        def showEdit():
            editor.show()
            editor.setFocus()
            QTimer.singleShot(2000, lambda: editor.hide())

        btn.clicked.connect(showEdit)
        w = QWidget()
        wlyt = QHBoxLayout(w)
        wlyt.addWidget(btn)
        wlyt.addWidget(editor)
        editor.hide()
        lyt.addRow(k.name, w)
        return pkm
```
It's quite annoying to use. I don't like it. Using QKeySequenceEdit alone does not seem to me a good idea either, it's unfriendly to keyboard navigation.

We could maybe just use text and for validation pass it to a QKeySequence and see what it says / check its `toString`.

We could have a combination of a text input and a QKeySequenceEditor input; normal text input at the start, a "listen" button next to it which changes to a stop button after clicking on it, and the text input turns into QKeySequenceEditor, until you press the stop button or Esc.

Or maybe we should conform with standard UI that I see frequently and put the QKeySequenceEditor on a modal dialog. But what advantage is there? There are other ways to prevent several editors invoked at once.

Probably just a QKeySequenceEditor that you can escape out of with Esc, that's all.

As for exiting out of it, there is no such method, but we could just switch the focus to another widget probably. But in order to catch Esc, we have to subclass in order to override keyPressEvent, and it might not have a parent, and we might not have access to another widget. On the [list of members](https://doc.qt.io/archives/qt-5.15/qkeysequenceedit-members.html) there is `setEditFocus(bool )`, but when I try to call it I get:

> AttributeError: 'QKeySequenceEdit' object has no attribute 'setEditFocus'

There's also `clearFocus`, and that one works.

```python
class EscapableKeySequenceEdit(QKeySequenceEdit):
    def __init__(self, parent=None):
        # type: (EscapableKeySequenceEdit, QWidget | None) -> None
        QKeySequenceEdit.__init__(self, parent)

    def keyPressEvent(self, e):
        # type: (EscapableKeySequenceEdit, QKeyEvent) -> None
        if e.key() == Qt.Key_Escape:
            self.clearFocus()
            return
        QKeySequenceEdit.keyPressEvent(self, e)
```

Using it, and also now doing it for all the selectors in a loop instead of just the first one:
```python
    def _keymapSettings(self):
        # type: (CustomisationDialog) -> QWidget
        pkm = QWidget()
        lyt = QFormLayout(pkm)
        for n, k in Keymap.selectors.items():
            lyt.addRow(QLabel("<b>" + n + "</b>"))
            for s in k.s():
                editor = EscapableKeySequenceEdit()
                editor.setKeySequence(s)
                lyt.addRow(k.name, editor)
        return pkm
```
It will need to be in its own widget later to be able to have signals, but for now keeping it inline for simplicity.

Currently it looks like this:
```asciiart
 Menu
 quit  Alt+F4
```

If I add this decorator to BookView:
```python
@keymap('BookView', K('gotoCursorPosition', []))
```
```asciiart
 Menu
 quit  Alt+F4
 BookView
```
With nothing under BookView.

We need to have the ability to have commands with no shortcuts bound to them by default. First of all, I don't want to have to pass `[]` in that case. Let's modify K's initialiser for `primary_shortcuts` to be optional as well:
```python
    def __init__(self, name, primary_shortcuts=None, other_shortcuts=None):
        # type: (K, str, list[str] | None, dict[str, list[str]] | None) -> None
        QObject.__init__(self)
        self.name = name
        self.entries_map = {'': primary_shortcuts or []}
        if other_shortcuts is not None:
            self.entries_map = {**self.entries_map, **other_shortcuts}
```
Now we can do:
```python
@keymap('BookView', K('gotoCursorPosition'))
```
with the same results as before.

To have commands not bound show up, instead of doing `for s in k.s()` and creating the editor inside of that loop, we could create the editor for primary shortcuts before the loop, and either doing `setKeySequence` with the first element of `k.s()` if it exists, or leaving it empty. But then how to handle other shortcuts? We don't currently have the argstr displaying, nor a way to add entries. And even in the primary shortcuts there could be several and we currently show only the first. `k.s()` is actually returning only primary shortcuts, to get the shortcuts for a non-empty argstr it's like `k.s('move')`.

Handling primary shortcuts:
- If `k.s()` is empty, show one empty editor
- If it's not empty, show as many editors as there are shortcuts

Handling other shortcuts: same thing but with `k.s(argstr)`? No, because actually there could be infinite argstrs. Imagine a command like `setChapter`. It takes a number in the first argument, and either empty or "m" or "move" in the second argument (to indicate to also move the cursor to the chapter we're going to). There's quite a lot of possible combinations here, so we're not going to have an empty editor for all the possible arguments. Only the primary shortcuts need one. Other shortcuts will only get one if there is a binding.

Instead of `k.s()` we can do `k.entries()` to get dict items where the keys are argstrs (empty string for primary) and the values the associated shortcuts.
```python
    def _keymapSettings(self):
        # type: (CustomisationDialog) -> QWidget
        pkm = QWidget()
        lyt = QFormLayout(pkm)
        for n, k in Keymap.selectors.items():
            lyt.addRow(QLabel("<b>" + n + "</b>"))
            for argstr, shortcuts in k.entries():
                if argstr == '':
                    editor = EscapableKeySequenceEdit()
                    if len(shortcuts):
                        editor.setKeySequence(shortcuts[0])
                        shortcuts = shortcuts[1:]
                    lyt.addRow(k.name, editor)
                for s in shortcuts:
                    editor = EscapableKeySequenceEdit()
                    editor.setKeySequence(s)
                    lyt.addRow(':'.join([k.name, argstr]), editor)
        return pkm
```

Now we get:
```asciiart
 Menu
 quit                Alt+F4
 BookView
 gotoCursorPosition  Press shortcut
```

If we change the decorator on BookView to:
```python
@keymap('BookView', K('gotoCursorPosition', [], {'move': []}))
```
we still get the same thing.

If we change it to:
```python
@keymap('BookView', K('gotoCursorPosition', [], {'move': ['Ctrl+M']}))
```
we now get:
```asciiart
 Menu
 quit                     Alt+F4
 BookView
 gotoCursorPosition       Press shortcut
 gotoCursorPosition:move  Ctrl+M
```

Problem: If the command name and argstr are long, they squish the key sequence editors too small. I think instead of showing up after, argstr needs to show up below? And maybe be in an edit rather than a label, to be modifiable. But I don't know how to do the interface in a way that won't be confusing... We also need to work out where to put the button to add an entry. So we need to have a think about the UI now before going further.

In Blender each entry can be expanded to see more options. I don't think I want it that way because in our case it's a lot simpler, there's only the argstr, and it's clearer and less annoying for that to be visible straight away.

Since there's always one entry for each command, the one with argstr '', that one doesn't need an input to change the argstr, nor does it need to show the argstr. Beneath it are the alternative keybindings, and there we do not need to repeat the command name again, so there can be just the argstr. Also, to avoid clutter and increase clarity, I do think it's a good idea to have each category in its own tab (i.e. Menu in its own tab, BookView in its own tab, and whatever else). So then if we consider BookView only, it would look like this:
```asciiart
 gotoCursorPosition  Press shortcut
 move                Ctrl+M         [-]
 [+]
```
with `[-]` being a button to remove the entry in that line, and `[+]` being a button to add an entry. After pressing `[+]`:
```asciiart
 gotoCursorPosition  Press shortcut
 move                Ctrl+M         [-]
 Optional arguments  Press shortcut [-]
 [+]
```
"Optional arguments" as the placeholder for the argstr edit, so that it's clear you can leave that empty. And the `[+]` button can have the text "Add another shortcut".

## Add and remove entries
At this point I'm going to copy `ThemeWidget`, `ThemeSelectorWidget`, `ThemeCategoryWidget` and adapt them into keymap variants.

The selector widget is the one which will hold the UI we worked out above, and category is a tab (so like "Menu" is a category, "BookView" is another category, i.e. two different tabs). In the case of `Theme` there are several `C`s that can be in one category, but in our case, at least for now, each `K` is its own category.

{% include note.html content='
> [!NOTE]
> I was wrong or confused here. Each K is one command, so this design means we would have a separate tab for every single command, which is horrible UI because there will be too many tabs.
' %}

Category is just going to be an empty widget for now:
```python
class KeymapCategoryWidget(QWidget):
    def __init__(self, parent=None):
        # type: (KeymapCategoryWidget, QWidget | None) -> None
        QWidget.__init__(self, parent)
```
The widget will have the loop over `Keymap.selectors`, creating a new selector widget for every k:
```python
class KeymapWidget(QWidget):
    def __init__(self, parent=None):
        # type: (KeymapWidget, QWidget | None) -> None
        QWidget.__init__(self, parent)
        self.lyt = QVBoxLayout(self)
        self.lyt.setContentsMargins(0, 0, 0, 0)
        self.tabw = ScrollTabWidget()
        self.lyt.addWidget(self.tabw)
        for n, k in Keymap.selectors.items():
            cat_widget = KeymapSelectorWidget(k)
            cat_name = n
            self.tabw.addTab(cat_widget, cat_name)

    def minimumSizeHint(self):
        # type: (KeymapWidget) -> QSize
        return QSize(100, 100)

    def sizeHint(self):
        # type: (KeymapWidget) -> QSize
        return self.minimumSizeHint()
```
The sizeHint stuff is just to prevent the configuration dialog minimum size being too large on some operating systems.

{% include note.html content='
> [!NOTE]
> I forgot to even use KeymapCategoryWidget. It was that which was supposed to be given to `addTab`, not the selector widget.
' %}

The selector widget will have the loop over `k.entries`:
```python
class KeymapSelectorWidget(QWidget):
    changed = pyqtSignal()

    def __init__(self, k, parent=None):
        # type: (KeymapSelectorWidget, K, QWidget | None) -> None
        QWidget.__init__(self, parent)
        self.lyt = QFormLayout(self)
        for argstr, shortcuts in k.entries():
            if argstr == '':
                editor = EscapableKeySequenceEdit()
                if len(shortcuts):
                    editor.setKeySequence(shortcuts[0])
                    shortcuts = shortcuts[1:]
                self.lyt.addRow(f'<b>{k.name}</b>', editor)
            for s in shortcuts:
                editor = EscapableKeySequenceEdit()
                editor.setKeySequence(s)
                w = QWidget()
                wlyt = QHBoxLayout(w)
                wlyt.setContentsMargins(0, 0, 0, 0)
                wlyt.addWidget(editor)
                wlyt.addWidget(QPushButton("-"))
                self.lyt.addRow(QLineEdit(argstr), w)
        self.lyt.addRow(QPushButton("+ Add another shortcut"))
```
Painfully, the editor is 1 pixel higher than w, even with content margins 0 on wlyt. Not sure what to do about that.

To make the add another shortcut button work (at least visually, we've not got anything updating the underlying data yet), instead of instantiating it directly inside of the `addRow`, first save it to `self.addbtn` and connect its clicked signal to a method we'll call `addEntry`. We can actually move all the logic in `for s in shortcuts` to it if we make ti able to take s (the shortcut) and argstr as optional arguments.
```python
class KeymapSelectorWidget(QWidget):
    changed = pyqtSignal()

    def __init__(self, k, parent=None):
        # type: (KeymapSelectorWidget, K, QWidget | None) -> None
        QWidget.__init__(self, parent)
        self.lyt = QFormLayout(self)
        for argstr, shortcuts in k.entries():
            if argstr == '':
                editor = EscapableKeySequenceEdit()
                if len(shortcuts):
                    editor.setKeySequence(shortcuts[0])
                    shortcuts = shortcuts[1:]
                self.lyt.addRow(f'<b>{k.name}</b>', editor)
            for s in shortcuts:
                self.addEntry(s, argstr)
        self.addbtn = QPushButton("+ Add another shortcut")
        self.addbtn.clicked.connect(self.addEntry)
        self.lyt.addRow(self.addbtn)

    def addEntry(self, s="", argstr=""):
        # type: (KeymapSelectorWidget, str, str) -> None
        editor = EscapableKeySequenceEdit()
        if s:
            editor.setKeySequence(s)
        w = QWidget()
        wlyt = QHBoxLayout(w)
        wlyt.setContentsMargins(0, 0, 0, 0)
        wlyt.addWidget(editor)
        wlyt.addWidget(QPushButton("-"))
        self.lyt.addRow(QLineEdit(argstr), w)
```
But there's a problem here: the entries are added after the add button. It needs to be in a separate layout. So we need to have two nested layouts here. But if we use QVBoxLayout for the outer layout, the button is going to take half the height no? [no.] And you actually can't have two nested layouts, we'd need another widget.

That's not that big of a problem. and actually it does not get split to two, the button appears at the very bottom. But that's a problem: we're going to have more than one "selector" (command to make shortcuts for) per tab. But this is the selector widget, so this is for one selector. But earlier I said it's... It looks like I was confused earlier. The design of KeymapWidget is wrong. The way it currently is, we're going to have every single command on its own tab so we would have tonnes of tabs and it's horrible UI. And we forgot to even use `KeymapCategoryWidget`. It's that that was supposed to be the widget to give to `addTab`, not the selector.

First, for the problem with the button, instead of having it at the bottom we could put it at the top and have a more clear separation between the primary and additional shortcuts:
```asciiart
 gotoCursorPosition
 Primary shortcut    Press shortcut
 Additional shortcuts: [+ Add]
 move                Ctrl+M         [- Remove]
 Optional arguments  Press shortcut [- Remove]
```
You may recall that in K we had "primary shortcuts" and "other shortcuts". It would not be right to call additional shortcuts here "other shortcuts", because it in fact can have both primary and other shortcuts, because what primary shortcuts means in K is an empty argstr, as we have a separate list for each argstr in its entries data structure.

I'm actually going to go back to K and rename the parameters so that it's less confusing. `primary_shortcuts` to `shortcuts_no_argstr`, and `other_shortcuts` to `shortcuts_per_argstr`.
```python
    def __init__(self,  # type: K
                 name,  # type: str
                 shortcuts_no_argstr=None,  # type: list[str] | None
                 shortcuts_per_argstr=None  # type: dict[str, list[str]] | None
                 ):
        # type: (...) -> None
        QObject.__init__(self)
        self.name = name
        self.entries_map = {'': shortcuts_no_argstr or []}
        if shortcuts_per_argstr is not None:
            self.entries_map = {**self.entries_map, **shortcuts_per_argstr}
```

I just realised an issue with the previous customisation UI design idea: what if it's a command that requires an argument?
1. We could have all our commands have default values for arguments, for example setChapter with no arguments could by default go to the first chapter. But this is not very flexible and there could be situations where we can't do that.
2. Could just do nothing if it's called with no arguments. Which I think is what happens now with console commands (if you do just `>chapter` with no arguments, nothing happens). But then is it still going to appear in the UI as "primary shortcut"? This is confusing for a command that does nothing.

Should it be that the primary shortcut can have an argstr also?
```asciiart
 setChapter
 Primary shortcut:
 0                   Alt+Shift+,
 Additional shortcuts: [+ Add]
 -1                  Alt+Shift+.    [- Remove]
 Optional arguments  Press shortcut [- Remove]
```
[setChapter can't actually take -1 to go to the last chapter. It probably should.]

I don't like this. I think I'm going to go with 2, that it will just do nothing. In order to be less confusing, we probably need to get rid of the "primary shortcut" and have it like this:
```asciiart
 setChapter
                     Alt+Shift+,
 Additional shortcuts: [+ Add]
```
where to the left of the shortcut can be a disabled edit that's either empty or explicitly the string/placeholder "No arguments".

```python
class KeymapSelectorWidget(QWidget):
    changed = pyqtSignal()

    def __init__(self, k, parent=None):
        # type: (KeymapSelectorWidget, K, QWidget | None) -> None
        QWidget.__init__(self, parent)
        self.lyt = QFormLayout(self)
        self.lyt.addRow(QLabel(f'<b>{k.name}</b>'))
        for argstr, shortcuts in k.entries():
            if argstr == '':
                editor = EscapableKeySequenceEdit()
                if len(shortcuts):
                    editor.setKeySequence(shortcuts[0])
                    shortcuts = shortcuts[1:]
                eargstr = QLineEdit()
                eargstr.setPlaceholderText("No arguments")
                eargstr.setEnabled(False)
                self.lyt.addRow(eargstr, editor)
                self.addbtn = QPushButton("+ Add")
                self.addbtn.clicked.connect(self.addEntry)
                self.lyt.addRow("Additional shortcuts:", self.addbtn)
            for s in shortcuts:
                self.addEntry(s, argstr)

    def addEntry(self, s="", argstr=""):
        # type: (KeymapSelectorWidget, str, str) -> None
        editor = EscapableKeySequenceEdit()
        if s:
            editor.setKeySequence(s)
        w = QWidget()
        wlyt = QHBoxLayout(w)
        wlyt.setContentsMargins(0, 0, 0, 0)
        wlyt.addWidget(editor)
        wlyt.addWidget(QPushButton("- Remove"))
        eargstr = QLineEdit(argstr)
        eargstr.setPlaceholderText("Optional arguments")
        self.lyt.addRow(eargstr, w)
```

I like the UI now. It would be quite nice to have a way to show available arguments the user could supply, but let's not worry about that for now.

How to make the remove button work? `QFormLayout` has `removeRow`, to which we can give the row index or the widget therein.
```python
    def addEntry(self, s="", argstr=""):
        # type: (KeymapSelectorWidget, str, str) -> None
        editor = EscapableKeySequenceEdit()
        if s:
            editor.setKeySequence(s)
        w = QWidget()
        wlyt = QHBoxLayout(w)
        wlyt.setContentsMargins(0, 0, 0, 0)
        wlyt.addWidget(editor)
        rembtn = QPushButton("- Remove")
        rembtn.clicked.connect(lambda: self.lyt.removeRow(w))
        wlyt.addWidget(rembtn)
        eargstr = QLineEdit(argstr)
        eargstr.setPlaceholderText("Optional arguments")
        self.lyt.addRow(eargstr, w)
```
Later on it's going to be more complicated because we'd have to remove it also from the underlying data.

Lastly for this section, we need to fix the issue with the category widget not being used, and gain the ability to have several K per tab. To be able to test this, let's first add a few more decorators over MenuController:
```python
@keymap('Menu', K('quit', ['Alt+F4']))
@keymap('Menu', K('setViewByEnum', [], {'1': ['Ctrl+1'], '2': ['Ctrl+2']}))
@keymap('Menu', K('showCustomisationDialog', ['Ctrl+O']))
class MenuController(QObject):
    # [..]
```
Since they're all in 'Menu', they overwrite each other, only quit gets saved to selectors; it's technically the last one as decorators happen in inverse order in Python.

The way that theme handles it is you don't have more than one with the same key, but they would all start with their category followed by period, like "Menu.Something", "Menu.SomethingElse.MaybeEvenMoreNested", then in ThemeWidget it splits on period, take the first item, and passes it through a function called "spacecamel" in order to turn e.g. "BookView" into "Book View". That part doesn't seem necessary (adding the space), not sure why I did that.

In our case, it doesn't really make sense to have it like that. I wonder if we can do away with the first argument and have the decorator get the name of the class its own? In any case that's not really the problem, but more the ability to have several K in one selector.

This highlights a problem in naming. Because what does selector mean, then? "Menu"? "Menu.quit", which we don't actually do? It's taken from theme, but keymap is not quite the same structurally.

But maybe it should be. Maybe we should be doing the decorator like this:
```python
@keymap('Menu.quit', K(['Alt+F4']))
@keymap('Menu.setViewByEnum', K([], {'1': ['Ctrl+1'], '2': ['Ctrl+2']}))
@keymap('Menu.showCustomisationDialog', K(['Ctrl+O']))
```

K changes to no longer take name as an argument and instead it gets set in the decorator, which is what theme does, and what I did initially and removed, if you remember the confusion over `k.name = selector` at the start.
```python
def keymap(selector, k):
    # type: (str, K) -> Callable[[type[T]], type[T]]
    def decorator(cls):
        # type: (type[T]) -> type[T]
        k.name = selector
        Keymap.selectors[selector] = k
        return cls
    return decorator
```

Adapting from ThemeWidget:
```python
class KeymapWidget(QWidget):
    def __init__(self, parent=None):
        # type: (KeymapWidget, QWidget | None) -> None
        QWidget.__init__(self, parent)
        self.cat_widgets = {}  # type: dict[str, KeymapCategoryWidget]
        self.lyt = QVBoxLayout(self)
        self.lyt.setContentsMargins(0, 0, 0, 0)
        self.tabw = ScrollTabWidget()
        self.lyt.addWidget(self.tabw)
        self._populateWidgets()

    def _populateWidgets(self):
        # type: (KeymapWidget) -> None
        for n, k in Keymap.selectors.items():
            selector_widget = KeymapSelectorWidget(k)
            cat_name = self.friendlyCatName(n)
            cat = self.cat_widgets.get(cat_name)
            if (cat):
                cat.addSelectorWidget(selector_widget)
            else:
                cat_widget = KeymapCategoryWidget()
                cat_widget.addSelectorWidget(selector_widget)
                self.cat_widgets[cat_name] = cat_widget
                self.tabw.addTab(cat_widget, cat_name)

    def friendlyCatName(self, name):
        # type: (KeymapWidget, str) -> str
        return spacecamel(name.split('.')[0])
```
```python
class KeymapCategoryWidget(QWidget):
    def __init__(self, parent=None):
        # type: (KeymapCategoryWidget, QWidget | None) -> None
        QWidget.__init__(self, parent)
        self.lyt = QFormLayout(self)

    def addSelectorWidget(self, widget):
        # type: (KeymapCategoryWidget, KeymapSelectorWidget) -> None
        self.lyt.addRow(widget)
```

## Replace hardcoded shortcuts
For example:
```python
        self._addAction(fileMenu, '&Quit', self.controller.quit,
                        ['Alt+F4'], 'door')
```
If we have access to the K instance, it would suffice to do `k.s()` instead of `['Alt+F4']`.

This is where doing it by analogy with theme has led us astray again. There are many commands, each having their own K, we would have to pass a tonne of instances.

We could just get it out of Keymap. Its selectors is a static property. And we could add a static method to streamline it. Actually, Theme has the same thing. [And that's what the widgets do with Theme also to get their `C`s out. I misremembered that they get passed in the initialiser or something.]
```python
    @staticmethod
    def s(selector_name):
        # type: (str) -> K
        ret = Keymap.selectors.get(selector_name)
        if ret is None:
            logger.warning(f'Invalid keymap selector name {selector_name}')
            ret = K()
        return ret
```
I copied from it and just changed Theme to Keymap, and C to K, and renamed the method from `get` to `s`, because I prefer for it to be shorter.

MenuController can now do:
```python
        self._addAction(fileMenu, '&Quit', self.controller.quit,
                        Keymap.s('Menu.quit').s(), 'door')
```
It's not the most amazing interface I've ever seen, but it does the job.

I did this for all the actions in MenuController and BookView.

## Modify keymap
We've got it functional and the UI to customise it, but not the connection between them.

Let's examine first how theme does it.

`CustomisationDialog._themeSettings` connects ThemeWidget's changed to its method themeUpdate. But this doesn't seem very interesting, it's just for dealing with the CustomisationDialog revert and restore defaults buttons.

More interesting is what it does on save:
```python
    def saveCurrent(self, path):
        # type: (ThemeWidget, str) -> None
        # Get values dict
        values = {}
        for name, widget in self.selector_widgets.items():
            values[name] = widget.get()
            # Commit all the cedits
            for cedit in widget.cedits.values():
                cedit.commitCurrent()
        # Serialise
        res = serialiseValuesDict(values)
        # Write to file in path
        file_path = os.path.join(path, THEME_MODIFICATIONS_FILENAME)
        self._save(file_path, res)
        # Update colours in application
        Theme.set_(values)
```
I do want to have keymap files later, but for now the only bit that concerns us is how it updates colours in the application. It creates a values dictionary based on the values in all its selector widgets, and passes it to `Theme.set_`.
```python
    @staticmethod
    def set_(values):
        # type: (ValuesDict) -> None
        changed_selectors = []
        for name, s in Theme.selectors.items():
            changed = False
            for p in s.props:
                if p.used:
                    new_value = values.get(s.name, {}).get(p.qss_property)
                    if new_value is not None:
                        p.set_(new_value)
                        changed = True
            if changed:
                changed_selectors.append(s)
        for s in changed_selectors:
            s.changed.emit()
```
It simply changes the values for its selectors (`C`s) that have a new value in the `values` dict that was passed to it, and emits `changed` on each of them. Interested widgets "subscribe" to be notified when colours it uses change, by connecting this `changed` signal.

For example, BookDisplay does in its initialiser:
```python
        self.c_display, self.c_cursor = self._loadTheme()
        self.c_highlight = c_highlight
        self.c_display.changed.connect(self.themeUpdate)
        self.themeUpdate()
```
and has methods:
```python
    def _loadTheme(self):
        # type: (BookDisplay) -> tuple[C, ...]
        return (Theme.get('BookView.BookDisplay'),
                Theme.get('BookView.BookDisplay.Cursor'))

    def themeUpdate(self):
        # type: (BookDisplay) -> None
        qss = Theme.getQss('BookView.BookDisplay').replace(
            'BookView.BookDisplay', 'QTextBrowser')
        self.setStyleSheet(qss)
```
But this is the only widget that applies qss directly [actually, RDictEntryEditor and KDictEntryEditor in customisation dialog do this as well], [most of] the others just do stuff with the colours. But in most cases nothing actually needs to be done, because in most cases the colours are obtained dynamically. For example, on BookView:
```python
    def _loadTheme(self):
        # type: (BookView) -> tuple[C, ...]
        return (Theme.get('BookView.Highlighting.Highlight'),
                Theme.get('BookView.Highlighting.Mistake'))

    def themeUpdate(self):
        # type: (BookView) -> None
        self.mistake_format.setForeground(self.c_mistake.fg())
        self.mistake_format.setBackground(self.c_mistake.bg())
```
As you see, the mistake format is the only thing that needs to change despite there being a highlight selector also. For performance reasons, we don't need character formats for highlighting, but rather paint it directly in paintEvent:
```python
        # Draw highlight
        qp.setPen(Qt.PenStyle.NoPen)
        qp.setBrush(self.c_highlight.bg())
        vrect = self.viewport().rect()
        if not self.full_highlight:
            under = vrect.height() - crect.y()
            vrect.setHeight(vrect.height() - under)
            w = crect.width()
            crect.setLeft(vrect.x())
            crect.setWidth(crect.width() - w)
            qp.drawRect(crect)
        qp.drawRect(vrect)
```
When it does `self.c_highlight.bg()`, it's getting the colour from the selector on Theme, so when the selector changes, this next paint event will use the new colour, without needing to update anything explicitly.

In our case, the shortcuts are passed to QActions, so we will need to change the QActions explicitly.

There is the problem again of having a lot of `K`s, compared to Theme's relatively few `C`s. Like BookView is only concerned with 2 Cs, as we saw. MenuController is concerned with 9 Ks. Since they all start with "Menu." we could maybe write a way to get them out easily, but updating the corresponding QAction would still be manual, as we have no way currently to register this association (to know which QAction a shortcut is used by).

BookView, for its toolbar, has an actions dictionary like this:
```python
        actions = {
            'back':
            {
                'name': 'Back to shelves',
                'func': self.switchToShelves,
                'shortcut': Keymap.s('BookView.switchToShelves').s(),
            },
            'pos':
            {
            # [..]
```
from which afterwards the actions are created, and it even saves the instances in the dictionary:
```python
        for k, v in actions.items():
            action = addAction(**v)
            actions[k]['action'] = action
```
[It really should be `v['action'] = action` instead of `actions[k]['action'] = action`]

This is currently a local variable in `_initToolbar`, but we make it a property `self.actions`. Then have a method like this:
```python
    def keymapUpdate(self):
        # type: (BookView) -> None
        for k in self.Ks:
            try:
                self.actions[k.name]['action'].setShortcuts(k.s())
            except KeyError:
                logger.error(f"Updating k '{k.name}' failed, "
                             "unable to find corresponding action")
```
And rename the `self.actions` keys to match the keymap selector names.

But some of the actions may be using a shortcut with an argstr, so `k.s()` would not be right in that case. We could save the argstr in the dictionary and do instead:
```python
            try:
                a = self.actions[k.name]
                argstr = a.get('argstr', '')
                a['action'].setShortcuts(k.s(argstr))
            except KeyError:
                logger.error(f"Updating k '{k.name}' failed, "
                             "unable to find corresponding action")
```
This actually isn't right, because a single K can have multiple argstrs with shortcuts, and there could be multiple actions using shortcuts from the same K. For example, in menu controller:
```python
@keymap('Menu.setViewByEnum', K([], {'1': ['Ctrl+1'], '2': ['Ctrl+2']}))
```
`Keymap.s('Menu.setViewByEnum').s('1')` (Ctrl+1) is used by the action View → Shelf View, and `Keymap.s('Menu.setViewByEnum').s('2')` (Ctrl+2) by View → Book View.

So what we actually need to do is iterate over the actions dictionary instead of over a list of Ks.
```python
    def keymapUpdate(self):
        # type: (BookView) -> None
        for name, d in self.actions.items():
            try:
                argstr = d.get('argstr', '')
                d['action'].setShortcuts(Keymap.s(name).s(argstr))
            except KeyError:
                logger.error(f"Updating k '{name}' failed with KeyError. "
                             f"{traceback.format_exc()}")
```

I wonder if there is a good way in Python to avoid having to write this exact same method on several classes. A baseclass, but I don't know if it's worth the overhead.

I see an issue by the way. K's `s` method does this:
```python
    def s(self, argstr=''):
        # type: (K) -> list[str]
        """Get shortcuts list for an entry"""
        res = self.entries_map.get(argstr)
        assert type(res) is list
        return res
```
But there could be a situation there argstr isn't there, or isn't there _anymore_. I guess it would still be there as an empty list though if the user removes it. But it's probably more sensible to do `self.entries_map.get(argstr, [])`.

Since the only thing we take from the class is its actions dictionary, we can just put this method as a function on `keymap.py`. But not sure I like this, because if something else needs to call it instead of `w.keymapUpdate()` it'd be `keymapUpdate(w.actions)`. But in practice it looks like it never happens that something else calls it, only the widget itself connects it to the `changed` signal on the selectors. For now I'm going to leave it as a method.

There's another issue. I said "rename the `self.actions` keys to match the keymap selector names" but then for the ones that call the same command with a different argstr that name is the same. I guess the key could instead be `selectorname:argstr` and we'll split on `:`. I'm also going to update `shortcuts` in the dictionary (which I renamed from `shortcut` as technically it's a list now) even though it's not used by anything after the action is first created, because we might as well.
```python
    def keymapUpdate(self):
        # type: (BookView) -> None
        if not hasattr(self, 'actions'):
            logger.warning("keymapUpdate called with no actions set")
            return
        for name, d in self.actions.items():
            n = name.split(':')
            selector_name = n[0]
            argstr = n[1] if len(n) > 1 else ''
            try:
                s = Keymap.s(selector_name).s(argstr)
                d['shortcuts'] = s
                d['action'].setShortcuts(s)
            except KeyError:
                logger.error(f"Updating k '{name}' failed with KeyError. "
                             f"{traceback.format_exc()}")
```
(I also added the `hasattr` check because I left the actions definition in `_initToolbar` so something may call `keymapUpdate` before it is defined. Which I could resolve instead by doing `self.actions = {}` in the initialiser, but it would be a pain: due to mypy, I would have to define a typed dict for it or something. But actually looks like there already is a type: `dict[str, ActionInfo]`. And I'm actually not even running mypy right now because I'm scared to see what the new version broke. It's living in my head shouting at me even when it's not running.)

We actually don't need `shortcuts` at all, since they're always `Keymap.s(selector_name).s(argstr)`. We could just remove them and get them dynamically in the loop where we're creating the actions
```python
        for name, info in self.actions.items():
            n = name.split(':')
            selector_name = n[0]
            argstr = n[1] if len(n) > 1 else ''
            info['shortcuts'] = Keymap.s(selector_name).s(argstr)
            action = addAction(**info)
            info['action'] = action
```
Or even in `addAction` itself and avoid having `shortcuts` on `info` at all, but it would require it being passed the key.

In MenuController, there is an action Menu.toggleConsoleWindow, which should only be added under Windows. It has a separator before it in the menu. This is achievable by having things in the action info dictionary that we address and remove before passing it to addAction.
```python
            'Menu.toggleConsoleWindow': {
                'menu': viewMenu, 'name': 'Toggle System &Console',
                'func': lambda: self.controller.toggleConsoleWindow(),
                'icon': 'console', 'condition': iswindows,
                'before': viewMenu.addSeparator,
            },
```
```python
        for name, info in self.actions.items():
            n = name.split(':')
            selector_name = n[0]
            argstr = n[1] if len(n) > 1 else ''
            info['shortcuts'] = Keymap.s(selector_name).s(argstr)
            if info.pop('condition', True):
                before = info.pop('before', None)
                if before:
                    before()
                action = self._addAction(**info)
            info['action'] = action
```

{% include note.html content='
> [!NOTE]
> Subtle bug here on the last line! It should be either indented, or `action` reset to None at the start of each iteration. (cf [later section](#menu-setviewbyenum-2-bug))
' %}

The shortcut will still be there in the customisation dialog, because the decorator is not optional. It will just do nothing under other operating systems, even if you bind it. This is fine, I think, and better than removing it from the list, because keymaps should be shareable across different operating systems, so it's going to be there in the keymap. We could add later some kind of description to each command to say what it does, available arguments (if not too long), and under which conditions it works.

We have to also modify keymapUpdate, because info['action'] will be None if condition doesn't pass.
```python
                # Action could be None for OS-specific actions
                action = d.get('action')
                if action:
                    action.setShortcuts(s)
                else:
                    logger.debug(f"Skip updating non-existing action {name}")
```

We still haven't connected keymapUpdate to anything.

Since we're dealing with a lot of Ks, and it's kind of a pain to subscribe to all of them, maybe we could do it simpler and just have a signal on Keymap. But we would have to make it a QObject subclass and give it an initialiser and all...

Well ok, let's do it like theme does it then. But for connecting to the Ks we clearly need a function first that will return all selectors we're concerned with, like for BookView all selectors that start with "BookView.".
```python
    @staticmethod
    def cat(cat_name):
        # type: (str) -> list[K]
        ret = []
        for k, v in Keymap.selectors.items():
            if k.split('.')[0] == cat_name:
                ret.append(v)
        if not ret:
            logger.warning(f'No selectors in {cat_name}')
        return ret
```

But wait, if we connect to all of them, we would have keymapUpdate called a tonne of times, no? When one would be sufficient. Oh well, I guess it's negligible. Unless Qt has some but that makes it segfault if setShortcuts is called repeatedly too fast...

Ok, here's a hacky idea: have an empty K that's just there for notifications, and concerned widgets will connect to that.
```python
class Keymap:
    selectors = {}  # type: dict[str, K]
    notifier = K()
```
Then concerned widgets can connect to it in their initialiser with `Keymap.notifier.changed.connect(self.keymapUpdate)`.

Now the only things left to be able to modify the keymap are:
- Update Keymap.selectors
- Emit notifier.changed

Over 9k words into this devlog, the end is in sight.

As we saw earlier, theme does `Theme.set_(values)` to update colours in the application in the ThemeWidget method saveCurrent. CustomisationDialog calls it in its method `accept`, which is called when the user presses save.

In `_keymapSettings`, let's save our KeymapWidget instance to `self.keymap`, and in `accept` call `self.keymap.saveCurrent()`, then create this method on KeymapWidget.

What do we need to pass to Keymap in order for it to be able to update its selectors?

Maybe it would help if we think about how our keymap would be represented on disk, if we had to save it to a file (which I plan to do later to implement presets). We'll probably use json files. Possible structure:
```json
{
  "Menu.quit": {"": ["Alt+F4"]},
  "Menu.setViewByEnum": {"": [], "1": ["Ctrl+1"], "2": ["Ctrl+2"]}
}
```
A dictionary where the keys are the selector names and the values are a dictionary which resembles K's `entries_map`; list of shortcuts for each argstr.

[type: `dict[str, dict[str, list[str]]]`]

In ThemeWidget's saveCurrent, it creates the structure that it passes to Theme by iterating `self.selector_widgets`, which is a dictionary where it saves the key of each selector and its corresponding widget.

So we need to add to KeymapWidget initialiser:
```python
self.selector_widgets = {}  # type: dict[str, KeymapSelectorWidget]
```
and when creating the widgets, add each one to this dictionary.
```python
            selector_widget = KeymapSelectorWidget(k)
            self.selector_widgets[n] = selector_widget
```

Now in saveCurrent we can do something similar to what ThemeWidget does:
```python
    def saveCurrent(self):
        # type: (KeymapWidget) -> None
        values = {}
        for name, widget in self.selector_widgets.items():
            values[name] = widget.get()
        Keymap.set_(values)
```
Need to implement:
- `get` method on KeymapSelectorWidget
- `set_` method on Keymap

In KeymapSelectorWidget I guess we have to save all the `EscapableKeySequenceEdit` that we instantiate, then on each one we can call `.keySequence.toString()`.

We're going to save the editors into a dictionary like this: `{selectorname: {argstr: [editor1, editor2, ...], ...}, ...}` because there is a different editor per shortcut not just per argstr.
```python
        self.editors = {
        }  # type: dict[str, dict[str, list[EscapableKeySequenceEdit]]]
```
But the argstr can change, because it's an edit. And also there is no need to save the selectorname, because each KeymapSelectorWidget only deals with one selector. So rather we need a a data structure that will store each EscapableKeySequenceEdit and its corresponding QLineEdit (the one that edits the argstr). How to store that? And keep in mind that they can also be deleted. I guess they're already stored in the QFormLayout rows. Is there a way to iterate them
and get the widgets out?

There's `rowCount` and `itemAt`. The way we set up the layout, we have the argstr editor in the label role, and a QWidget with a QHBoxLayout containing the key sequence editor and a remove button in the field role. And the first line has the label with the selector name, so we start the loop from 1.
```python
    def get(self):
        # type: (KeymapSelectorWidget) -> dict[str, list[str]]
        values = {}
        for i in range(1, self.lyt.rowCount()):
            eargstr = self.lyt.itemAt(
                i, QFormLayout.ItemRole.LabelRole).widget()
            w = self.lyt.itemAt(
                i, QFormLayout.ItemRole.FieldRole).widget()
            editor = w.layout().itemAt(0).widget()
            if values.get(eargstr.text()):
                values[eargstr.text()].append(editor.keySequence.toString())
            else:
                values[eargstr.text()] = [editor.keySequence.toString()]
        return values
```
It doesn't work. The FieldRole is not actually that w widget, but instead it's sometimes the editor directly, and sometimes it's the remove button. Actually, the first one is a sequence editor, the second is a button, then it's plain QWidgets. It's because for the first entry it's different, that's the one where we have the disabled argstr edit and below that the label "Additional shortcuts:" and add button:
```asciiart
Menu.quit
No arguments          Alt+F4
Additional shortcuts: [+ Add]
```
So we want to start the loop from the row after that, 3. And before the loop, we'll handle the first editor explicitly.
```python
    def get(self):
        # type: (KeymapSelectorWidget) -> dict[str, list[str]]
        values = {}
        try:
            # 2nd row, after the selector name label
            firsteditor = self.lyt.itemAt(
                1, QFormLayout.ItemRole.FieldRole).widget()
            values[''] = [firsteditor.keySequence().toString()]
        except AttributeError:
            logger.error("Getting KeymapSelectorWidget first editor failed."
                         f"{traceback.format_exc()}")
            return values
        # Additional shortcuts starting from 4th row
        for i in range(3, self.lyt.rowCount()):
            try:
                eargstr = self.lyt.itemAt(
                    i, QFormLayout.ItemRole.LabelRole).widget()
                w = self.lyt.itemAt(
                    i, QFormLayout.ItemRole.FieldRole).widget()
                editor = w.layout().itemAt(0).widget()
                s = editor.keySequence().toString()
            except AttributeError:
                logger.error("Getting KeymapSelectorWidget editors failed."
                             f"i:{i} type:{type(editor)}\n"
                             f"{traceback.format_exc()}")
                continue
            if values.get(eargstr.text()):
                values[eargstr.text()].append(s)
            else:
                values[eargstr.text()] = [s]
        return values
```

In theory all that's left now is the `set_` method on Keymap.
```python
    @staticmethod
    def set_(values):
        # type: (ValuesDict) -> None
        changed = False
        for name, d in values.items():
            try:
                changed_ = Keymap.selectors[name].set_(d)
                changed = changed if changed else changed_
            except KeyError:
                logger.error(
                    f'Keymap.set_: Invalid keymap selector name {name}')
        if changed:
            Keymap.notifier.changed.emit()
```
[`ValuesDict = dict[str, dict[str, list[str]]]`]

And now `set_` method on K.
```python
    def set_(self, entries_map):
        # type: (dict[str, list[str]]) -> bool
        if entries_map != self.entries_map:
            self.entries_map = entries_map
            self.changed.emit()
            return True
        return False
```
`entries_map` has got a nested list, but it seems like the comparison still works.

## Hurrah, but how to clear?
It works, and the menu even changes to display the new shortcut. And it's pretty awesome that we can define Emacs-style shortcuts and they just work!

However, I noticed that there is no way to clear key sequence editor. Maybe bind delete or backspace but only when pressed with no modifiers?

Our EscapableKeySequenceEdit:
```python
class EscapableKeySequenceEdit(QKeySequenceEdit):
    def __init__(self, parent=None):
        # type: (EscapableKeySequenceEdit, QWidget | None) -> None
        QKeySequenceEdit.__init__(self, parent)

    def keyPressEvent(self, e):
        # type: (EscapableKeySequenceEdit, QKeyEvent) -> None
        if e.key() == Qt.Key_Escape:
            self.clearFocus()
            return
        QKeySequenceEdit.keyPressEvent(self, e)
```
I'm also going to change the escape to only clear focus if no modifiers are pressed.

```python
    def keyPressEvent(self, e):
        # type: (EscapableKeySequenceEdit, QKeyEvent) -> None
        if not e.modifiers():
            if e.key() == Qt.Key_Escape:
                self.clearFocus()
                return
            elif e.key() in [Qt.Key_Backspace, Qt.Key_Delete]:
                self.clear()
                return
        QKeySequenceEdit.keyPressEvent(self, e)
```

IIRC for compatibility with Qt6 we should rename to `Qt.Key.Key_Escape`

## Keymap presets
`ConfigurationDialog._keymapSettings` currently:
```python
    def _keymapSettings(self):
        # type: (CustomisationDialog) -> QWidget
        pkm = QWidget()
        lyt = QFormLayout(pkm)
        self.keymap = KeymapWidget()
        lyt.addRow(self.keymap)
        return pkm
```
In `_themeSettings` lyt is also given the controls for selecting a preset, and a label with 

I also want to add a label with information about Esc to unfocus and Backspace or Delete to clear.

Above `self.keymap`:
```python
        preset_lyt = QHBoxLayout()
        keymaps = QComboBox()
        preset_lyt.addWidget(keymaps, 1)
        apply_btn = QPushButton("Apply")
        apply_btn.setToolTip("Apply selected preset")
        preset_lyt.addWidget(apply_btn)
        export_btn = QPushButton("Export")
        export_btn.setToolTip("Export saved modifications as new preset")
        preset_lyt.addWidget(export_btn)
        lyt.addRow(QLabel("Preset:"), preset_lyt)
        lyt.addRow(descl("To import a preset, place the json keymap file in\
 the 'style' subfolder in either the user dir or application folder, and\
 retart retype."))
        lyt.addRow(descl("In the shortcut edits, Backspace or Delete to clear,\
 Esc to unfocus."))
```
In `_themeSettings` there's a row with a horizontal line as well to separate the presets section from the ThemeWidget, but I worry about taking too much space already with our extra descl.

[If you're wondering what descl is, it's just a label that word-wraps. Which is not as simple as `setWordWrap(True)` because had to work around Qt bugs, see WrappedLabel in [extras/widgets.py](https://github.com/plu5/retype/blob/main/retype/extras/widgets.py)]

Next, `_themeSettings` does this:
```python
        user_dir = self.getUserDir()
        populateThemes(getStylePath(), getStylePath(user_dir))
        prefix_to_exclude = THEME_MODIFICATIONS_FILENAME.rsplit('.', 1)[0]
        for t in Theme.themes:
            if not t.startswith(prefix_to_exclude):
                themes.addItem(t)
        themes.model().sort(0)
        themes.setCurrentIndex(0)
```
`populateTheme` is a function from `theme.py`. Not sure why it's not a Theme static method. It goes through the qss files on disk in the paths you give it.

But since we don't have any keymap files on disk, I think we should work on export first. Adapting from `_themeSettings` (just changed `theme` to `kepmap`):
```python
        export_btn.clicked.connect(
            lambda: self.keymap.exportCurrent(self.getUserDir()))
```

This is the export method on ThemeWidget:
```python
    def exportCurrent(self, user_dir):
        # type: (ThemeWidget, str) -> None
        # Get values dict
        values = self.get()
        # Serialise
        res = serialiseValuesDict(values)
        # Save dialog
        path = getStylePath(user_dir)
        if not os.path.exists(path):
            path = getStylePath()
        file_path = QFileDialog.getSaveFileName(
            self, 'Export as new theme preset', path, 'Theme preset (*.qss)'
        )[0]
        if len(file_path) == 0:
            return
        # Write to file in path
        self._save(file_path, res)
```

We first need a `get` method on KeymapWidget, which will do the same as our existing method `saveCurrent` but return the resulting dictionary instead of passing it to `Keymap.set_`.
```python
    def get(self):
        # type: (KeymapWidget) -> dict[str, dict[str, list[str]]]
        values = {}
        for name, widget in self.selector_widgets.items():
            values[name] = widget.get()
        return values
```

The next thing is to serialise values. In our case we can just use `json.dumps`.

The rest of the steps are the same, except for changing the save dialog strings to json and keymap preset rather than theme and qss.
```python
    def exportCurrent(self, user_dir):
        # type: (KeymapWidget, str) -> None
        # Get values dict
        values = self.get()
        # Serialise
        res = json.dumps(values, indent=2)
        # Save dialog
        path = getStylePath(user_dir)
        if not os.path.exists(path):
            path = getStylePath()
        file_path = QFileDialog.getSaveFileName(
            self, 'Export as new keymap preset', path, 'Keymap preset (*.json)'
        )[0]
        if len(file_path) == 0:
            return
        # Write to file in path
        self._save(file_path, res)
```

ThemeWidget's `_save` method just saves the file:
```python
    def _save(self, file_path, qss):
        # type: (ThemeWidget, str, str) -> None
        try:
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            with open(file_path, 'w', encoding='utf-8') as f:
                logger.debug(f'Saving theme: {file_path}')
                f.write(qss)
        except OSError as e:
            logger.error(f'Failed to save theme to file {file_path}')
            logger.error(f'{type(e)}: {e}')
```
We can copy it as is, just changing theme to keymap, and qss to json.
```python
    def _save(self, file_path, json):
        # type: (KeymapWidget, str, str) -> None
        try:
            os.makedirs(os.path.dirname(file_path), exist_ok=True)
            with open(file_path, 'w', encoding='utf-8') as f:
                logger.debug(f'Saving keymap: {file_path}')
                f.write(json)
        except OSError as e:
            logger.error(f'Failed to save keymap to file {file_path}')
            logger.error(f'{type(e)}: {e}')
```
(json is the name of the module so we won't be able to use it in there, having shadowed it with a local variable. but it doesn't really matter in this case. though mypy may be unhappy about it)

Now our export button works, and I've used it to export the default keymap.

It's maybe a bit weird to save into the style directory, but it's simpler to keep it analogous to theme than to move it elsewhere. In any case there are already things in there that don't make sense, like Typespeed word lists.

The next thing to do is populate the presets combobox based on the files on disk.

We first need a function analogous to `populateThemes`. This is what it does:
```python
def populateThemes(app_path, user_path):
    # type: (str, str) -> None
    Theme.themes = {}
    paths = [app_path, user_path] if app_path != user_path else [app_path]
    for path in paths:
        for root, _, files in os.walk(path):
            for f in files:
                if f.lower().endswith('.qss'):
                    p = os.path.join(root, f)
                    name = os.path.splitext(f.lower())[0]
                    if name in Theme.themes and path == user_path:
                        name += ' (user dir)'
                    values = getThemeValues(name, p)
                    Theme.themes[name] = {'path': p, 'values': values}
            break  # do not recurse
```

`Theme.themes` is of type `dict[str, ThemeData]`, where ThemeData is:
```python
    ThemeData = TypedDict(
        'ThemeData',
        {'path': str, 'values': ValuesDict})
```
What do the str keys of `Theme.themes` store, then? The name. Based on the filename minus extension, and with `' (user dir)'` appended in the case of a file with the same name existing in both user dir style subfolder and application dir style subfolder.

getThemeValues:
```python
def getThemeValues(name, path):
    # type: (str, str) -> ValuesDict
    values = {}
    if os.path.exists(path):
        try:
            with open(path, 'r') as f:
                qss = f.read()
                values = valuesFromQss(Theme.getValuesDict(), qss)
        except OSError as e:
            logger.error(f'Failed to open theme file {name} in {path}')
            logger.error(f'{type(e)}: {e}')
    else:
        logger.warning(f'Theme {name} in path {path} not found.')
    return values
```
We can copy it as is and do `json.loads` instead of `valuesFromQss`.

But something else `valuesFromQss` does is have the default selectors as fallback, allowing a theme to only specify what's different from default instead of breaking if it's incomplete. In our case, we want to make sure all the selectors are in there, but we don't want to fallback to default values for the shortcuts nor for the argstrs; if a selector isn't there, then it should just be empty like:
```json
  "BookView.nextChapter": {
    "": [
      ""
    ]
  },
```

What we could do is take `Keymap.selectors.keys()` and create a fallback dictionary with those keys and `{"": []}` as the values.
```python
def getKeymapValues(name, path):
    # type: (str, str) -> ValuesDict
    values = {n: {'': []} for n in Keymap.selectors.keys()}  # fallback
    if os.path.exists(path):
        try:
            with open(path, 'r') as f:
                values.update(json.load(f))
        except OSError:
            logger.error(f'Failed to open keymap file {name} in {path}.'
                         f'{traceback.format_exc()}')
        except json.decoder.JSONDecodeError:
            logger.error(f'Failed to parse keymap file {name} in {path}.'
                         f'{traceback.format_exc()}')
    else:
        logger.warning(f'Keymap {name} in path {path} not found.')
    return values
```

There could be the situation where it's a valid json file but not structured like we're expecting. Since we're using the fallback dictionary, all the widgets should still be able to update their shortcuts, but something in customisation dialog or keymap.py might break. But I guess we're just going to accept that possibility and move on, not sure what I should be doing to deal with it.

```python
def populateKeymaps(app_path, user_path):
    # type: (str, str) -> None
    Keymap.keymaps = {}
    paths = [app_path, user_path] if app_path != user_path else [app_path]
    for path in paths:
        for root, _, files in os.walk(path):
            for f in files:
                if f.lower().endswith('.json'):
                    p = os.path.join(root, f)
                    name = os.path.splitext(f.lower())[0]
                    if name in Keymap.keymaps and path == user_path:
                        name += ' (user dir)'
                    values = getKeymapValues(name, p)
                    Keymap.keymaps[name] = {'path': p, 'values': values}
            break  # do not recurse
```
```python
class Keymap:
    selectors = {}  # type: dict[str, K]
    notifier = K()
    keymaps = {}  # type: dict[str, KeymapData]
    # [..]
```
```python
if TYPE_CHECKING:
    from typing import Callable, TypeVar, TypedDict  # noqa: F401
    T = TypeVar('T')
    ValuesDict = dict[str, dict[str, list[str]]]
    KeymapData = TypedDict(
        'KeymapData',
        {'path': str, 'values': ValuesDict})
```

Then in `_keymapSettings` to populate our presets combobox `keymaps`:
```python
        user_dir = self.getUserDir()
        populateKeymaps(getStylePath(), getStylePath(user_dir))
        for km in Keymap.keymaps:
            keymaps.addItem(km)
        keymaps.model().sort(0)
        keymaps.setCurrentIndex(0)
```

To make the apply button work, first let's add to `_keymapSettings`:
```python
        apply_btn.clicked.connect(
            lambda: self.keymap.applyKeymap(keymaps.currentText()))
```

Then examine ThemeWidget's `applyTheme` method:
```python
    def applyTheme(self, name):
        # type: (ThemeWidget, str) -> None
        values = {}  # type: dict[str, dict[str, str]]
        t = Theme.themes.get(name)
        if t:
            values = t.get('values', {})
        else:
            logger.warning('applyTheme: Unable to find values for theme '
                           f'{name}')
        for name, widget in self.selector_widgets.items():
            v = values.get(name, {})
            widget.set_(v)
```
There actually should be a return after the warning. Though it's not a bug, there won't be an exception because values is falls back to a dictionary, it's just pointless going through the loop of selector widgets after.

Otherwise, this is easy to adapt for keymap:
```python
    def applyKeymap(self, name):
        # type: (KeymapWidget, str) -> None
        values = {}  # type: dict[str, dict[str, list[str]]]
        km = Keymap.keymaps.get(name)
        if km:
            values = km.get('values', {})
        else:
            logger.warning('applyKeymap: Unable to find values for keymap '
                           f'{name}')
            return
        for name, widget in self.selector_widgets.items():
            v = values.get(name, {})
            widget.set_(v)
```
Though we have yet to implement a `set_` method on KeymapSelectorWidget. And here looking at the theme equivalent won't help, as keymap selectors data and widget are very different. It's a little bit tricky, because, remember, we don't save our subwidgets in a data structure, we instead rely on QFormLayout to contain them and extract them out of that. That's a bit of a pain.

What we could do is extract that code out of the `get` method we already wrote and put it in another method that would yield the widgets.
```python
    def widgets(self):
        # type: (KeymapSelectorWidget) -> Iterator
        try:
            # 2nd row, after the selector name label
            firsteditor = self.lyt.itemAt(
                1, QFormLayout.ItemRole.FieldRole).widget()
            yield '', firsteditor.keySequence().toString(), None, firsteditor
        except AttributeError:
            logger.error("Getting KeymapSelectorWidget first editor failed."
                         f"{traceback.format_exc()}")
        # Additional shortcuts starting from 4th row
        for i in range(3, self.lyt.rowCount()):
            try:
                eargstr = self.lyt.itemAt(
                    i, QFormLayout.ItemRole.LabelRole).widget()
                w = self.lyt.itemAt(
                    i, QFormLayout.ItemRole.FieldRole).widget()
                editor = w.layout().itemAt(0).widget()
                s = editor.keySequence().toString()
                yield eargstr.text(), s, eargstr, editor
            except AttributeError:
                logger.error("Getting KeymapSelectorWidget editors failed."
                             f"i:{i} type:{type(editor)}\n"
                             f"{traceback.format_exc()}")

    def get(self):
        # type: (KeymapSelectorWidget) -> dict[str, list[str]]
        values = {}
        for argstr, s, _, _ in self.widgets():
            if values.get(argstr):
                values[argstr].append(s)
            else:
                values[argstr] = [s]
        return values

    def set_(self, values):
        # type: (KeymapSelectorWidget, dict[str, list[str]]) -> None
        for _, _, eargstr, editor in self.widgets():
            # just realised this is not actually the right thing to
            # iterate, because the widgets layout needed may change
            pass
```
This works for `get`, but `set_` is more complicated, because what we actually need to iterate is `values`, and reconstruct the layout accordingly.

We do know that the first editor will remain the same widget. All the subsequent editors (rows after "Additional shortcuts:") we should probably remove, then call `addEntry` to readd them.
```python
    def set_(self, values):
        # type: (KeymapSelectorWidget, dict[str, list[str]]) -> None
        i = 0
        remove_rows_of = []
        for _, _, eargstr, editor in self.widgets():
            if i == 0:
                s = values.get('')
                if len(s):
                    editor.setKeySequence(s[0])
                else:
                    editor.clear()
            else:
                remove_rows_of.append(eargstr)
            i += 1
        for w in remove_rows_of:
            self.lyt.removeRow(w)
        for argstr, s in values.items():
            if argstr == '' and len(s):
                s = s[1:]
            for shortcut in s:
                self.addEntry(shortcut, argstr)
```
I at first made the mistake of calling `removeRow` inside the loop iterating over the rows. This causes errors in the `widgets` method, as the end of the range of its loop is still the same, so it keeps iterating onto rows that don't exist.

## `__current__` file
So far, we have the ability to modify shortcuts in the current session, and even save and load presets, but keymap changes that we make will also not be preserved across sessions. The way that theme handles it is by using a `__current__.qss` file. On first launch it does not exist [it only gets created/modified when the customisation dialog save button is pressed]. In its initialiser, ThemeWidget calls its method `_loadValues`:
```python
    def _loadValues(self, path):
        # type: (ThemeWidget, str) -> dict[str, dict[str, str]]
        values = deepcopy(self.default_values)
        file_path = os.path.join(path, THEME_MODIFICATIONS_FILENAME)
        if os.path.exists(file_path):
            try:
                with open(file_path, 'r') as f:
                    qss = f.read()
                    v = valuesFromQss(self.default_values, qss)
                    # Merge with default values (fallback for any missing)
                    update(values, v)
                    # Update colours in application
                    Theme.set_(values)
            except OSError as e:
                logger.error(f'Failed to open current theme file in {path}')
                logger.error(f'{type(e)}: {e}')
        else:
            logger.debug(f'Current theme {file_path} not found. This is normal\
 on first launch or if it has not been saved yet. Falling back to default\
 values.')
        return values
```
Where `self.default_values = Theme.getValuesDict()`. These default values are also used to implement restore defaults, so we need them anyway.

```python
    @staticmethod
    def getValuesDict():
        # type: () -> ValuesDict
        values = {}  # type: ValuesDict
        for name, k in Keymap.selectors.items():
            values[name] = {**k.entries_map}
        return values
```

Currently KeymapSelectorWidget takes a K instance, but it only really needs its name and the entries map (shortcuts per argstr).
```python
    def __init__(self,  # type: KeymapSelectorWidget
                 selector_name,  # type: str
                 entries_map,  # type: dict[str, list[str]]
                 parent=None  # type: QWidget | None
                 ):
        # type: (...) -> None
        QWidget.__init__(self, parent)
        self.lyt = QFormLayout(self)
        self.lyt.addRow(QLabel(f'<b>{selector_name}</b>'))
        for argstr, shortcuts in entries_map.items():
```

In KeymapWidget initialiser, add:
```python
        self.default_values = Keymap.getValuesDict()
        self.values = self._loadValues(getStylePath(user_dir))
```
and add `user_dir` as an argument, which we can pass when we instantiate KeymapWidget in `_keymapSettings`.

Then when populating the widgets, instead of doing `for name, k in Keymap.selectors.items():` do `for name, entries_map in self.values.items():`.

Now we need to write `_loadValues`. Since it needs to once again load a keymap file, similar to what we do in `getKeymapValues`, I'm going to split up that logic into another function.

But actually `getKeymapValues` already does that. It takes a name as well as a path, but the name is only used for logging.

And since we already do fallback values in `getKeymapValues`, we don't need to deep copy default values.
```python
    def _loadValues(self, path):
        # type: (KeymapWidget, str) -> dict[str, dict[str, list[str]]]
        values = {}  # dict[str, dict[str, list[str]]]
        file_path = os.path.join(path, KEYMAP_MODIFICATIONS_FILENAME)
        if os.path.exists(file_path):
            values = getKeymapValues(KEYMAP_MODIFICATIONS_FILENAME, file_path)
            # Update shortcuts in application
            Keymap.set_(values)
        else:
            logger.debug(f'Current keymap {file_path} not found. This is\
 normal on first launch or if it has not been saved yet. Falling back to\
 default values.')
        return values or self.default_values
```
where `KEYMAP_MODIFICATIONS_FILENAME = '__current_keymap__.json'`

`self.values` is never modified, so I _think_ it's ok to return default values like that without deepcopying.

Even though `getKeymapValues` checks for existence of the file already, it logs a warning if it can't find it, and here it's normal for it to not be found before it has been created.

Now we are loading from this file, but we are not yet creating or saving it. Our `saveCurrent` currently only does:
```python
    def saveCurrent(self):
        # type: (KeymapWidget) -> None
        Keymap.set_(self.get())
```
We already have the method `_save` to save a keymap preset, and `exportCurrent` is very similar to what we're trying to do, it suffices to modify it in the same vein.
```python
    def saveCurrent(self, path):
        # type: (KeymapWidget) -> None
        # Get values dict
        values = self.get()
        # Serialise
        res = json.dumps(values, indent=2)
        # Write to file in path
        file_path = os.path.join(path, KEYMAP_MODIFICATIONS_FILENAME)
        self._save(file_path, res)
        # Update keymap in application
        Keymap.set_(values)
```
Note that I had to add a path argument. This gets called in `CustomisationDialog.accept`, which has access to the user dir.
```python
        # Save keymap
        self.keymap.saveCurrent(getStylePath(self.getUserDir()))
```

I at first accidentally saved it to `THEME_MODIFICATIONS_FILENAME` instead of `KEYMAP_MODIFICATIONS_FILENAME`, and the only thing that happened is it changed to the default theme next launch. It's good to know it can handle it, I guess. (What, it can handle loading a json file as qss? It didn't even complain)

There's one more thing to sort out: when loading the presets we should ignore `__current_keymap__`, so that it doesn't show up in the list of presets.
```python
        prefix_to_exclude = KEYMAP_MODIFICATIONS_FILENAME.rsplit('.', 1)[0]
        for km in Keymap.keymaps:
            if not km.startswith(prefix_to_exclude):
                keymaps.addItem(km)
```

## Revert and restore defaults
The customisation dialog has a revert button and a restore defaults buttons, and they don't react at all to changes we make to the keymap.

What does revert mean? That if you diverge from the save, the revert button should activate, and pressing it should return the state of all the controls to what it was after the last save.

In addition to being able to revert everything, the ThemeWidget also has the ability to revert each selector individually: when you change a colour to differ from the save, a revert button appears next to it. For keymap I don't think we'll implement that because the UI is already stacked, and we also have the ability to add and remove entries, so it's unclear what the individual revert button would do for anything other than the "main" undeletable shortcut, and where it would be positioned. I think this functionality was actually inspired by Blender shortcuts, though I'm not sure if it's reverting to save or to keymap preset. I don't recall it being like that before, but I just checked and the UI has a small X button next to each shortcut, and if you modify the shortcut it changes to a left arrow instead, so if you want to delete a modified shortcut you have to first press revert so that the button turns back to delete then press delete. And if you create a new shortcut that didn't exist, the button shows both X and the arrow, and pressing it removes the shortcut in one click.

That also makes me think that our remove buttons should be smaller.

Let's not over complicate things and just make the global revert button work.

But actually for the global revert button it's going to be just as complicated with the way we have things set up. We don't have a data structure holding all our edits. But we could connect their `changed` signal to a method when we instantiate them.

I'm thinking to save `entries_map` in a property on KeymapSelectorWidget, connect each of the edits (including argstr edits) to a method `_editChanged`, and there compare `self.get()` to `self.entries_map`, emitting `self.changed` when it differs. And how would we make it go back to not being revertable when it no longer differs (without the user having pressed revert, just changed the value to what it was before)?

In theme, the ThemeSelectorWidgets subdivide further into widgets called CEdit which just contain the colour box, and a hidden revert button [and it actually subdivides further, to `_CEdit` widgets that contain just the colour box, which CEdit stores as `self.inner`]. On change, they do this:
```python
    def handleChange(self):
        # type: (CEdit) -> None
        if self.inner.current_c == self.inner.committed_c:
            self.committed = True
            self.btn.hide()
        else:
            self.committed = False
            self.btn.show()
        self.changed.emit()
```
Then each ThemeSelectorWidget connects each of the CEdits' `changed` signal to:
```python
    def _ceditChanged(self):
        # type: (ThemeSelectorWidget) -> None
        all_committed = True
        for cedit in self.cedits.values():
            if cedit.committed is False:
                self.committed = False
                all_committed = False
                break
        if all_committed is True:
            self.committed = True
        self.changed.emit()
```
The ThemeWidget connects each selector widget's `changed` signal to:
```python
    def _selectorWidgetChanged(self):
        # type: (ThemeWidget) -> None
        all_committed = True
        for s in self.selector_widgets.values():
            if s.committed is False:
                self.committed = False
                all_committed = False
                break
        if all_committed is True:
            self.committed = True
        self.changed.emit()
```

The main thing I note here is CEdit's handleChange emits change always, and additionally it hides or unhides its revert button (and sets self.committed).

So to start with let's write a `handleChange` button on the selector widget that does something similar:
```python
    def handleChange(self):
        # type: (KeymapSelectorWidget) -> None
        if self.get() == self.entries_map:
            self.committed = True
            self.revertbtn.hide()
        else:
            self.committed = False
            self.revertbtn.show()
        self.changed.emit()
```
And I'm actually going to add a revert button anyway. Just one for each selector, on the first row, next to the selector name.
```python
    def __init__(self,  # type: KeymapSelectorWidget
                 selector_name,  # type: str
                 entries_map,  # type: dict[str, list[str]]
                 parent=None  # type: QWidget | None
                 ):
        # type: (...) -> None
        QWidget.__init__(self, parent)
        self.entries_map = entries_map
        self.committed = True
        self.lyt = QFormLayout(self)
        self.lyt.setContentsMargins(0, 0, 0, 0)
        self.revertbtn = QToolButton()
        self.revertbtn.setArrowType(Qt.ArrowType.LeftArrow)
        self.revertbtn.setFixedSize(16, 13)
        self.revertbtn.setToolTip("Revert")
        self.revertbtn.hide()
        self.lyt.addRow(QLabel(f'<b>{selector_name}</b>'), self.revertbtn)
        # [..]
```

For each key sequence editor that is created, `editor.keySequenceChanged.connect(self.handleChange)`

For each eargstr, `eargstr.textChanged.connect(self.handleChange)` (other than the first one, which is disabled and not supposed to be ever edited)

Adding and removing entries should also call handleChange. But at the start we call addEntry programmatically, and we don't want it to trigger then. I guess maybe we won't call it on addEntry at all, because it's just a blank entry that gets added, it's not until the key sequence editor or argstr are changed that we care about the changes.

But I'll connect it when removing rows. Instead of `rembtn.clicked.connect(lambda: self.lyt.removeRow(w))`, we'll connect a method `removeEntry`: `rembtn.clicked.connect(lambda: self.removeEntry(w))`, which will remove the row and call handleChange.
```python
    def removeEntry(self, widget):
        # type: (KeymapSelectorWidget, QWidget) -> None
        """widget: no matter which widget on that row"""
        self.lyt.removeRow(widget)
        self.handleChange()
```

Some of the selectors show the revert button from the start, even though I haven't changed them

I also see a bunch of `get` errors "getting first editor failed".

We could add `self.initialised = False` to the top of the initialiser, `self.initialised = True` to the bottom, and
```python
        if not self.initialised:
            return
```
in handleChange.

It seems to work now. Confirmed that removing the entries of `Menu.setViewByEnum` unhides the revert button, and readding them (1 : Ctrl+1, 2 : Ctrl+2) hides it again.

To revert, we could just use the existing `set_` method and pass to it `self.entries_map`.
```python
    def revert(self):
        # type: (KeymapSelectorWidget) -> None
        self.set_(self.entries_map)
```
and connect it in the initialiser
```python
self.revertbtn.clicked.connect(self.revert)
```
It works, but the revert button doesn't go away. It looks like handleChange doesn't get called, which is I guess good? We could hide the revert button at the end of revert, but is this cheating?

If you change the main shortcut and then revert, then handleChange does get called, otherwise no.
- If I modify main shortcut and then revert, the revert button goes away.
- If I change an existing additional shortcut (whether the key sequence or the argstr) and then revert, the revert button doesn't go away.
- If I add another additional shortcut, change it, then revert, the revert button doesn't go away either.

It only goes away when you revert if the main shortcut had been modified.

I guess because that's the only widget that stays the same? The others get removed and readded.

I think I will simply call `self.handleChange()` myself. Maybe it's a bit wasteful, but it feels wrong to me to explicitly hide the revert button without having checked that the data matches the initial state. It will also correctly update `self.committed`. If we want to avoid the extra call in `set_`, we could set `self.initialised` to False again, but I'm not going to bother.

I noticed that if we add several empty entries and delete them such that one is still there, the revert button appears. I think that empty entries should be ignored and not affect the revert button. Also, if we save with empty entries they get saved. We should probably filter them out in `get`. It's sufficient to just add `if s` before appending:
```python
    def get(self):
        # type: (KeymapSelectorWidget) -> dict[str, list[str]]
        values = {}
        for argstr, s, _, _ in self.widgets():
            if values.get(argstr):
                if s:
                    values[argstr].append(s)
            else:
                values[argstr] = [s]
        return values
```

Now in the KeymapWidget we could connect all the selectors to a `_selectorWidgetChanged` method, similar to what ThemeWidget has.

(The category widget doesn't need to do anything, it only serves as a tab and doesn't instantiate nor store the selectors, other than on its layout. It's KeymapWidget that does all the work. It could have been just a QWidget instead of subclassing)

It's the exact same as the method on ThemeWidget, the only thing I changed is the type.
```python
    def _selectorWidgetChanged(self):
        # type: (KeymapWidget) -> None
        all_committed = True
        for s in self.selector_widgets.values():
            if s.committed is False:
                self.committed = False
                all_committed = False
                break
        if all_committed is True:
            self.committed = True
        self.changed.emit()
```
And ensure it has a changed signal:
```python
class KeymapWidget(QWidget):
    changed = pyqtSignal()
    # [..]
```

Then
- add `self.committed = True` to KeymapWidget's initialiser
- connect each one we instantiate with `selector_widget.changed.connect(self._selectorWidgetChanged)`

We also need a revert method on KeymapWidget which is simply going to call revert on each of the selector widgets. This is again the exact same function as on ThemeWidget.
```python
    def revert(self):
        # type: (KeymapWidget) -> None
        for s in self.selector_widgets.values():
            if s.committed is False:
                s.revert()
```
No need to change `self.committed` here, it will get updated by `_selectorWidgetChanged`, since each of the selector reverts emit the `changed` signal on each selector.

To make the global revert button change state, `ConfigurationDialog._themeSettings` does:
```python
self.theme.changed.connect(self.themeUpdate)
```
```python
    def themeUpdate(self):
        # type: (CustomisationDialog) -> None
        revert = restore = False

        if self.theme.committed is False:
            revert = True
        else:
            revert = self.config_edited != self.config

        if self.theme.isDefault() is False:
            restore = True
        else:
            restore = self.config_edited != DEFAULTS

        self.revert_btn.setEnabled(revert)
        self.restore_btn.setEnabled(restore)
```

Ignoring restore defaults for now,
```python
self.keymap.changed.connect(self.keymapUpdate)
```
```python
    def keymapUpdate(self):
        # type: (CustomisationDialog) -> None
        revert = False

        if self.keymap.committed is False:
            revert = True
        else:
            revert = self.config_edited != self.config

        self.revert_btn.setEnabled(revert)
```

The revert button now updates its state correctly, but clicking on it doesn't revert. For that, in `ConfigurationDialog.revert`, there is a call to `self.theme.revert()`. We can add after it `self.keymap.revert()`, which we already implemented.

The save button makes the global revert button grey out, but it doesn't make the little revert buttons on the KeymapSelectorWidgets go away. Theme handles this in saveCurrent by calling commitCurrent on all the CEdits. It sets `self.committed_c = self.current_c`, self.committed to True, and hides the revert button. I guess our equivalent is to do `self.entries_map = self.get()`
```python
    def commitCurrent(self):
        # type: (KeymapSelectorWidget) -> None
        self.entries_map = self.get()
        self.committed = True
        self.revertbtn.hide()
```
and add to KeymapWidget's saveCurrent:
```python
        # Commit changes
        for name, widget in self.selector_widgets.items():
            widget.commitCurrent()
        self.committed = True
```
I guess we could also update self.values with self.get(), but they're actually not used for anything other than initially populating the widgets. So I will remove the property and just make it a local.

That's it for revert.

For restore defaults, let's first modify `CustomisationDialog.keymapUpdate` with what it should have had earlier to match themeUpdate:
```python
    def keymapUpdate(self):
        # type: (CustomisationDialog) -> None
        revert = restore = False

        if self.keymap.committed is False:
            revert = True
        else:
            revert = self.config_edited != self.config

        if self.keymap.isDefault() is False:
            restore = True
        else:
            restore = self.config_edited != DEFAULTS

        self.revert_btn.setEnabled(revert)
        self.restore_btn.setEnabled(restore)
```

ThemeWidget's isDefault method:
```python
    def isDefault(self):
        # type: (ThemeWidget) -> bool
        res = True
        values = self.get()
        for selector_name, props in self.default_values.items():
            for prop_name, value in props.items():
                v = values.get(selector_name, {}).get(prop_name)
                if v:
                    res = QColor(v) == QColor(value)
                else:
                    logger.warning('isDefault: Theme values asymmetry in '
                                   f'{selector_name} / {prop_name}')
                    res = False
                if res is False:
                    return res
        return res
```

For keymap it's a lot simpler.
```python
    def isDefault(self):
        # type (KeymapWidget) -> bool
        return self.get() == self.default_values
```

Now the restore defaults button state updates correctly, but clicking on it doesn't affect keymap.

For theme, `self.theme.restoreDefaults()` is called in `ConfigurationDialog.restoreDefaults`. Let's add `self.keymap.restoreDefaults()` after it.

And again have a look at what ThemeWidget does, so we can copy its homework.
```python
    def restoreDefaults(self):
        # type: (ThemeWidget) -> None
        for selector_name, props in self.default_values.items():
            for prop_name, value in props.items():
                s = self.selector_widgets.get(selector_name)
                if s:
                    cedit = s.cedits.get(prop_name)
                    if cedit:
                        cedit.set_(QColor(value))
                    else:
                        logger.error('restoreDefaults: Missing cedit for '
                                     f'{prop_name} in {selector_name}')
                else:
                    logger.error('restoreDefaults: Missing selector widget '
                                 f'for {selector_name}')
```

It's again a lot simpler than that for keymap.
```python
    def restoreDefaults(self):
        # type: (KeymapWidget) -> None
        values = self.default_values
        for name, widget in self.selector_widgets.items():
            v = values.get(name, {})
            widget.set_(v)
```
But I guess since we don't expect a selector to ever be missing from the default values, we should at least add a warning.
```python
    def restoreDefaults(self):
        # type: (KeymapWidget) -> None
        values = self.default_values
        for name, widget in self.selector_widgets.items():
            v = values.get(name, None)
            if v is None:
                logger.warning('KeymapWidget.restoreDefaults: Default values '
                               f'missing for selector {name}')
                continue
            widget.set_(v)
```

## Menu.setViewByEnum:2 bug
Menu.setViewByEnum:1 is getting bound, but not Menu.setViewByEnum:2. The action is in the menu and functions, so it's being created correctly, appears in customisation dialog correctly with argstr "2", but the action gets no shortcut on it (no shortcut appears in the menu, and Ctrl+2 doesn't do anything).

I was very confused about what it is, until I tried to reverse the shortcuts in menu's makeAction and saw it's not doing anything, because keymapUpdate is called after -- so long as the user has a `__current_keymap__.json`. if we remove that file, then it works again. So it's a bug in keymapUpdate. Though debug prints there are normal too.

It's not necessarily a bug in keymapUpdate itself but something else in the updating logic.

I noticed another bug: If I remove `__current_keymap__.json` and save CustomisationDialog without having changed anything, everything in Menu is unassigned except for Menu.setViewByEnum 1 and 2 (but, as with the previous bug, only 1 actually works). In BookView, both zoomIn and zoomOut are there, but only zoomIn actually works.

Let's trace what happens when we save.
- CustomisationDialog.accept
- self.keymap.saveCurrent(getStylePath(self.getUserDir()))
- KeymapWidget.saveCurrent
- KeymapWidget.get
- Keymap.set_(values)
- Keymap.selectors[name].set_(d)
- Keymap.notifier.changed.emit()
- MenuController.keymapUpdate (or BookView.keymapUpdate)

Printing the values out of KeymapWidget.get, it seems like the problem for that particular issue is there, although it doesn't explain everything. It has the Ctrl+1, Ctrl+2, Ctrl++, and Ctrl+- shortcuts only (on the right actions). This doesn't explain why only Ctrl+1 and Ctrl++ work. But at least it gives us something to try and fix before continuing to investigate the rest of the bizarreries.

If I remove `__current_keymap__.json` and launch retype, the customisation UI has those aforementioned shortcuts and not the others, so it looks like not `get` which is wrong but the UI that is not populated properly. Or is it? Let's ensure `KeymapWidget.default_values` is what we think it is. They're not. Only the aforementioned shortcuts, and something else interesting is instead of Ctrl++ and Ctrl+-, they have 16 and 17.

`default_values` is obtained from Keymap.getValuesDict(). It's a very simple method:
```python
    @staticmethod
    def getValuesDict():
        # type: () -> ValuesDict
        values = {}  # type: ValuesDict
        for name, k in Keymap.selectors.items():
            values[name] = {**k.entries_map}
        return values
```
It won't be Keymap.selectors itself that is wrong, because if we launch retype without a `__current_keymap__.json`, then all the shortcuts defined in the decorators work as expected. And yet `dict(zip(Keymap.selectors.keys(), [k.entries_map for k in Keymap.selectors.values()]))` matches `default_values`.

How come, for example, Ctrl+O works, even though it's not in Keymap.selectors?

Wait, I'm printing the selectors in `getValuesDict`, maybe something else is changing them beforehand.
- Printing the same command from the end of MenuController's initialiser, the result is the same (no Ctrl+O)
- Printing it before the loop that creates the actions, the result is different. Ctrl+O is there.

Ah, oups. It's me who introduced this bug while testing the first one. I did `action.setShortcut(QKeySequence(shortcuts.pop()))` just to see quickly if that would work (as opposed to `setShortcuts`), the pop is laziness to avoid the error when shortcuts is empty.

The bug with Ctrl+2 not working after saving still exists though, and now instead of Ctrl++ working and Ctrl+- not working, it's the inverse. Is this going to be random? No, it's like this every time (I deleted `__current_keymap__.json`, relaunched and tested again several times. Before save they both work, after save only Ctrl+- works. Same situation with Ctrl+1 and Ctrl+2.)

Debug print:
```python
                if action:
                    action.setShortcuts(s)
                    print(f"keymapUpdate: {s}, {name}, {action.text()}, {action.shortcut().toString()}")
```
and everything is normal, namely Ctrl+1 and Ctrl+2 get assigned correctly. I really don't understand what's wrong. Something changes it after?

To be able to check the shortcut in real time, let's try this:
```python
        q = self._makeAction('test', lambda: print(self.actions['Menu.setViewByEnum:2']['action'].shortcut().toString()))
        q.setShortcuts(['Ctrl+Q'])
        helpMenu.addAction(q)
```
and it's empty. Whereas if I change it to `Menu.setViewByEnum:1`, it prints its shortcut correctly. If I do this same print in the same place in the code directly, I get Ctrl+2 correctly. Placing this same print at the beginning and end of keymapUpdate: at the beginning it's Ctrl+2, at the end it's empty. But inside of the loop I'm printing each shortcut that gets assigned, and it seemingly gets assigned fine (the last one there is action.shortcut().toString()):
```text
keymapUpdate: ['Ctrl+1'], Menu.setViewByEnum:1, &Shelf View, Ctrl+1
keymapUpdate: ['Ctrl+2'], Menu.setViewByEnum:2, &Book View, Ctrl+2
```

Printing it inside the loop right after action.setShortcuts, it's empty the next iteration:
```text
keymapUpdate: ['Ctrl+2'], Menu.setViewByEnum:2, &Book View, Ctrl+2
mid keymapupdate Ctrl+2
keymapUpdate: [''], Menu.toggleConsoleWindow, &Book View, 
mid keymapupdate 
```
Hang on a minute, why is `Menu.toggleConsoleWindow` even appearing here? This is inside a `if action:`, and it doesn't get an action created on Linux. Apparently it does, and it gets the same action of `Menu.setViewByEnum:2`. Ah, I see what is happening here. Look at the last line of our actions instantiation loop:
```python
        for name, info in self.actions.items():
            n = name.split(':')
            selector_name = n[0]
            argstr = n[1] if len(n) > 1 else ''
            info['shortcuts'] = Keymap.s(selector_name).s(argstr)
            if info.pop('condition', True):
                before = info.pop('before', None)
                if before:
                    before()
                action = self._addAction(**info)
            info['action'] = action
```
Stupidly, I actually had that thought the other day and moved it in to be inside the if, then changed my mind.

We could either reset action to None at the start of each loop, or indent the last line. Due to issues with typing, I'd rather indent the last line, and change the check in keymapUpdate. Actually it doesn't even need to change, as we already do `d.get('action')`.

Indenting the last line fixed the issue with Ctrl+1, but not with Ctrl++/Ctrl+- (it flipped again, it's now zooming in that doesn't work).

Ah, that one I think is my keyboard layout (gb). I think I just got confused, because Ctrl++ requires Shift, but Ctrl+- doesn't. It seems to work the same with or without `__current_keymap__.json`. "It flipped again" = me pressing Shift for both or not pressing Shift for both, without even realising that I'm doing it.

## Handling arbitrary argstrs
By design, argstrs can be anything, and it's up to the widgets concerned to handle them, or not. But currently we're not even handling cases that you'd expect to be handled, like numerical arguments.

- setViewByEnum (numerical argument, but only 1 and 2 are would currently do anything)
- gotoCursorPosition (m/move)
- nextChapter (m/move)
- previousChapter (m/move)

In addition, there are these two console commands that currently not defined in the keymap where this problem would become more evident:

- load (numerical argument)
- setChapter (numerical argument)

The function associated with setViewByEnum can take numerical argument 1-4.

The changes we make to support this should be in keymapUpdate, because as we've seen with the bug in the previous section, this is what gets called after the user's keymap is loaded. The actions that get created before that is just the stuff defined in the decorators by default, so we won't ever need to create extra QActions to support other argstrs there. So in keymapUpdate we need to check if there are argstrs in the new keymap we don't have a QAction for, create, and add them. But add them to what? As if we add them to a menu, they will appear in the menu, and we don't want that. MenuController is a QObject, and it's not possible to addAction to that. Maybe `self._menu.addAction`, or `self.controller.addAction`? The latter won't work because MainController is a QObject too. The former works (and doesn't appear in the menu).
```python
        q = self._makeAction("test", lambda: print("hi"))
        q.setShortcuts(['Ctrl+Q'])
        self._menu.addAction(q)
```

This is keymapUpdate currently:
```python
    def keymapUpdate(self):
        # type: (MenuController) -> None
        if not hasattr(self, 'actions'):
            logger.warning("keymapUpdate called with no actions set")
            return
        for name, d in self.actions.items():
            n = name.split(':')
            selector_name = n[0]
            argstr = n[1] if len(n) > 1 else ''
            try:
                s = Keymap.s(selector_name).s(argstr)
                d['shortcuts'] = s
                # Action could be None for OS-specific actions
                action = d.get('action')
                if action:
                    action.setShortcuts(s)
                else:
                    logger.debug(f"Skip updating non-existing action {name}")
            except (KeyError, AttributeError):
                logger.error(f"Updating k '{name}' failed. "
                             f"{traceback.format_exc()}")
```
I think instead of iterating through self.actions, we should be iterating through Keymap.getValuesDict. Actually no, because we still only want to support commands that are in self.actions, just other argstrs of the same commands.

We could add this to the end of the loop:
```python
            # Support other argstrs
            for argstr, s in Keymap.s(selector_name).entries():
                if not s or s == ['']:
                    continue
                combined_name = selector_name
                combined_name += f':{argstr}' if argstr else ''
                if combined_name in self.actions:
                    continue
                regex = d.get('args_regex')
                func = d.get('args_func')
                if not (regex and func) or not re.match(regex, argstr):
                    continue
                a = self._makeAction(combined_name, lambda: func(argstr))
                action.setShortcuts(s)
                self._menu.addAction(action)
                logger.debug(f'keymapUpdate: Created custom action {action}')
```

{% include note.html content='
> [!NOTE]
> Problem with the lambda. Should be `lambda _, f=func, a=argstr: f(a)` (in the first argument Qt passes a boolean, and the other two will be by reference instead of value, and they are temporary variables, so we must copy them).
' %}

Then the actions map, add `args_regex` and `args_func` to just Menu.setViewByEnum:1
```python
            'Menu.setViewByEnum:1': {
                'menu': viewMenu, 'name': '&Shelf View',
                'func': lambda: self.controller.setViewByEnum(1),
                'icon': 'shelf_view',
                'args_regex': r'[1-4]',
                'args_func': lambda d: self.controller.setViewByEnum(int(d)),
            },
```

This doesn't work, because when the actions are initially created we're passing everything in the dictionary as keyword arguments.
```python
        for name, info in self.actions.items():
            n = name.split(':')
            selector_name = n[0]
            argstr = n[1] if len(n) > 1 else ''
            info['shortcuts'] = Keymap.s(selector_name).s(argstr)
            if info.pop('condition', True):
                before = info.pop('before', None)
                if before:
                    before()
                action = self._addAction(**info)
                info['action'] = action
```
We could remove and readd them later, but it feels wrong. Maybe modify `_makeAction` and `_addAction` to take arbitrary `**kwargs` after its list of arguments and ignore them? Because also seeing the condition there reminds me that we should be checking that before creating QActions in our new thing also, so we should not pop condition there. On the other hand, popping 'before' is fine because it's only needed for the actions that appear in the UI.

So let's modify the first `pop` to `get`, and add `**_` to the end of the arguments list of the make and add action utility functions.

And in the actions loop in keymapUpdate, instead of doing this:
```python
                # Action could be None for OS-specific actions
                action = d.get('action')
                if action:
                    action.setShortcuts(s)
                else:
                    logger.debug(f"Skip updating non-existing action {name}")
```
Add a check of `d.get('condition', True)` to the beginning, returning straight away if not. Sorry, not returning!!! `continue`! (This mistake just cost me a bit of time)

And another thing I spent ages on is errors with the lambda when triggering the QAction (None object is not callable). It seems it's because the variables are temporary so we must pass them in arguments to copy: `lambda f=func, a=argstr: f(a)` but this still fails with bool object is not callable. `lambda _, f=func, a=argstr: f(a)` works.

And then I realised setViewByEnum actually only supports 1 or 2, so this was pointless.
```text
    return self.views[View(view)]
           ~~~~~~~~~~^^^^^^^^^^^^
KeyError: <View.typespeed_view: 3>
```

In theory though, we could use this same logic to support the others:
- gotoCursorPosition (m/move)
- nextChapter (m/move)
- previousChapter (m/move)

These are all on BookView. Now that keymapUpdate is large, the code duplication issue becomes more unignorable. I guess I will after all convert it to a utility function on keymap.py instead of having it be a method on each widget. We can pass the actions dictionary and the widget to add the actions to as arguments. `_makeAction` should be converted to a utility function placed elsewhere. I will even move the loop where we initially create the widget's default actions to a utility function, because it's essentially the same thing on MenuController and BookView. Not sure where to put it though, keymap or the new utility actions.py file I created for makeAction? I think keymap because it's got keymap-specific stuff, even though it's slightly out of scope.

```python
def genActions(actions):
    # type: (dict[str, ActionInfo], QWidget) -> None
    """Utility function to generate QActions from a ActionInfo dict"""
    for name, info in actions.items():
        n = name.split(':')
        selector_name = n[0]
        argstr = n[1] if len(n) > 1 else ''
        info['shortcuts'] = Keymap.s(selector_name).s(argstr)
        if info.get('condition', True):
            before = info.pop('before', None)
            if before:
                before()
            action = makeAction(**info)
            info['action'] = action


def keymapUpdate(actions, widget):
    # type: (dict[str, ActionInfo], QWidget) -> None
    """Utility function to update QActions"""
    for name, d in actions.items():
        if not d.get('condition', True):
            continue
        n = name.split(':')
        selector_name = n[0]
        argstr = n[1] if len(n) > 1 else ''
        try:
            s = Keymap.s(selector_name).s(argstr)
            d['shortcuts'] = s
            action = d['action']
            action.setShortcuts(s)
        except (KeyError, AttributeError):
            logger.error(f"Updating k '{name}' failed. "
                         f"{traceback.format_exc()}")
        # Support other argstrs
        for argstr, s in Keymap.s(selector_name).entries():
            if not s or s == ['']:
                continue
            combined_name = selector_name
            combined_name += f':{argstr}' if argstr else ''
            if combined_name in actions:
                continue
            regex = d.get('args_regex')
            func = d.get('args_func')
            if not (regex and func) or not re.match(regex, argstr):
                continue
            action = makeAction(
                combined_name, lambda _, f=func, a=argstr: f(a))
            action.setShortcuts(s)
            widget.addAction(action)
            logger.debug(
                f'keymapUpdate: Created custom action {combined_name}')
```
ActionInfo type is:
```python
    ActionInfo = TypedDict(
        'ActionInfo',
        {'menu': QMenu, 'name': str, 'func': Callable[[], None],
         'tooltip': str, 'shortcuts': list[str], 'icon': str,
         'action': QAction, 'condition': bool, 'before': Callable[[], None],
         'args_regex': str, 'args_func': Callable[[str], None]},
        total=False)
```

It's a problem that we're never deleting the actions. And since the custom ones we've created don't get added to actions dict, duplicate ones will be created each keymapUpdate.

I guess we could create entries in the ActionInfo dictionary with the key as the combined name that are only going to have the action set and nothing else.
```python
actions[combined_name] = {'action': action}
```

Or not:
> RuntimeError: dictionary changed size during iteration

We could put them in a temporary structure then add them at the end, but we're also going to need a way to delete the ones that are no longer bound each time, which we don't want to do with the original actions, so should we keep it in a separate structure? I kind of don't want to. We could have a different way to indicate they should be deleted, like "Custom" in name field, but it's a bit risky, and also I'm not sure if you're allowed to have several QActions with the same name. Probably not. [They don't have names. It's actually just a descriptive text field, and yes you can have several with the same text, or none at all.]

Something potentially cursed comes to mind: Python functions are object like any other, so they can store state. We could store a list of the actions we create on the function itself, and delete them every time keymapUpdate gets called.
```python
def keymapUpdate(actions, widget):
    # type: (ActionsInfo, QWidget) -> None
    """Utility function to update QActions shortcuts"""
    logger.debug(f'keymapUpdate for {type(widget)}')
    # 1. Remove any previously created custom actions
    custom_actions = getattr(keymapUpdate, 'custom_actions', [])
    for a in custom_actions:
        a['widget'].removeAction(a['action'])
    custom_actions = []
    # Update bindings for actions in actions dictionary
    for name, d in actions.items():
        if not d.get('condition', True):
            continue
        n = name.split(':')
        selector_name = n[0]
        argstr = n[1] if len(n) > 1 else ''
        try:
            s = Keymap.s(selector_name).s(argstr)
            d['shortcuts'] = s
            action = d['action']
            action.setShortcuts(s)
        except (KeyError, AttributeError):
            logger.error(f"Updating k '{name}' failed. "
                         f"{traceback.format_exc()}")
        # Create custom actions based on bindings that lack actions
        for argstr, s in Keymap.s(selector_name).entries():
            if not s or s == ['']:
                continue
            combined_name = selector_name
            combined_name += f':{argstr}' if argstr else ''
            if combined_name in actions:
                continue
            regex = d.get('args_regex')
            func = d.get('args_func')
            if not (regex and func) or not re.match(regex, argstr):
                continue
            action = makeAction(
                combined_name, lambda _, f=func, a=argstr: f(a))
            action.setShortcuts(s)
            custom_actions.append({'action': action, 'widget': widget})
            widget.addAction(action)
            logger.debug(
                f'keymapUpdate: Created custom action {combined_name}')
    keymapUpdate.custom_actions = custom_actions
    logger.debug(f'keymapUpdate.custom_actions = {custom_actions}')
```
Would have been nice to do a dictionary like `action: widget`, but QActions are non-iterable.

{% include note.html content='
> [!NOTE]
> This is broken, as I find out in a [later section](#add-load-and-setchapter). There is a keymapUpdate call for each widget that connects to the notifier, so if we have actions on several widgets, each call deletes the custom actions of the previous.
' %}

Let's add `args_regex` and `args_func` for gotoCursorPosition on BookView, because I want to see if it will work.
```python
            {
                'name': 'Cursor position',
                'func': self.gotoCursorPosition,
                'tooltip': 'Go to the cursor position. Hold Ctrl to move\
 cursor to your current position',
                'icon': 'cursor',
                'args_regex': '(m|move)',
                'args_func': lambda m: self.gotoCursorPosition(move=True),
            },
```
It does. Verified that it stops working after deleting it too.

Next and previous chapter are pretty much the same.
```python
            'BookView.previousChapter':
            {
                'name': 'Previous chapter',
                'func': self.previousChapterAction,
                'tooltip': 'Go to the previous chapter. Hold Ctrl to move\
 cursor with you as well',
                'icon': 'arrow-left',
                'args_regex': '(m|move)',
                'args_func': lambda m: self.previousChapter(move_cursor=True),
            },
            'BookView.nextChapter':
            {
                'name': 'Next chapter',
                'func': self.nextChapterAction,
                'tooltip': 'Go to the next chapter. Hold Ctrl to move cursor\
 with you as well',
                'icon': 'arrow-right',
                'args_regex': '(m|move)',
                'args_func': lambda m: self.nextChapter(move_cursor=True),
            },
```

It's useful binding them to PgUp and PgDown, and with Ctrl to move the cursor. Maybe I will include that as default bindings.

You may have noticed that `func` and `args_func` are too different functions for these commands. This is because the actions in the BookView toolbar pass True to move cursor if the user is holding Ctrl. For gotoCursorPosition it's the same function, which is undesirable, because if you bind to it a keyboard shortcut that involves Ctrl, it acts as if you passed m/move in the arguments.

Actually if I bound next/previous chapter without moving the cursor to something with Ctrl, it would have hte same problem. We need some way to only take into account Ctrl if the button was clicked on, rather than invoked via a keyboard shortcut. Unfortunately it's nontrivial. Would have to install an event filter and I don't think it's worth it just for that.

Maybe duplicates of those actions, toolbar and non-toolbar variants, and only add the toolbar variants to the toolbar, the rest to the BookView widget (so that they don't appear in any UI as it's not a toolbar or a menu).

Keymap is unhappy about it, because then we search for shortcuts for a command that doesn't exist in the keymap.

> 07:16:54.83 [retype.services.keymap] WARNING: Invalid keymap selector name BookView.gotoCursorPosition(ToolbarVariant)

We could keep it as a single entry in the ActionInfo dictionary, and introduce a new field `func_ui` where if it's present, create another QAction with that function and don't set shortcuts to it. But one of these QActions will need to be added to the toolbar and the other to the BookView widget, and I don't know how to communicate that information.

Our ActionInfo dictionary is getting annoyingly complicated, but maybe something like this?
```python
            'BookView.gotoCursorPosition':
            {
                'name': 'Cursor position',
                'func': self.gotoCursorPosition,
                'func_ui': self.gotoCursorPositionAction,
                'tooltip': 'Go to the cursor position. Hold Ctrl to move\
 cursor to your current position',
                'icon': 'cursor',
                'widget': self.toolbar,
                'widget_ui': self,
                'args_regex': '(m|move)',
                'args_func': lambda m: self.gotoCursorPosition(move=True),
            },
```
```python
ActionInfo = TypedDict(
    'ActionInfo',
    {'menu': QWidget, 'widget': QWidget, 'widget_ui': QWidget, 'name': str,
     'func': Callable[[], None], 'func_ui': Callable[[], None],
     'tooltip': str, 'shortcuts': list[str], 'icon': str,
     'action': QAction, 'action_ui': QAction,
     'condition': bool, 'before': Callable[[], None],
     'args_regex': str, 'args_func': Callable[[str], None]},
    total=False)
```
I had to also add `action_ui`, because BookView needs to be able to access next/previous chapter UI action to be able to visually disable the button when we're on the first/last chapter (and thus pressing the button would do nothing as we can't go further).
```python
def genActions(actions, widget=None):
    # type: (ActionsInfo, QWidget | None) -> None
    """Utility function to generate QActions from a ActionInfo dict"""
    for name, info in actions.items():
        n = name.split(':')
        selector_name = n[0]
        argstr = n[1] if len(n) > 1 else ''
        info['shortcuts'] = Keymap.s(selector_name).s(argstr)
        if info.get('condition', True):
            before = info.pop('before', None)
            if before:
                before()
            widget = info.pop('widget', None) or widget
            widget_ui = info.pop('widget_ui', None)
            func = info.pop('func', None)
            func_ui = info.pop('func_ui', None)
            if widget_ui and func_ui:
                action_ui = makeAction(**info, widget=widget_ui, func=func_ui)
                info['action_ui'] = action_ui
            action = (makeAction(**info, widget=widget, func=func) if widget
                      else makeAction(**info, func=func))
            info['action'] = action
```

I'm not exactly happy about the complexity here. We're also popping the widgets and functions so they're not going to be available later, isn't this going to be a problem for keymapUpdate? I'm actually confused in general, if keymapUpdate is using `widget`, how does it work with MenuController, which doesn't have any `widget` in its ActionInfo dictionaries? it uses `menu` instead.

It oddly doesn't complain... I'm confused.

For menu I think it's broken and we just don't see this because we don't really have any argstrs there, only setViewByEnum which, as established, only takes 1 or 2 which are already bound by default. Menu parameter should be removed and MenuController should set it to widget.

However, that still doesn't explain how we're doing pop widget and keymapUpdate has no issue with that. This part should break:
```python
    # 1. Remove any previously created custom actions
    custom_actions = getattr(keymapUpdate, 'custom_actions', [])
    for a in custom_actions:
        print('aa', a['widget'])  # temp
        a['widget'].removeAction(a['action'])
    custom_actions = []
```
But it doesn't, it still has the widget somehow.

Ah, the `a` isn't a ActionInfo dictionary, we create it ourselves near the end of keymapUpdate:
```python
custom_actions.append({'action': action, 'widget': widget})
```
And where does it get that widget from? From the argument to keymapUpdate. `keymapUpdate(actions, widget)`. Widgets connect it like:
```python
        Keymap.notifier.changed.connect(
            lambda: keymapUpdate(self.actions, self))
```

So the only time we need the widget in ActionInfo is in genActions, which is only called once, in the initialiser. We don't need the widget in ActionInfo afterwards.

We're still passing shortcuts even when creating `action_ui` (since it gets added to ActionInfo). It still works, because the user shortcuts aren't there when genActions get called and only get set in keymapUpdate, which only sets them to `action`, not `action_ui`. But if we had any default keyboard shortcuts in the decorators that have Ctrl in them, we would still have the issue. We need to not have shortcuts in ActionInfo when we create `action_ui`.
```python
def genActions(actions, widget=None):
    # type: (ActionsInfo, QWidget | None) -> None
    """Utility function to generate QActions from a ActionInfo dict"""
    for name, info in actions.items():
        n = name.split(':')
        selector_name = n[0]
        argstr = n[1] if len(n) > 1 else ''
        s = Keymap.s(selector_name).s(argstr)
        if info.get('condition', True):
            before = info.pop('before', None)
            if before:
                before()
            widget = info.pop('widget', None) or widget
            widget_ui = info.pop('widget_ui', None)
            func = info.pop('func', None)
            func_ui = info.pop('func_ui', None)
            if widget_ui and func_ui:
                action_ui = makeAction(**info, widget=widget_ui, func=func_ui)
                info['action_ui'] = action_ui
            action = makeAction(**info, widget=widget, func=func, shortcuts=s)
            info['action'] = action
            info['shortcuts'] = s
```
Now that `menu` is no longer a parameter in ActionInfo and there is only `widget` and `widget_ui` to specify the widget, we don't need the:
```python
            action = (makeAction(**info, widget=widget, func=func) if widget
                      else makeAction(**info, func=func))
```
that I had before.

## Alignment bug
There's an alignment bug with our customisation UI for the keymap and I think I already know why. There is a gap between the left and right parts of the UI (argstrs part, and shortcut + remove button part) for Selectors with long names. I think this is because of the revert button that we add next to the selector name. It causes it to split into columns instead of being a spanning widget like it was before. If I'm right, the solution is to wrap the selector name and revert button in a widget, then add it as a spanning widget to the form layout.

At the start of KeymapSelectorWidget, instead of doing this:
```python
        self.revertbtn = QToolButton()
        self.revertbtn.setArrowType(Qt.ArrowType.LeftArrow)
        self.revertbtn.setFixedSize(16, 13)
        self.revertbtn.setToolTip("Revert")
        self.revertbtn.hide()
        self.revertbtn.clicked.connect(self.revert)
        self.lyt.addRow(QLabel(f'<b>{selector_name}</b>'), self.revertbtn)
```
First wrap the button and name in another widget:
```python
        namewidget = QWidget()
        namewidget_l = QHBoxLayout(namewidget)
        namewidget_l.addWidget(QLabel(f'<b>{selector_name}</b>'))
        self.revertbtn = QToolButton()
        self.revertbtn.setArrowType(Qt.ArrowType.LeftArrow)
        self.revertbtn.setFixedSize(16, 13)
        self.revertbtn.setToolTip("Revert")
        self.revertbtn.hide()
        self.revertbtn.clicked.connect(self.revert)
        namewidget_l.addWidget(self.revertbtn)
        self.lyt.addRow(namewidget)
```
Now the revert button appears on the far right instead of right after the name, but I don't mind it, it might be better in a consistent position like that.

## More default shortcuts
Over BookView:
```python
@keymap('BookView.gotoCursorPosition', K(['Ctrl+.']))
@keymap('BookView.switchToShelves', K())
@keymap('BookView.previousChapter', K(['PgUp'], {'m': ['Ctrl+PgUp']}))
@keymap('BookView.nextChapter', K(['PgDown'], {'m': ['Ctrl+PgDown']}))
@keymap('BookView.skipLine', K(['Ctrl+Return']))
@keymap('BookView.zoomIn', K([QKeySequence.StandardKey.ZoomIn]))
@keymap('BookView.zoomOut', K([QKeySequence.StandardKey.ZoomOut]))
```

Don't forget to export the keymap as the new default keymap. Also, I'm going to rename it `0_default.json` instead of `keymapdefault.json`, in order to conform with themes and iconsets, where in both cases the default is called `0_default` (`0_default.qss` in the case of themes, and `0_default` directory name in the case of iconsets).

Problem: For previousChapter and nextChapter, the alternate keybindings for argstr `m` are not going to be bound, because the actions get created in keymapUpdate, which only gets called on launch if the user has a `__current_keymap__` file (as otherwise `Keymap.set_` doesn't get called, and the notifier that we connect to `keymapUpdate` doesn't trigger), which only gets created after saving customisation. So on first launch, the Ctrl+PgUp Ctrl+PgDown bindings do nothing.

The relevant logic is in `KeymapWidget._loadValues`:
```python
    def _loadValues(self, path):
        # type: (KeymapWidget, str) -> dict[str, dict[str, list[str]]]
        values = {}  # dict[str, dict[str, list[str]]]
        file_path = os.path.join(path, KEYMAP_MODIFICATIONS_FILENAME)
        if os.path.exists(file_path):
            values = getKeymapValues(KEYMAP_MODIFICATIONS_FILENAME, file_path)
            # Update shortcuts in application
            Keymap.set_(values)
        else:
            logger.debug(f'Current keymap {file_path} not found. This is\
 normal on first launch or if it has not been saved yet. Falling back to\
 default values.')
        # Filter out selectors that don't exist
        values = values or self.default_values
        return {s: values[s] for s in self.default_values.keys()}
```
I guess we can move the `Keymap.set_(values)` call to right before the return, so that it gets called in both cases, and add a comment to avoid future confusion.
```python
        # [..]
        # Filter out selectors that don't exist
        values = values or self.default_values
        # Update shortcuts in application (both in the case the
        # current keymap file exists and the case it doesn't, because
        # the default keybinds may have argstrs that need actions
        # created)
        Keymap.set_(values)
        return {s: values[s] for s in self.default_values.keys()}
```
But it still doesn't work, because `Keymap.set_` doesn't emit `notifier.changed` if the values haven't changed, and they don't in this case, because it's the default values.

I guess we could just directly `Keymap.notifier.changed.emit()` in the else case.
```python
        else:
            logger.debug(f'Current keymap {file_path} not found. This is\
 normal on first launch or if it has not been saved yet. Falling back to\
 default values.')
            # In that case force keymapUpdate, because widgets may
            # have default bindings with argstrs that need actions
            # created
            Keymap.notifier.changed.emit()
```

## Add `load`
There are two console commands that don't exist as actions on BookView, and thus can't be bound to keyboard shortcuts:
- load n (numerical argument)
- setChapter n (numerical argument)

```python
    def load(self, book_id=0):
        # type: (CommandService, int) -> None
        try:
            self.loadBook.emit(int(book_id))
        except ValueError:
            logger.error('{} is not a valid book_id'.format(book_id))

    def setChapter(self, pos=None, move=None):
        # type: (CommandService, str | None, str | None) -> None
        if pos is None:
            return
        if self.onBookView():
            m = True if move in ['move', 'm'] else False
            try:
                p = int(pos)
                self.book_view.setChapter(p, m)
            except (TypeError, ValueError):
                if pos in ['next', 'n']:
                    self.book_view.nextChapter(m)
                elif pos in ['previous', 'prev', 'p']:
                    self.book_view.previousChapter(m)
```
I didn't remember this, but looks like setChapter can also take n/next or p/prev/previous.

Load isn't BookView related. Maybe we should put it on MenuController. But I wonder if it should be rather on library or ShelfView. I think ShelfView makes most sense. This will be the first time we're adding our actions shenanigans to another widget other than BookView and MenuController. What are the steps?

(1) Import stuff from keymap.py
```python
from retype.services.keymap import keymap, K, Keymap, genActions, keymapUpdate
```
(2) Add a keymap decorator above the class
```python
@keymap('ShelfView.load', K())
```
(3) Create an actions dictionary in the initialiser, call genActions, and connect to the notifier (I put `*` instead of `+` in the regex, because we'll make it possible for argstr to be empty in which case it will load the default book, like in the console command) [but actually the case of empty argument is already handled by argstr '', i.e. the main command / no argstr, so this is stupid, it should have been `\d+`]
```python
        self.actions = {
            'ShelfView.load': {
                'widget': self, 'func': self.loadBook,
                'args_regex': r'\d*',
                'args_func': lambda s: self.loadBook(int(s)),
            }
        }  # type: ActionsInfo
        genActions(self.actions)
        Keymap.notifier.changed.connect(
            lambda: keymapUpdate(self.actions, self))
```
(4) Write the loadBook method, copying from the command service method
```python
    def loadBook(self, book_id=0):
        # type: (ShelfView, int) -> None
        try:
            self._controller.loadBookRequested.emit(int(book_id))
        except ValueError:
            logger.error(f'{book_id} is not a valid book_id')
```
It's not ideal for command service to have duplicate logic, but it doesn't communicate with ShelfView otherwise. And it's a really simple function, so I will leave it for now.

{% include note.html content='
> [!NOTE]
> This is a case of copying without understanding. The catching ValueError is for the conversion of str to int, which can fail if it\'s not a number. Here we\'re already taking an int. `self._controller.loadBookRequested.emit(book_id)` would have sufficed.
' %}

But wait, is it a problem to put it on ShelfView because we might want to invoke that in BookView, in order to switch between books quickly with a shortcut?

I'm going to have to put it on menu after all. Library is not really a widget. But I guess in order to avoid it being confusing, since there is no load in the menu, we could call it Main.load despite the fact it will be on MenuController.

But there is also another issue. argstrs don't work with our ShelfView action. Our debug logs reveal why:
```python
00:33:50.820 [retype.services.keymap] DEBUG: keymapUpdate for <class 'PyQt5.QtWidgets.QMenuBar'>
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate.custom_actions = []
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate for <class 'retype.ui.shelf_view.ShelfView'>
\d*
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate: Created custom action ShelfView.load:2
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate.custom_actions = [{'action': <PyQt5.QtWidgets.QAction object at 0x7f96cadc99a0>, 'widget': <retype.ui.shelf_view.ShelfView object at 0x7f96cb46bd10>}]
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate for <class 'retype.ui.book_view.BookView'>
(m|move)
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate: Created custom action BookView.previousChapter:m
(m|move)
00:33:50.821 [retype.services.keymap] DEBUG: keymapUpdate: Created custom action BookView.nextChapter:m
00:33:50.822 [retype.services.keymap] DEBUG: keymapUpdate.custom_actions = [{'action': <PyQt5.QtWidgets.QAction object at 0x7f96cadc9a30>, 'widget': <retype.ui.book_view.BookView object at 0x7f96cad64200>}, {'action': <PyQt5.QtWidgets.QAction object at 0x7f96cadc9ac0>, 'widget': <retype.ui.book_view.BookView object at 0x7f96cad64200>}]
```
For each widget that connects to the notifier, there is a separate keymapUpdate call, and each one will delete the custom actions of the previous, so that only custom actions on the last widget will remain (or an empty list if it doesn't have any).

I guess `custom_actions` should instead be a global variable? Or should we put it on Keymap?

I'll put it on the keymap.
```python
class Keymap:
    selectors = {}  # type: dict[str, K]
    notifier = K()
    keymaps = {}  # type: dict[str, KeymapData]
    custom_actions = []  # type: list[CustomAction]
```
```python
    CustomAction = TypedDict(
        'CustomAction',
        {'action': QAction, 'widget': QWidget})
```

But actually just using this instead of `keymapUpdate.custom_actions` wouldn't resolve the issue. If we delete all the actions every keymapUpdate call, we're going to have the same bug. When to delete them?

Since the widget in CustomAction is the same as the widget argument passed in to keymapUpdate, we could maybe delete just the custom actions for that widget. It's kind of hard to filter them sadly, we'd have to iterate the entire list and check the widget for each one. We can't make the widget the key because it's non-iterable. Well, it's not so bad, we were already iterating them anyway, it's just one extra line to check if widget corresponds. Except we also have to delete from the list of `custom_actions`, which we can't do that easily while iterating over it.
```python
def keymapUpdate(actions, widget):
    # type: (ActionsInfo, QWidget) -> None
    """Utility function to update QActions shortcuts"""
    logger.debug(f'keymapUpdate for {type(widget)}')
    # 1. Remove previously created custom actions for widget
    i = len(Keymap.custom_actions) - 1
    while i >= 0:
        a = Keymap.custom_actions[i]
        if a['widget'] is widget:
            a['widget'].removeAction(a['action'])
            del Keymap.custom_actions[i]
        i -= 1
    # 2. Update bindings for actions in actions dictionary
    for name, d in actions.items():
        if not d.get('condition', True):
            continue
        n = name.split(':')
        selector_name = n[0]
        argstr = n[1] if len(n) > 1 else ''
        try:
            s = Keymap.s(selector_name).s(argstr)
            d['shortcuts'] = s
            action = d['action']
            action.setShortcuts(s)
        except (KeyError, AttributeError):
            logger.error(f"Updating k '{name}' failed. "
                         f"{traceback.format_exc()}")
        # Create custom actions based on bindings that lack actions
        for argstr, s in Keymap.s(selector_name).entries():
            if not s or s == ['']:
                continue
            combined_name = selector_name
            combined_name += f':{argstr}' if argstr else ''
            if combined_name in actions:
                continue
            regex = d.get('args_regex')
            func = d.get('args_func')
            if not (regex and func) or not re.match(regex, argstr):
                continue
            action = makeAction(
                combined_name, lambda _, f=func, a=argstr: f(a))
            action.setShortcuts(s)
            Keymap.custom_actions.append({'action': action, 'widget': widget})
            widget.addAction(action)
            logger.debug(
                f'keymapUpdate: Created custom action {combined_name}')
    logger.debug(
        'Keymap.custom_actions (len '
        f'{len(Keymap.custom_actions)}) = {Keymap.custom_actions}')
```

We actually didn't help anything by moving `custom_actions` onto Keymap, we could have done the same thing with it still stored on the function. But I guess it's more proper/clean that way (though I don't know what difference this would make to be honest), so let's go with that.

Ok, back to working on load then. Which we've established should not be on ShelfView, because we want it to work in BookView too. I'm going to put it on MenuController, but name it Main.load. Actually, should we name it loadBook instead? The console command is load, so I want to call it load to correspond with it, even though it's not super clear.

But it feels wrong to put it on menu. Like is MenuController going to have a loadBook function? It's not responsible for that.

I guess I could put it on MainController? Though it's a super bloated module already, so I hate adding anything there.

It's already the case that MenuController calls stuff on MainController for its actions. `self.controller.quit`, `self.controller.setViewByEnum(int(d))`, `self.controller.toggleConsoleWindow`. We could just call `self.controller.loadBookRequested.emit`, or even directly `self.controller.loadBook` (the signal just calls that function). It's coupling, but we already have coupling, so we're not worsening it.

(1) Add decorator
```python
@keymap('Main.load', K())
```
(2) Add ActionInfo entry to self.actions
```python
            'Main.load': {
                'widget': self._menu,
                'func': self.controller.loadBook,
                'args_regex': r'\d+',
                'args_func': lambda s: self.controller.loadBook(int(s)),
            },
```

And that's it, but I'm also going to modify MainController.loadBook, which is currently:
```python
    def loadBook(self, book_id=0):
        # type: (MainController, int) -> None
        book_view = self.views[View.book_view]
        try:
            self.library.setBook(book_id, book_view, self.switchViewRequested)
        except ValueError:
            logger.error(f'loadBook: {book_id} is not a valid book_id')
```

Then CommandService doesn't need to do this check--

Actually no. I am an idiot. I just realised that that catch is just for the conversion of a string into int, that's why it was on command service. It wasn't needed in the ShelfView function and it's not needed here. If the numerical id isn't valid, library logs and handles that.

Oddly, ShelfView.load is still there in the keymap customisation, even though we no longer have the decorator. It's apparently because I still have it in `__current_keymap__.json`. Is this a problem? ... I don't really want to touch this right now. Although now would be the best time to fix it, if I'm ever going to fix it. Months or years from now, I'm going to want to touch it even less.

## Fix customisation showing selectors that don't exist anymore
In the method `_loadValues` in KeymapWidget, we get the values with `getKeymapValues(KEYMAP_MODIFICATIONS_FILENAME, file_path)`.
```python
    def _loadValues(self, path):
        # type: (KeymapWidget, str) -> dict[str, dict[str, list[str]]]
        values = {}  # dict[str, dict[str, list[str]]]
        file_path = os.path.join(path, KEYMAP_MODIFICATIONS_FILENAME)
        if os.path.exists(file_path):
            values = getKeymapValues(KEYMAP_MODIFICATIONS_FILENAME, file_path)
            # Update shortcuts in application
            Keymap.set_(values)
        else:
            logger.debug(f'Current keymap {file_path} not found. This is\
 normal on first launch or if it has not been saved yet. Falling back to\
 default values.')
        return values or self.default_values
```
To avoid creating selectors for commands that don't really exist in the application, we could filter to only items with keys that are present in `self.default_values`. Should we do it here or in `getKeymapValues` itself? I guess here, but I'll add a note to `getKeymapValues` about this pitfall.

But `populateKeymaps` uses `getKeymapValues` too, is this a potential problem there too? I think not, because our only problem is with selector widgets being created for commands that don't exist, and that only happens once, on initialisation of the CustomisationDialog, using `getKeymapValues`. The keymaps having commands that don't exist after that point won't be able to create any selectors, nor would any actions be created for them (because only commands in the widget's actions dictionary, and any argstrs the user adds for those commands, get QActions. Although a widget could connect to the notifier something other than keymapUpdate and do whatever it wants).

```python
    def _loadValues(self, path):
        # type: (KeymapWidget, str) -> dict[str, dict[str, list[str]]]
        values = {}  # dict[str, dict[str, list[str]]]
        file_path = os.path.join(path, KEYMAP_MODIFICATIONS_FILENAME)
        if os.path.exists(file_path):
            values = getKeymapValues(KEYMAP_MODIFICATIONS_FILENAME, file_path)
            # Update shortcuts in application
            Keymap.set_(values)
        else:
            logger.debug(f'Current keymap {file_path} not found. This is\
 normal on first launch or if it has not been saved yet. Falling back to\
 default values.')
        # Filter out selectors that don't exist
        values = values or self.default_values
        return {s: values[s] for s in self.default_values.keys()}
```
`values[s]` could throw an exception if one of the keys doesn't exist, but in our case `getKeymapValues` uses fallbacks to ensure it always has all selectors, so we should never face that situation.
```python
    values = {n: {'': []} for n in Keymap.selectors.keys()}  # fallback
```

## Add `setChapter`
Back to adding the other console command function we're lacking. That one belongs on BookView.

Command service does all this:
```python
    def setChapter(self, pos=None, move=None):
        # type: (CommandService, str | None, str | None) -> None
        if pos is None:
            return
        if self.onBookView():
            m = True if move in ['move', 'm'] else False
            try:
                p = int(pos)
                self.book_view.setChapter(p, m)
            except (TypeError, ValueError):
                if pos in ['next', 'n']:
                    self.book_view.nextChapter(m)
                elif pos in ['previous', 'prev', 'p']:
                    self.book_view.previousChapter(m)
```
I think I'm going to move most of this logic to a function on BookView:
```python
    def setChapterAction(self, pos=None, move=None):
        # type: (BookView, str | None, str | None) -> None
        if pos is None:
            return
        m = True if move in ['move', 'm'] else False
        try:
            p = int(pos)
            self.setChapter(p, m)
        except (TypeError, ValueError):
            if pos in ['next', 'n']:
                self.nextChapter(m)
            elif pos in ['previous', 'prev', 'p']:
                self.previousChapter(m)
```
Then command service only needs to do this:
```python
    def setChapter(self, pos=None, move=None):
        # type: (CommandService, str | None, str | None) -> None
        if self.onBookView():
            self.book_view.setChapterAction(pos, move)
```

Now add a decorator over BookView:
```python
@keymap('BookView.setChapter', K())
```
and add it to BookView's actions dictionary:
```python
            'BookView.setChapter':
            {
                'func': self.setChapterAction,
                'args_regex': r'(\d+|next|n|previous|prev|p)( (m|move))?$',
                'args_func': lambda s: self.setChapterAction(*s.split()),
                'widget': self,
            },
```

It will crash if there are more than two words in s. We could make setChapterAction take more arbitrary arguments to avoid this (`*_`), but our `args_regex` already makes sure there can only be one space.

And I just noticed there is a problem with the rest of our regexes, because re.match returns a match object even if there are more stuff past the end if you don't have $ in the regex. Instead of re.match we should use re.fullmatch.

Oddly, with no arguments it goes to the first chapter. I expected it to do nothing. It seems like False is passed as the first argument. I guess it's Qt passing a boolean, like the problem we had with the lambdas earlier.

```python
    def setChapterAction(self, pos=None, move=None):
        # type: (BookView, str | None | bool, str | None) -> None
        # If no argstr, Qt passes a boolean in the first argument
        if pos is None or type(pos) is bool:
            return
        m = True if move in ['move', 'm'] else False
        try:
            p = int(pos)
            self.setChapter(p, m)
        except (TypeError, ValueError):
            if pos in ['next', 'n']:
                self.nextChapter(m)
            elif pos in ['previous', 'prev', 'p']:
                self.previousChapter(m)
```

I also had to modify the test for the console command because it expects setChapter to be called, not setChapterAction.

Problem: With the console command we can do `>chapter -1` to go to the last chapter, whereas with the equivalent keyboard shortcut we're not able to because it doesn't pass the args regex. The `\d+` in the regex should be changed to `-?\d+`.

And it crashes when we give it an index out of range... Did I really not think to check for that when I wrote this command a decade ago? I will tackle it in a separate section because it's not to do with the keyboard shortcuts, but the command itself.

## Regex match bugfix
In `keymapUpdate`, replace `re.match` by `re.fullmatch`.

## setChapter out of range bugfix
It actually does have a check that pos is in range:
```python
    def setChapter(self, pos, move_cursor=False, reset=True):
        # type: (BookView, int, bool, bool) -> None
        if self.book is None or self.chapter_pos is None:
            logger.error(f'setChapter: Unexpected None. book: {self.book}, '
                         f'chapter_pos: {self.chapter_pos}')
            return

        if len(self.book.chapters) > pos:
            self.setSource(self.book.chapters[pos])
        else:
            logger.error(f'setChapter: pos {pos} exceeds '
                         f'{len(self.book.chapters)} chapters')
            return

        self.viewed_chapter_pos = pos
        if move_cursor:
            self.chapter_pos = pos
            self._initChapter(reset)
            if isspaceorempty(self.tobetyped):
                logger.debug("Skipping empty chapter")
                self.setChapter(pos + 1, move_cursor)
            self.updateProgress()
        elif pos == self.chapter_pos:
            self.setCursor()
        elif pos < self.chapter_pos:
            self.highlight(full=True)
        self.updateModeline()
        self.display.updateFont()
        self.updateToolbarActions()
```
but we need to better handle negative numbers.

I first wrote a test to reproduce the error. Said test is too much of a dumpster fire to paste here.

I noticed that even doing setChapter -1 changes the chapter pos to -1; even though it works to go to the last chapter, we should probably change the pos to len-1 in that case.

This line before the `len(self.book.chapters) > pos` check makes it pass my tests:
```python
        # Normalise negative indices
        pos = pos if pos > 0 else abs(len(self.book.chapters) + pos)
```
But is this right? Let's say we have 5 chapters. -1 works out to be abs(5+-1) = abs(4) = 4, which is right. -5 works out to be abs(5+-5) = 0. -6 works out to be abs(5+-6) = abs(-1) = 1. Is this how Python negative indices are supposed to work? Well, no, in the interpreter -6 of a list with only 5 elements gives an index out of range error. So we need to make it so the `len(self.book.chapters) > pos` check isn't going to pass in that case.

{% include note.html content='
> [!NOTE]
> It\'s bugged for 0. It should have been if `pos >= 0`.
' %}

We could do away with the abs and check after the normalisation if it's still negative, in that case it's out of range.

But I kind of want to make it work with my existing error log:
```python
        if len(self.book.chapters) > pos:
            self.setSource(self.book.chapters[pos])
        else:
            logger.error(f'setChapter: pos {pos} exceeds '
                         f'{len(self.book.chapters)} chapters')
            return
```
In the case of -6, "pos -6 exceeds 5 chapters"? I guess I will change it to "pos -6 is not in range (num chapters: 5)", and the test to `0 < pos < len(self.book.chapters)`.

{% include note.html content='
> [!NOTE]
> `0 <= pos < len(self.book.chapters)`
' %}

That seems to have fixed it for retype, but for an unknown reason, and without any error or message, pytest segfaults half of the time. No information even with [PYTHONUNBUFFERED=1](https://github.com/pytest-dev/pytest/issues/1886).

With PYTHONFAULTHANDLER=1 there is sometimes a C stacktrace.
```python
Fatal Python error: Segmentation fault

Current thread 0x00007f1c2251ab80 [pytest] (most recent call first):
  <no Python frame>

Current thread's C stack trace (most recent call first):
  Binary file "/usr/lib/libpython3.14.so.1.0", at _Py_DumpStack+0x4d [0x7f1c21f3731c]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x13c250 [0x7f1c21f3c250]
  Binary file "/usr/lib/libc.so.6", at +0x3e2d0 [0x7f1c21c4d2d0]
  Binary file "/usr/lib/libQt5Core.so.5", at _ZNK11QMetaObject4castEPK7QObject+0x1c [0x7f1c1dacf77c]
  Binary file "/usr/lib/libQt5Widgets.so.5", at +0x34c323 [0x7f1c1e94c323]
  Binary file "/usr/lib/libQt5Widgets.so.5", at +0x34c605 [0x7f1c1e94c605]
  Binary file "/usr/lib/libQt5Widgets.so.5", at _ZN7QWidgetD2Ev+0x18e [0x7f1c1e784d4e]
  Binary file "/usr/lib/python3.14/site-packages/PyQt5/QtWidgets.abi3.so", at +0xd3f8f [0x7f1c1eed3f8f]
  Binary file "/usr/lib/libQt5Core.so.5", at _ZN14QObjectPrivate14deleteChildrenEv+0x5b [0x7f1c1daf78eb]
  Binary file "/usr/lib/libQt5Widgets.so.5", at _ZN7QWidgetD2Ev+0x375 [0x7f1c1e784f35]
  Binary file "/usr/lib/python3.14/site-packages/PyQt5/QtWidgets.abi3.so", at +0x30696f [0x7f1c1f10696f]
  Binary file "/usr/lib/python3.14/site-packages/PyQt5/QtCore.abi3.so", at +0x1a9c4f [0x7f1c1a9a9c4f]
  Binary file "/usr/lib/python3.14/site-packages/PyQt5/sip.cpython-314-x86_64-linux-gnu.so", at +0x15b58 [0x7f1c1f3f9b58]
  Binary file "/usr/lib/python3.14/site-packages/PyQt5/QtCore.abi3.so", at +0x1a9b00 [0x7f1c1a9a9b00]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x2af1ac [0x7f1c220af1ac]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x28fdd4 [0x7f1c2208fdd4]
  Binary file "/usr/lib/libpython3.14.so.1.0", at Py_Exit+0x3f [0x7f1c220bb10f]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x2b88c9 [0x7f1c220b88c9]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x2b8603 [0x7f1c220b8603]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x95528 [0x7f1c21e95528]
  Binary file "/usr/lib/libpython3.14.so.1.0", at +0x2ab817 [0x7f1c220ab817]
  Binary file "/usr/lib/libpython3.14.so.1.0", at Py_RunMain+0x2e8 [0x7f1c2205e508]
  Binary file "/usr/lib/libpython3.14.so.1.0", at Py_BytesMain+0x3b [0x7f1c22057c6b]
  Binary file "/usr/lib/libc.so.6", at +0x276c1 [0x7f1c21c366c1]
  Binary file "/usr/lib/libc.so.6", at __libc_start_main+0x89 [0x7f1c21c367f9]
  Binary file "/usr/bin/python", at _start+0x25 [0x55774a625045]

Extension modules: charset_normalizer.md, charset_normalizer.cd, _time_machine, PyQt5.QtCore, PyQt5.QtGui, PyQt5.QtWidgets, lxml._elementpath, lxml.etree, lxml.builder, PyQt5.QtXml, PyQt5.QtX11Extras, PyQt5.QtTest, PyQt5.QtSvg, PyQt5.QtSql, PyQt5.QtNetwork, PyQt5.QtQml, PyQt5.QtQuick, PyQt5.QtQuickWidgets, PyQt5.QtPrintSupport, PyQt5.QtOpenGL, PyQt5.QtDBus (total: 21)
Segmentation fault         (core dumped) PYTHONFAULTHANDLER=1 pytest
```

For my tests I have an object that subclasses the class I'm actually testing. It seems to be its initialiser that provokes it. Could it be because of BookView initialising BookDisplay?

I ended up having to patch a few more things.
```python
class PartiallyFakeBookView(BookView):
    @patch('retype.ui.book_view.BookDisplay')
    @patch('retype.ui.book_view.QTextCursor')
    @patch('retype.ui.book_view.keymapUpdate')
    @patch('retype.ui.book_view.Keymap')
    def __init__(self, win, ctrlr, *_):
        self.modeline = MagicMock()
        super().__init__(win, ctrlr)
        self._reset()
        self.book = FakeBook()
        self.tobetypedlist = ['abcdef']*5
        self.chapter_pos = 0

    def _initUI(self):
        pass

    def setCursor(self):
        pass

    def updateToolbarActions(self):
        pass

    def _reset(self):
        self._called = None
```

## Bonus: Add a new command, fillChars
A command that fills the next n characters where if the console text matches the current line up to a certain index (i.e. all of the console text should be correct, but not complete).

Going to write tests first, because the cases are very clear to me:
- If the text in the console doesn't match the beginning of the line, it should do nothing.
  + If there's nothing in the console, then it counts as matching. There just needs to not be any "wrong text".
- If it does match and n=1, fill in the next character.
- If it does match and n is greater than the amount of characters left, it should advance to the next line.
- If n<1, do nothing.

I don't think we want it to error if n is too large. Just fill the whole line.

I thought it has to be on BookView, but it has more to do with HighlightingService, and it's the latter that implements the function advanceLine for example.

```python
    def fillChars(self, n=1):
        # type: (HighlightingService, int) -> None
        v = self.book_view

        if (not self.valid(v) or n < 1 or self.wrong
                or not hasattr(v, 'current_line')):
            logger.debug('fillChars: Ignoring')
            return

        end = min(len(self._console.text())+n, len(v.current_line))
        self._console.setText(v.current_line[:end])
```

I'm bad at writing tests so should not be used as an example, but here they are if you're curious:
```python
        (console, v, service, cursor) = _setup("some test text<br/>hi")

        console.setText("")
        service.fillChars(1)
        assert cursor.position() == 1

        # wrong text, position shouldn't change
        console.setText("x")
        service.fillChars(1)
        assert cursor.position() == 0

        # correct text with n too large
        console.setText("some")
        service.fillChars(2390423)
        assert cursor.position() == 15

        # negative n, position shouldn't change
        # (still 15 bc we're at the start of the second line)
        console.setText("")
        service.fillChars(-2390423)
        assert cursor.position() == 15
```

Now where on earth do we put the action? BookView doesn't have access to HighlightingService. But it does have access to the console so it could access it from there.

```python
@keymap('BookView.fillChars', K())
```
```python
            'BookView.fillChars':
            {
                'func': self.fillCharsAction,
                'args_regex': r'\d+',
                'args_func': lambda s: self.fillCharsAction(s),
                'widget': self,
            },
```
```python
    def fillCharsAction(self, n="1"):
        # type: (BookView, str) -> None
        h = self._console.highlighting_service
        if h:
            try:
                n = int(n)
            except ValueError:
                return
            h.fillChars(n)
```

Also need a console command
```python
    'fillChars':
    {
        'desc': 'If console text matches, add next N characters.',
        'aliases': ['fill'],
        'args': 'N',
    },
```
```python
    def fillChars(self, n="1"):
        # type: (CommandService, str) -> None
        if self.onBookView():
            self.book_view.fillCharsAction(n)
```

But I just realised it could never work as a console command, because by entering a console command we're necessarily entering wrong text.

The keyboard shortcut doesn't work either, it always logs "Ignoring".

It's the Qt boolean thing again. I guess I will just change func to a lambda to avoid it. `lambda s: self.fillCharsAction(1)`

Another issue is we have to explicitly convert the string we're passing to setText to str: `self._console.setText(str(v.current_line[:end]))`. This is because retype works with str subclasses like ManifoldStr to allow replacements (several allowed strings for the same text in string comparisons), and the Qt edit widget needs a real str.

Another problem is the caret position after the setText is at the beginning of the console text. It should be at the end. This is a problem the console command history has also.

I think moving the cursor should be done in the BookView action function rather than in the highlighting service function, because otherwise highlighting service import Qt modules.

I tried `self._console.textCursor().setPosition(QTextCursor.MoveOperation.End)` but it doesn't move and I just get:
> QTextCursor::setPosition: Position '11' out of range

(always 11)

This does it:
```python
self._console.moveCursor(QTextCursor.MoveOperation.EndOfLine)
```

## Cursor position when traversing command history
To fix the same problem in the console's command history, I thought about adding this method and calling it every time we do setText instead of the console's setText directly:
```python
    def setText(self, text):
        # type: (CommandService, str) -> None
        self._console.setText(text)
        self._console.moveCursor(QTextCursor.MoveOperation.EndOfLine)
```
but CommandService doesn't import any Qt stuff, so I'm hesitant to put it there. Maybe we should put that method on the console itself. Should it be called setText or something more explicit? (setTextAndEol?) I don't know if there is ever a situation when you do setText that you want the cursor to move to the beginning (that's what happens by default). Ending up at the end makes more sense to me as the default. So I guess I will call it just setText. Careful to avoid infinite recursion.
```python
    def setText(self, text):
        # type: (Console, str) -> None
        super().setText(text)
        self.moveCursor(QTextCursor.MoveOperation.EndOfLine)
```

## "Bonus" 2: Fixing a bug I discovered in Customisation Dialog
While working on this, I noticed a bug in the customisation dialog. In line splits settings, opening an editor, opening another editor, then opening the first one again, crashes with:
```python
Traceback (most recent call last):
  File "../retype/ui/customisation_dialog.py", line 1001, in createEditor
    return SDictEntryEditor(data[0], data[1], parent)
  File "../retype/ui/customisation_dialog.py", line 957, in __init__
    self.keep_e = CheckBox('', keep)
                  ~~~~~~~~^^^^^^^^^^
  File "../retype/ui/customisation_dialog.py", line 542, in __init__
    self.setChecked(value)
    ~~~~~~~~~~~~~~~^^^^^^^
TypeError: setChecked(self, a0: bool): argument 1 has unexpected type 'dict'
```

I see in `config_edited` that this is what ends up happening:
```python
'\n': {'keep': {'keep': False}}
```

You can even see this change in the UI.
```asciiart
Substring: '_' (\r\n)
Keep: {'keep': False}
```

In SDictEntryEditor:
```python
    def keep(self):
        # type: (SDictEntryEditor) -> SDictEntry
        return {'keep': self.keep_e.isChecked()}
```
Why is it doing that instead of returning just `self.keep_e.isChecked()`? There must have been a reason, as it would have worked to start with and been broken by something I did later. Well, the only place where I see it being called is SDictDelegate.setModelData, which doesn't need it in this format, so I have no clue.

Something I'm suspicious of this this in SDictDelegate.createEditor, because it's a fairly recent change:
```python
        # NOTE(plu5, 2025-10-14): alas, mypy broke * in the past year
        return SDictEntryEditor(data[0], data[1], parent)
```
Commit [0a57370](https://github.com/plu5/retype/commit/0a5737047782e9490a968a615c78b82ecfe613a8). But it was just `*data` before, so I don't see what it has to do with it.

```python
    def keep(self):
        # type: (SDictEntryEditor) -> SDictEntry
        return self.keep_e.isChecked()
```
That fixed it and nothing appears to have broken.

Let me check old builds I have on my system to see how long this bug has existed.

- 1.6.4 : existed
- 1.6.3 : existed
- 1.6.2 : existed
- 1.3.6 : doesn't even work at all, crashes immediately when I try to open an editor for a line splits entry.

Well, uh... let's... let's stop there.

T_T

## Zoom stopped working
I was confused then I saw that the function gets called and realised it's again the problem of Qt passing in an argument to the function that has come back to bite me. If we put on an action a function that takes an argument, Qt passing through a boolean is going to break it. I don't want to have to remember to put it in a lambda, I will likely forget and it will come back to bite me yet again. We need a way to fix it for good.

In my `makeAction` utility function, instead of:
```python
action.triggered.connect(func)
```
Change to:
```python
# Avoid Qt passing arguments
action.triggered.connect(lambda *args, **kwargs: func())
```

But it breaks if func is a lambda that takes arguments.

> TypeError: `keymapUpdate.<locals>.<lambda>()` missing 1 required positional argument: '_'

Maybe if I make it a keyword argument?
```python
action.triggered.connect(lambda checked=False: func())
```

No, that has the same problem, in the cases func is a lambda we already avoid the checked argument, and the argument we have there is swallowing the argument func needs if it's in the lambda as a positional argument. Obviously I could change all the functions to use keyword arguments only, but it's shifting the problem, I have to remember to do that or I will have subtle bugs, and I don't want to have to remember to do that, I want to solve it once and for all.

I'm going to do this in keymap:
```python
def logReminderIfNonLambda(func):
    # type: (Callable | None) -> None
    if func and func.__name__ != "<lambda>":
        logger.warning(f'{func.__name__} is not a lambda; remember that \
Qt sends a boolean as first argument, which could cause bugs if this \
function expects a different argument.')
```
then in genActions:
```python
            func = info.pop('func', None)
            logReminderIfNonLambda(func)
            func_ui = info.pop('func_ui', None)
            logReminderIfNonLambda(func_ui)
```

I was going to make it debug rather than warning, but I am worried I would miss it.

And now I have rather a lot of functions I have to change into a lambda. It's annoying but it's less annoying than having to chase more bugs caused by this nonsense in future.

{% include fin.html %}
