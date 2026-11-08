---
layout: post
title: 31 — retype line edit UX
date: 2026-09-22 13:12
modified_date: 2026-11-07 12:04
categories: retype
lang: en
redirect_from: /devlog/31
wip: true
---

## What
This is actually the issue I was referring to at the end of devlog 28. I noticed it while testing the builds for 1.7.2 and after I saw it I couldn't understand how I didn't notice it before.

There are UX problems with the line edits in the customisation dialog LibraryPathsWidget. The PathDelegate editors.

1. **Width issues:** Instead of truncating the text, the editor grows in width to fit it, so if there is a long path the browse button goes out of view (hard to notice, but it doesn't clip; there is a horizontal scrollbar).
2. **Invisibility:** This issue doesn't appear on the build, but when running from sources, when you activate the editor the text behind the editor is still visible, i.e. the background of the editor is invisible, you can see through it, thus leading to two texts overlapping and hard to see what you're editing. We can't/shouldn't hardcode a background colour, so maybe just hide the thing behind it when it's active?
3. **Manual confirm:** After selecting a path with the browse button, you have to press enter to confirm and exit out of the editor in order for the path to actually be added and to be able to save configuration. It would be better if it confirms automatically after browse.

## Where
PathDelegate creates an editor like this:
```python
    def createEditor(self,
                     parent,  # type: QWidget
                     option,  # type: QStyleOptionViewItem
                     index  # type: QModelIndex
                     ):
        # type: (...) -> QWidget
        data = index.data(Qt.ItemDataRole.EditRole)  # type: object
        if not isinstance(data, str):
            logger.error(f'createEditor: EditRole data at index {index} is not'
                         f' str. Received {data} ({type(data)}).')
            data = ''
        return PathSelector(data, parent)
```

LibraryPathsWidget instantiates it in its initialiser:
```python
    def __init__(self, library_paths, parent=None):
        # type: (LibraryPathsWidget, list[str], QWidget | None) -> None
        QWidget.__init__(self, parent)

        lyt = QFormLayout(self)
        lyt.setContentsMargins(0, 0, 0, 0)
        self.view = QListView()
        self.view.setItemDelegate(PathDelegate())
        self.setModel(library_paths)
        lyt.addRow(self.view)
        # ..
```

Is it PathSelector that needs to be modified, or something here? PathSelector is simply a QWidget with a QLineEdit and a QToolButton, so I wouldn't think it's responsible, but maybe is supposed to set sizehint or policy to allow it to shrink to fit the width of the view.

Ah, PathDelegate inherits Delegate, which has this sizehint:
```python
    def sizeHint(self, option, index):
        # type: (Delegate, QStyleOptionViewItem, QModelIndex) -> QSize
        size = self.toDoc(index).size().toSize()
        size.setHeight(size.height() + self.spacing*2)
        return size
```
I can't remember why, and all the other delegates inherit from it: sdict, rdict, kdict delegates. They seem to have the same issue; when the text is long, the inputs are only partially visible and annoying to use, with a scrollbar at the bottom of the listview that is not likely to be noticed by the user. So ideally we should fix this sizehint.

## Attempts to fix the sizehint
If I remove it altogether, the width problem is the same (and it makes the height inconsistent, too little or too much), so this is not what is causing it. We need to keep it and add also an adjustment for the width.

`size.setWidth(option.rect.width())` or `size.setWidth(option.widget.viewport().width())`[^option-widget] only work after I open and close the editor. The first time there is the width problem even if the text is short or empty. Even when it does help, in the view without the editor, like when the editor is not opened, text too long clips and without scrollbar so it's not ideal (presently at least if you are aware of the scrollbar you can use it to see the entire text).

I don't need word wrap, but to my surprise `self.view.setWordWrap(True)` sorts it and does not add word wrap.

Is it just that it triggers a recalculation? What is the result without the sizehint and with `setWordWrap(True)`? Wrong heights and the widths are correct.

It seems like it affects the height calculation which was wrong already, so with word wrap and with the sizehint that sets the height, that fixes the problem without side effects if all that word wrap affects is the height calculation.

It's actually not just the word wrap but additionally in the sizehint starting from the default height and then setting its height, rather than starting from `self.toDoc(index).size().toSize()`.

New sizehint:
```python
    def sizeHint(self, option, index):
        # type: (Delegate, QStyleOptionViewItem, QModelIndex) -> QSize
        size = super().sizeHint(option, index)
        size.setHeight(
            self.toDoc(index).size().toSize().height() + self.spacing*2)
        return size
```

There is a QListView subclass called ListView used by sdict, rdict, and kdict. Moving it up and modifying LibraryPathsWidget to use it as well, then this issue is solved for all of them in one place.

Except that it doesn't solve it for the others. Ah, I guess I have to change the sizehint on their delegates as well if it also does the doc size thing. No, they all (SDictDelegate, RDictDelegate, KDictDelegate) subclass Delegate, which is the class that has that sizehint, and they don't redefine it. Then is it because they use different editors?

- PathDelegate : PathSelector
- SDictDelegate : SDictEntryEditor
- RDictDelegate : RDictEntryEditor
- KDictDelegate : KDictEntryEditor

None of them define a sizehint.

The other difference is the entry editors are over multiple lines (their layout has 2 rows) whereas PathSelector is only one line (just a line edit with a button next to it).

Tried `self.setHorizontalScrollBarPolicy(Qt.ScrollBarAlwaysOff)` in ListView, but although it hides the scrollbar, it only masks the issue, the width of the line edit is still too large when the data is too long (just the rest of it is out of view).

I suppose in reality only for paths is this commonly a problem, as we don't usually have long entries in sdict, rdict, or kdict, so the wordwrap workaround is be better than nothing.

Commit for now and test build on different platforms.

Idea for diagnosing this further: try to make a MWE (a simple standalone pyqt script that reproduces the issue)

## Attempts to fix text showing under

notes to self / ideas:
- signal on path selector to emit at the end of browse that can be connected to to close the editor after browse to solve the manual confirm issue
- research qlistview delegate editor line edit transparent (can see beneath it)

{% include fin.html %}

[^option-widget]: and why isn't `widget` documented as a member of QStyleOptionViewItem on  [https://doc.qt.io/archives/qt-5.15/qstyleoptionviewitem-members.html]?
