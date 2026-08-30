---
layout: post
title: 28 — retype zipfile bug
date: 2026-08-27 20:58
modified_date: 2026-08-28 04:29
categories: retype debugging python
lang: en
redirect_from: /devlog/28
---

## Traceback
While testing a build for the next release (1.7.2) I added a library load path that I haven't used before, and it provoked a crash.

```python
Traceback (most recent call last):
  File "/usr/lib/python3.14/site-packages/ebooklib/epub.py", line 1755, in _load
    self.zf = zipfile.ZipFile(self.file_name, "r", compression=zipfile.ZIP_DEFLATED, allowZip64=True)
              ~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
  File "/usr/lib/python3.14/zipfile/__init__.py", line 1480, in __init__
    self._RealGetContents()
    ~~~~~~~~~~~~~~~~~~~~~^^
  File "/usr/lib/python3.14/zipfile/__init__.py", line 1547, in _RealGetContents
    raise BadZipFile("File is not a zip file")
zipfile.BadZipFile: File is not a zip file

During handling of the above exception, another exception occurred:

Traceback (most recent call last):
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/controllers/main_controller.py", line 220, in saveConfig
    self._repopulateLibrary(config['user_dir'],
    ~~~~~~~~~~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^
                            config['library_paths'])
                            ^^^^^^^^^^^^^^^^^^^^^^^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/controllers/main_controller.py", line 178, in _repopulateLibrary
    self.library.instantiateBooks()
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/controllers/library.py", line 75, in instantiateBooks
    book = BookWrapper(item, self.load(item))
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/controllers/library.py", line 218, in __init__
    self._book = self._readEpub()
                 ~~~~~~~~~~~~~~^^
  File "/media/Windows/Users/pm/dev/retype/bin/../retype/controllers/library.py", line 235, in _readEpub
    ret = epub.read_epub(self.path, options={'ignore_ncx': True})
  File "/usr/lib/python3.14/site-packages/ebooklib/epub.py", line 1817, in read_epub
    book = reader.load()
  File "/usr/lib/python3.14/site-packages/ebooklib/epub.py", line 1463, in load
    self._load()
    ~~~~~~~~~~^^
  File "/usr/lib/python3.14/site-packages/ebooklib/epub.py", line 1757, in _load
    raise EpubException(0, "Bad Zip file")  # noqa: B904
    ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
ebooklib.epub.EpubException: 'Bad Zip file'
Aborted                    (core dumped) bin/retype -l DEBUG
```

## Initial thoughts
Before looking at anything, my theory is that it's caused by there being non-epub archives in the library path. But if I recall correctly, retype only tries to load files whose name ends with `.epub`, so I'm not sure why this would happen.

And if it's an epub file, even if it was corrupted or problematic, the result shouldn't be a crash, there is logic to handle that situation which was added in release 1.6.4 ([issue 41](https://github.com/plu5/retype/issues/41)).

First thing we could do is add a print before line 218 in library to see which book makes it happen.

## It's a rar archive with epub extension
It's `library_of_babel_book_1.epub` from [issue 30](https://github.com/plu5/retype/issues/30)! It looks to be a rar file that contains an epub file:
```sh
$ 7z l /home/pm/pm/library/dl/library_of_babel_book_1.epub

7-Zip 26.02 (x64) : Copyright (c) 1999-2026 Igor Pavlov : 2026-06-25
 64-bit locale=en_GB.UTF-8 Threads:8 OPEN_MAX:4096, ASM

Scanning the drive for archives:
1 file, 930606 bytes (909 KiB)   

Listing archive: /home/pm/pm/library/dl/library_of_babel_book_1.epub

--
Path = /home/pm/pm/library/dl/library_of_babel_book_1.epub
Open WARNING: Cannot open the file as [zip] archive
Type = Rar5
Physical Size = 930606
Characteristics = Locator QuickOpen:930498
Encrypted = -
Solid = -
Blocks = 1
Method = v6:1M:m3
Multivolume = -
Volumes = 1

   Date      Time    Attr         Size   Compressed  Name
------------------- ----- ------------ ------------  ------------------------
2025-01-12 03:17:55 ....A      1015433       930418  library_of_babel_book_1.epub
------------------- ----- ------------ ------------  ------------------------
2025-01-12 03:17:55            1015433       930418  1 files

Warnings: 1
```
So it's not really in itself an epub file and should not have the extension `.epub` (but rather `.rar`), but retype shouldn't crash because of this, it should handle it gracefully.

## Handle exception
Here is the way it's currently handled:
```python
    def _readEpub(self):
        # type: (BookWrapper) -> epub.EpubBook
        ret = None
        try:
            ret = epub.read_epub(self.path, options={'ignore_ncx': True})
            self.valid = True
        except (LookupError, OSError) as e:
            s = (f'Unable to read epub {self.idn}:\n{self.path}.\n\n'
                 'This is not fatal, but the book will not be loaded.')
            logger.error(f"{s}\n{e}", exc_info=True)
            msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
            msg.setDetailedText(f'Path: {self.path}\n\n'
                                f'{traceback.format_exc()}')
            msg.exec()
        return ret or epub.EpubBook()
```

In this case the exception is not `LookupError` nor `OSError`, but rather `ebooklib.epub.EpubException`. We could simply add this to the except, and the issue should be resolved.

Indeed. It now shows the user an error with the path to the problematic book and copiable traceback (should they wish to report it), and skips loading that book.

## retype 1.7.2
I released the fixes from this and the previous devlog in [1.7.2](https://github.com/plu5/retype/releases/tag/v1.7.2).

While testing the release, I noticed usability problems with the customisation dialog's library load paths widget. It's annoying rather than critical, so I think it will have to wait for next week.

{% include fin.html %}
