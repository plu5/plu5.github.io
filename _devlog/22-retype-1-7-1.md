---
layout: post
title: 22 — More retype bugfixes for 1.7.1
date: 2026-06-09 12:01
modified_date: 2026-06-11 01:48
categories: retype
lang: en
redirect_from: /devlog/22
---

After the release of retype 1.7.0 which I worked towards in [devlog 19](/devlog/19), I noticed a few issues that I would like to fix in a patch release posthaste.

## Bug 1: gotoCursorPosition not always clearing console
Before the 1.7.0 release I noticed going to next and previous chapter while moving the cursor does not clear the console. Similar behaviour with skip line led the bug dealt with in [devlog 18](/devlog/18). This can lead to position issues. All commands that move the cursor should clear the console first. I had added that in time for the release, and had the presence of mind to add it for `gotoCursorPosition` too, but only if `move` is True:
```python
    def gotoCursorPosition(self, move=False):
        # type: (BookView, bool) -> None
        if self.chapter_pos is None:
            return
        if move:
            self._controller.console.clear()
            self.setChapter(self.viewed_chapter_pos, True)
            self.updateProgress()
        else:
            self.setChapter(self.chapter_pos)
            self.setCursor()
            self.display.centreAroundCursor()
```
We should actually clear it always for this command, unlike setChapter where we only clear if `move` (so I can see why I made this mistake).

## Bug 2: Crash if autosave fails
I noticed this while testing on macOS where I ran retype directly from the mounted volume without bothering to copy over to the filesystem, and that volume is read-only, so the autosave raises an exception and retype crashes. What it should do is catch PermissionError. Or maybe the parent exception, OSError?
```asciiart
 OSError
  ├── BlockingIOError
  ├── ChildProcessError
  ├── ConnectionError
  │    ├── BrokenPipeError
  │    ├── ConnectionAbortedError
  │    ├── ConnectionRefusedError
  │    └── ConnectionResetError
  ├── FileExistsError
  ├── FileNotFoundError
  ├── InterruptedError
  ├── IsADirectoryError
  ├── NotADirectoryError
  ├── PermissionError
  ├── ProcessLookupError
  └── TimeoutError
```
([Python docs](https://docs.python.org/3/library/exceptions.html))

The idea with catching exceptions is you don't want to catch ones where it means something has gone catastrophically wrong, because retype is not a mission-critical piece of software and *should* probably crash if the system is unable to allocate memory anymore for example. I'm not sure if in this case these exceptions could happen other than PermissionError, maybe Blocking IO, and is that catastrophic or is it ok to catch it? I think not, and that it would be better to give the user information about what happened than just crash in this case. Catch OSError and open an error message box with information about the error, similar to [the one added in October](https://github.com/plu5/retype/commit/1a862bcd2adc73ed739937ded82d8bb55e83bb80) when failing to load an epub. Looks like I am already catching OSError there and in several other places in the codebase (customisation dialog when saving theme or keymap for example).

It's this function in controllers/library.py that gets invoked when saving:
```python
    def save(self, book, data):
        # type: (LibraryController, BookWrapper, SaveData) -> None
        book.save_data = data

        if not os.path.exists(self._user_dir):
            logger.error(f'Unable to find user_dir {self._user_dir}')
            return

        self.addFriendlyName(data, book.path)
        key = book.checksum
        save = self.save_file_contents
        if (save):
            save[key] = data
        else:
            save = self.save_file_contents = {key: data}

        with open(self.save_abs_path, 'w', encoding='utf-8') as f:
            json.dump(save, f, indent=2)
```
It's also weird to just return if the user dir doesn't exist, that should be an error dialog too. I think we don't need to check `os.path.exists(self._user_dir)` explicitly, `FileNotFoundError` will be raised if the containing folder of `self.save_abs_path` doesn't exist.
```python
>>> with open('/home/pm/hi', 'w'): print('fine')
...
fine
>>> with open('/home/pm/hi/hi', 'w'): print('fine')
...
Traceback (most recent call last):
  File "<python-input-11>", line 1, in <module>
    with open('/home/pm/hi/hi', 'w'):
         ~~~~^^^^^^^^^^^^^^^^^^^^^^^
NotADirectoryError: [Errno 20] Not a directory: '/home/pm/hi/hi'
>>> with open('/home/pm/doesntexist/hi', 'w'): print('fine')
...
Traceback (most recent call last):
  File "<python-input-12>", line 1, in <module>
    with open('/home/pm/doesntexist/hi', 'w'):
         ~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
FileNotFoundError: [Errno 2] No such file or directory: '/home/pm/doesntexist/hi'
```

I also want instead of returning None to return a boolean from this function to indicate whether it failed, and if it did to disconnect autosave, to avoid the message popping up again after we already alerted the user.
```python
    def save(self, book, data):
        # type: (LibraryController, BookWrapper, SaveData) -> bool
        book.save_data = data

        self.addFriendlyName(data, book.path)
        key = book.checksum
        save = self.save_file_contents
        if (save):
            save[key] = data
        else:
            save = self.save_file_contents = {key: data}

        try:
            with open(self.save_abs_path, 'w', encoding='utf-8') as f:
                json.dump(save, f, indent=2)
        except OSError as e:
            s = 'Unable to save progress to disk.'
            if e is FileNotFoundError:
                s += f' Unable to find user_dir {self._user_dir}.'
            logger.error(f"{s}\n{e}", exc_info=True)
            msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
            msg.setDetailedText(f'Path: {self.save_abs_path}\n\n'
                                f'{traceback.format_exc()}')
            msg.exec()
            return False
        return True
```

I have a test that fails now because it asserts that json.dump doesn't get called if the user dir doesn't exist. I guess I will just delete it (open and json.dump are mocked so I can't check if it raises an exception, I don't want to do real I/O).

It's BookView that's responsible for connecting autosave, so I guess it should be disconnecting it when it fails.

maybeSave currently does:
```python
            self._library.save(self.book, data)  # type: ignore[arg-type]
            self.book.dirty = False
```
Change it to:
```python
            if self._library.save(self.book, data):  # type: ignore[arg-type]
                self.book.dirty = False
            else:               # Saving failed
                logger.info("Can't save. Deactivating autosave.")
                self.autosave.save.disconnect(self.maybeSave)
```

This is apparently not a good way of doing it, as if save gets called again by something else, like when we quit retype, it crashes with:
```python
TypeError: 'method' object is not connected
```

Will it be better if I do `self.autosave.save.disconnect()` without parameters?

No:
```python
TypeError: disconnect() failed between 'save' and all its connections
```

I see that Autosave has an `on` parameter. Instead of disconnecting it we could just set `on` to False, and it will stop emitting.
```python
            if self._library.save(self.book, data):  # type: ignore[arg-type]
                self.book.dirty = False
            else:               # Saving failed
                logger.info("Can't save. Deactivating autosave.")
                self.autosave.on = False
```

## OSError in other places
I'm going to search the codebase for `open(` to check we don't have this problem in other places

1. customisation dialog -- theme load values and saving theme and keymap, already handled
2. theme -- getThemeValues, already handled
3. keymap -- getKeymapValues, already handled
4. library
5. hashing
6. safe config
7. constants

Although 1-3 are already handled it is just logs then silently continuing without a message box. Perhaps in those cases though that is desireable, as keymap and theme are just auxiliary functions.

4 (library):
Other than the case we just fixed, there's also loadSaveFile:
```python
    def loadSaveFile(self):
        # type: (LibraryController) -> Save
        if os.path.exists(self.save_abs_path):
            logger.info(f'Read save: {self.save_abs_path}')
            with open(self.save_abs_path, 'r') as f:
                save = json.load(f)  # type: Save
        else:
            logger.debug(
                f'Save path {self.save_abs_path} not found.\n'
                'This is normal if the save file has not been created yet.')
            save = {}

        save = self.migrateV1Save(save)
        self.save_file_contents = save
        return save
```
Here I'm not going to put FileNotFoundError in a catch, since it's expected in the normal operation of the program. We should still wrap the open in a try catch for OSError though.
```python
            try:
                with open(self.save_abs_path, 'r') as f:
                    save = json.load(f)  # type: Save
            except OSError as e:
                s = 'Unable to read save file.'
                logger.error(f"{s}\n{e}", exc_info=True)
                msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
                msg.setDetailedText(f'Path: {self.save_abs_path}\n\n'
                                    f'{traceback.format_exc()}')
                msg.exec()
```

5 (hashing):
```python
def generate_file_md5(path, blocksize=2**20):
    # type: (str, int) -> str
    """https://stackoverflow.com/a/1131255"""
    m = hashlib.md5()
    with open(path, "rb") as f:
        while True:
            buf = f.read(blocksize)
            if not buf:
                break
            m.update(buf)
    return m.hexdigest()
```
This gets called on each book to get its checksum when indexing the library. The purpose of it is that the same book on different paths would have the same save (because it would have the same checksum). If we can't read the book file, we have bigger issues, probably it would have failed earlier on, upon ebooklib trying to read it? We actually generate the checksum before that. And we do have a check for OSError around that, that is what I mentioned earlier [that I added in October](https://github.com/plu5/retype/commit/1a862bcd2adc73ed739937ded82d8bb55e83bb80).

It's not desireable to have two errors. But I don't really want to remove the one in readEpub, and I can't see how to move the checksum generation to happen after that, we need to generate the checksum first in order to avoid reading the same book twice.

Code duplication it is...
```python
    def checksum(self, path):
        # type: (LibraryController, str) -> str | None
        checksum = None
        try:
            checksum = generate_file_md5(path)
        except OSError as e:
            s = (f'Unable to read epub {self.idn}:\n{self.path}.\n\n'
                 'This is not fatal, but the book will not be loaded.')
            logger.error(f"{s}\n{e}", exc_info=True)
            msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
            msg.setDetailedText(f'Path: {path}\n\n'
                                f'{traceback.format_exc()}')
            msg.exec()
        return checksum
```
Then in indexLibrary skipping if no checksum:
```python
                        path = os.path.join(root, f)
                        checksum = self.checksum(path)
                        if not checksum or checksum in book_checksum_list:
                            continue
```
This means that the readEpub error should never happen, because we're already trying to read the same file before, and if it's unreadable we would get to the point of calling readEpub. There could be a situation in future where we would want to call readEpub in another set of circumstances, so best leave it there.

6 (safe config):
```python
    def _load(self, path):
        # type: (_SafeConfig, str) -> Config | None
        if os.path.exists(path):
            logger.info(f'Read config: {path}')
            with open(path, 'r') as f:
                config = json.load(f)  # type: Config
                return config
        else:
            logger.debug(
                f'Config path {path} not found.\n'
                'This is normal if the config file has not been created yet.')
            return None
```
```python
    def save(self):
        # type: (_SafeConfig) -> None
        user_dir = self.raw['user_dir']
        if not os.path.exists(user_dir):
            logger.error(f'Unable to find user_dir {user_dir}')
            return

        path = os.path.join(user_dir, self.config_rel_path)
        with open(path, 'w') as f:
            json.dump(self.raw, f, indent=2)
        if not self.isPathDefaultUserDir(user_dir):
            path = os.path.join(self.default_user_dir, self.config_rel_path)
            if os.path.exists(path):
                with open(path, 'r') as f:
                    dconfig = json.load(f)  # type: Config
                    dconfig['user_dir'] = user_dir
                with open(path, 'w') as f:
                    json.dump(dconfig, f, indent=2)
            else:
                logger.error(f'Unable to find dconfig {path}')
```

For load, can just wrap open in the same thing we did for save.
```python
            try:
                with open(path, 'r') as f:
                    config = json.load(f)  # type: Config
                    return config
            except OSError as e:
                s = 'Unable to read config file.'
                logger.error(f"{s}\n{e}", exc_info=True)
                msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
                msg.setDetailedText(f'Path: {path}\n\n'
                                    f'{traceback.format_exc()}')
                msg.exec()
```
and where we call it, bail if it fails:
```python
        config = self._load(path)
        if config is None:      # Loading failed
            return default_config
```

Do we risk subtle bugs by returning default config without deepcopying it? I don't think anything modifies it. But we might as well just do `return deepcopy(default_config)` instead.

Save is more complicated. I broke it up into 3 functions:
```python
    def save(self):
        # type: (_SafeConfig) -> None
        user_dir = self.raw['user_dir']
        path = os.path.join(user_dir, self.config_rel_path)
        if not self._save(path, self.raw):  # Saving failed
            return

        if not self.isPathDefaultUserDir(user_dir):
            dconfig = self.loadDconfig()
            if dconfig is not None:
                dconfig['user_dir'] = user_dir
                self._save(path, dconfig)

    def _save(self, path, data):
        # type: (_SafeConfig, str, Config) -> bool
        try:
            with open(path, 'w') as f:
                logger.debug(f'Saving config: {path}')
                json.dump(data, f, indent=2)
        except OSError as e:
            s = 'Unable to save config file.'
            logger.error(f"{s}\n{e}", exc_info=True)
            msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
            msg.setDetailedText(f'Path: {path}\n\n'
                                f'{traceback.format_exc()}')
            msg.exec()
            return False
        return True

    def loadDconfig(self):
        # type: (_SafeConfig) -> Config | None
        dconfig = None
        path = os.path.join(self.default_user_dir, self.config_rel_path)
        try:
            with open(path, 'r') as f:
                dconfig = json.load(f)  # type: Config
        except OSError as e:
            s = 'Unable to load dconfig file.'
            logger.error(f"{s}\n{e}", exc_info=True)
            msg = QMessageBox(QMessageBox.Icon.Warning, 'retype', s)
            msg.setDetailedText(f'Path: {path}\n\n'
                                f'{traceback.format_exc()}')
            msg.exec()
        return dconfig
```

7 (constants):
```python
def getBuilddate():
    # type: () -> str | None
    builddate = None
    if not getattr(sys, 'frozen', False) and not hasattr(  # type: ignore[misc]
            sys, '_MEIPASS'):
        return builddate
    path = os.path.join(getIncludePath(), 'builddate.txt')
    if os.path.exists(path):
        with open(path, 'r') as f:
            builddate = f.read()
    return builddate
```
The build date is just for logging. We don't need a message box in that case, just log.
```python
        try:
            with open(path, 'r') as f:
                builddate = f.read()
        except OSError as e:
            logger.error(f"Failed to get builddate from {path}\n{e}",
                         exc_info=True)
```

Will there be a problem when it's None? No, where we're using it already handles that:
```python
RETYPE_VERSION_STR = __version__
RETYPE_BUILDDATE_STR = getBuilddate()
RETYPE_BUILDDATE_DESC = f' built @ {RETYPE_BUILDDATE_STR}' \
    if RETYPE_BUILDDATE_STR else ''
```

## 1.7.1
I think that's all for now. These fixes will be released in retype 1.7.1.

{% include fin.html %}
