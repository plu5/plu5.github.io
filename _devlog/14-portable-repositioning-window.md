---
layout: post
title: 14 — Portable repositioning window
date: 2026-04-16 15:37
modified_date: 2026-04-21 04:55
categories: xcb win32 site metaf csv
lang: en
redirect_from: /devlog/14
wip: true
---

## Linux - X
There are two options for X, Xlib (sometimes called just X11) and XCB. The former is older and written by hand, the latter is generated from the specs. Technically there could be others, because rather than a C API like in win32, the X server uses a [network protocol](https://www.x.org/releases/X11R7.7/doc/xproto/x11protocol.html), and the aforementioned are like C bindings for it.

XCB is preferred:
It's more verbose / a little bit harder to get started, but it gives you more control, ability to make non-blocking requests (the gains in speed can be enormous when you need to send many requests at once), and has a more logical and consistent interface (less idiosyncrasies).

See [this friendly rundown](https://www.x.org/wiki/guide/xlib-and-xcb/) by Alan Coopersmith.

TODO maybe show the same thing in both of them

I stayed up late reading [this xcb tutorial](https://www.x.org/archive/X11R7.5/doc/libxcb/tutorial/index.html)

I have experience in win32 and it seems pretty familiar to me, other than
- having to pass a connection object around
- having to subscribe to events you want rather than receiving all of them and deciding which ones to handle (the tutorial made a point of how commonly people forget this, and indeed in my searches I came across several people confused by why they are not receiving an event)
- (there is also the huge difference that there are no "base controls" to use like a button, text edit, list view)

[This series of articles by Chuan Ji](https://jichu4n.com/posts/how-x-window-managers-work-and-how-to-write-one-part-i/) about window managers is also useful. Notes:
- The difference between X and macOS and Unity is exaggerated, it's not really true that a program couldn't function independently of the window manager, menus are not a critical function.
- First diagram: There doesn't have to be a GUI framework between X and an application.
- "In X terminology, all top level windows and all UI elements within are _windows_. In other words, a _window_, is any rectangular area that is an unit of user interaction and/or graphic display." -- this is the same in win32.
- There was meant to be more but he abandoned after part 3 (but there was a gap of 3 years between part 2 and 3, so maybe? but it's been 9 years since then)

## Linux - Wayland
maybe impossible?

## Windows

tested on windows 10 and windows 98

## macOS

## ------ In other news ------

## Site: /notes and /article
On the 18th added pages [/notes](/notes) and [/article](/article) in line with what I said on [devlog 13](/devlog/13). Used `redirect_from` to make [/note](/note) and [/articles](/articles) work also to mitigate the singular/plural confusion.

At the beginning of this site there was no /article/, all the posts were on / and there were no other collections. /article/ was added because that's what I saw on Politico which I idolised at the time (was a fan of the [London Playbook](https://www.politico.eu/newsletter/london-playbook/) -- I don't recommend subscribing unless you work in politics, you probably have better things to do).

## Site: Image overflow
There was a problem with images overflowing on mobile devices. Preventing this seems to be as simple as:
```css
img {
  max-width: 100%;
}
```
Subsequently the images resize to fit.

## Metaf: `--update`
My [metaf script](https://github.com/plu5/dotfiles/blob/main/pm/scripts/metaf.py) got an update: the `--update` option.

It's a script I wrote in January that collects metadata for files under a path (user choice from type, creation date, modified date, dates as formatted strings or seconds from epoch or both) and output or save to file the resulting data. Each type of metadata is represented by a character: `t` type, `C` formatted creation date, `c` creation date as seconds from epoch, and the same idea with `M` and `m` for modified date. With these characters the user can specify which data to output and the order they will be in, for example `tCM` for type, formatted creation date, formatted modified date (in that order).

The script is designed for Linux, but it works on Windows so long as you don't choose type (`t`) (which depends on `file --mime-type`; other than that there is no easy way to get the type AFAIK).

To add new types of metadata to save, there are only two places:
- Add an entry to `FORMATOPTIONS` dict; the key is the character to represent it, the value is the key it will be saved under in the json structure.
- Add function to `dispatch` in `get_file_information` that returns the data for it.

## Metaf: `--quiet`, `--sort`, name, path, checksum
Other ideas for it:
1. `-q`/`--quiet`
2. sort by one of the fields (`--sort`)
3. option to save as csv instead of json (this could reduce significantly the size of the output, which could be useful in large folders. the output for my home folder for example is 176 MiB) (`--exporter csv`)
4. add metadata: name, path (and ability to sort by them)
5. add metadata: checksum

19th: Implemented 1-2 and 4-5 easily. 3 is complicated.

For sort (2) it's the same characters you use in format, with optionally `r` as the second character to indicate reverse. Example: cr -> sort by creation epoch, reverse order.

This script is quite slow in folders with a lot of files, especially if you have a lot of metadata options in format (especially type and checksum), but once you have the data, subsequent runs with `--update` are fast, and you can also `--sort` quickly to your heart's content. In other words, once we have the data it's fast to manipulate it, but the initial scan is slow.

20th: Added `--dryrun`, which can be useful if you want to see what `--update` would do without modifying the file. A slight issue with it: I'm not sure if in that case we should stdout what would have been written to disk or not. Currently I'm doing it, but it can be a lot of output if all the user wants to see is what changed (which messages at the beginning of the output indicate, you don't have to diff, it's just that I output these messages _as well as_ what would have been written to the file).

## Metaf: "csv"
Problems with csv:
- We don't have a flat structure to save. I would have to write my own exporter and parser and it would no longer really qualify as csv but some kind of csv-inspired custom format.
- The csv module doesn't have a `.dumps()` equivalent, you have to write to a file, and using stdout as the file [is borked on Windows](https://stackoverflow.com/questions/23665264/how-to-write-csv-output-to-stdout). †
  + † It's actually possible nevertheless to turn it to a string, because the parameter `csvfile` that `csv.writer` and `csv.DictWriter` take can accept any object that implements a `write()` method. Courtesy of [this Medium article](https://levelup.gitconnected.com/building-csv-strings-in-python-32934aed5a9e) by Bhavesh Poddar.
  + It's also possible with StringIO: [article by Dimitrije Stamenic](https://stackabuse.com/how-to-convert-a-list-into-a-csv-string-in-python/)
  + StringIO can also be used in the other direction, with a reader: [SE 2021](https://stackoverflow.com/questions/66549575/parsing-csv-string-with-csv-module), [SE 2010](https://stackoverflow.com/questions/3305926/python-csv-string-to-array)
- The problem of importing the data for when running with `--update`, as we don't necessarily know what format the file is in.
  + Idea 1: First `json.load` and catch `json.decoder.JSONDecodeError` and try with csv, since json is a lot stricter and most(?) csv files could not be interpreted as json.
  + Idea 2: Include a field with a string that we know is unique to us and indicates the format. As near as possible to the start of the file, to avoid having to search the entire file.

The json structure is:
```json
{
  "generated": "2025-04-20",
  "generated_epoch": 0,
  "files": {
    "file1": {"creation": "..", ..},
    "file2": {"creation": "..", ..}, ..
  }
}
```
To represent as csv, we need to flatten it somehow.

Maybe:
```text
key,creation,creation epoch,..
generated,"2025-04-20",0
file1,"..",..
file2,"..",..
```
But can't guarantee number of columns nor which there will be. But there will be at least one. Maybe each metadata key that's not a file in its own line with the key name encoded, and ignoring the headers:
```text
<<metaf:generated,2025-04-20
<<metaf:generated_epoch,0
```
Then we can extract the lines that start with `<<metaf:`.

```python
MAGICCSVKEYPREFIX = '<<metaf:'

class CsvOutput():
    """Passed to csv module to get it write into a string instead of a file.
    Adapted from Bhavesh Poddar:
    https://levelup.gitconnected.com/building-csv-strings-in-python-32934aed5a9e
    """
    def __init__(self):
        self.lines = []

    def write(self, line):
        self.lines.append(line)

    def __str__(self):
        return ''.join(self.lines)

def export_csv(d):
    # type: (dict) -> str
    files = d['files']
    # key header so that we can convert to a dictionary later, and the
    # rest of the headers are the metadata fields on the first file
    # (they should all have the same fields)
    fieldnames = ['key', *list(files[list(files.keys())[0]].keys())]

    rows = []
    rows.append(fieldnames)
    for k, v in d.items():
        if k != 'files':
            # non-file keys each in their own row, because we can't
            # guarantee there will be more than 1 column
            rows.append([f'{MAGICCSVKEYPREFIX}{k}'] + [str(v)])
    for k, data in files.items():
        rows.append([k] + [str(v) for v in data.values()])

    out = CsvOutput()
    writer = csv.writer(out)
    writer.writerows(rows)

    return str(out)
```
And now for the reverse; get a string back to the dictionary we started with:
```python
def parse_csv(s):
    # type: (str) -> dict
    res = {}
    rows = list(csv.DictReader(s.splitlines()))
    keys = list(rows[0].keys())
    i_files_start = 0

    # general metadata fields
    for i, row in enumerate(rows):
        key = row['key']
        if key.startswith(MAGICCSVKEYPREFIX):
            # the value is in the 2nd column, we don't care what it is
            res[key[len(MAGICCSVKEYPREFIX):]] = row[keys[1]]
        else:
            i_files_start = i
            break

    # file data
    files = res['files'] = {}
    for row in rows[i_files_start:]:
        f = files[row['key']] = {}
        for field in keys[1:]:
            # REMARK(plu5): Epochs are the only non-string value we
            # are storing currently, so this does the job despite
            # being an ugly hack (I apologise)
            f[field] = float(row[field]) if 'epoch' in field else row[field]

    return res
```

As for the problem of loading the file now that we have two different formats and are not sure which one it is, I went with idea 1 because I wanted to do it in the simplest way for now and leave it to future me to refactor if it becomes necessary (evergreen)
```python
def read_existing(path):
    # type: str -> dict | None
    existing = None
    with open(path, "r") as f:
        # REMARK(plu5): At the moment we have just 2 exporters, json
        # and csv. json will fail to parse csv files so I am doing it
        # that way, but it's not very robust and will make it hard to
        # add other exporters. Possibly should add a magic string to
        # our exports to be able to check it to see which format we
        # are dealing with
        try:
            existing = json.load(f)
        except json.decoder.JSONDecodeError:
            msg("File contents are not valid json, trying csv.")
            f.seek(0)  # needed to be able to read after the failed json.load
            existing = parse_csv(f.read())
    return existing
```

In implementing this I learned something i didn't know: apparently doing `f.read()` or `json.load(f)` "consumes the flux" and you have to do `r.seek(0)` to return to the start if you want to do it again ([pythonmorsels](https://www.pythonmorsels.com/seeking-in-files/)).

A problem that I left unsolved is that it still saves by default into `metaf.json` (`metaf.csv` would make more sense). I don't really want to fix this because it would complicate a bit and make it so `--update` won't work with csv without specifying the save file.

Resource I came across and found cool:
[2017.compciv.org enjoyable introduction to csv](http://2017.compciv.org/guide/topics/python-standard-library/csv.html) ("then it becomes obvious the official CSV specification is more or less considered a disaster.")

## emacs-doentry: Proprening
Proprening = rendering a thing more proper.

The proprening is thanks in part to things I learned while working on braille.el. Header comments, defcustoms, naming of things that conforms with expectations. I did however leave the function `create-doentry` named as is, I don't really want to rename it even though you're meant to name everything prefixed by the name of the package.

The doentry folder is now defined by the customisable variable `doentry-gen-dir` (I named it dir instead of folder to conform with what seems to be the convention), which is nil by default and fine to leave as nil; in that case the entries are generated under pwd and n is not calculated / falls back to 1.

## emacs-doentry: Metaf integration
The reason I am working on it:
The point of the metaf changes was in fact to speed up calculating next n for [emacs-doentry](https://github.com/plu5/emacs-doentry) gen, but it feels weird to add this coupling with the external script. We need a defcustom at least, and make it off by default. Or just make the function used customisable:
```elisp
(defcustom doentry-gen-n-function 'doentry-gen-n-string
  "Function to use to find the n of the previous doentry
if {n} is used in `doentry-gen-template'."
  :type 'function
  :group 'doentry-gen)
```
Then we can call it with `funcall`
```elisp
       (string-replace "{n}" (funcall doentry-gen-n-function)
                       doentry-gen-template)
```

This is how the n is obtained by default:
```elisp
(defcustom doentry-gen-n-regexp "# \\([0-9]+\\) |"
  "Regexp to use to find the n of the previous doentry
if {n} is used in `doentry-gen-template'.
The first match group should be the n."
  :type 'regexp
  :group 'doentry-gen)

(defun doentry-gen-latest-file (path &optional match)
  "Return latest file in PATH matching MATCH.
PATH must not be nil."
  (car (sort (directory-files path 'full match t) #'file-newer-than-file-p)))

(defun doentry-gen-get-file-contents (path)
  (with-temp-buffer
    (insert-file-contents path)
    (buffer-string)))

(defun doentry-gen-latest-file-contents ()
  "Return string contents of latest doentry in `doentry-gen-dir' or nil."
  (when doentry-gen-dir
    (let ((latest-log-path
           (doentry-gen-latest-file doentry-gen-dir ".*\\.doentry$")))
      (doentry-gen-get-file-contents latest-log-path))))

(defun doentry-gen-n-in-string (contents)
  (if (string-match doentry-gen-n-regexp contents)
      (+ 1 (string-to-number (match-string 1 contents)))
    1))

(defun doentry-gen-n ()
  "Return next doentry number.
Returns 1 if the previous number is not found, there is no previous
file, or `doentry-gen-dir' is nil."
  (if doentry-gen-dir
      (doentry-gen-n-in-string (doentry-gen-latest-file-contents))
    1))

(defun doentry-gen-n-string ()
  "Calculates next n based on previous doentry and returns as a string
with 5 digits, for example 00001.
This function is the default `doentry-gen-n-function'. You can replace it
with a any function that takes no argument and returns a string."
  (format "%05d" (doentry-gen-n)))
```

This is slow if there are a lot of files in `doentry-gen-dir`; it has to sort them by modification date. It also could be that the most recently modified file is not the latest created, in which case the resulting number will be off.

With metaf we can use `--sort cr` to make it so that the first file in the listing is the most recently created, then extract it with regexp. I added these two additional functions:
```elisp
(defun doentry-gen-n-metaf-first-doentry ()
  (when doentry-gen-dir
    (with-temp-buffer
      (insert-file-contents
       (expand-file-name "metaf.json" doentry-gen-dir))
      (goto-char (point-min))
      (when (re-search-forward
             "^\\([A-Z0-9]\\{32\\}.doentry\\)," nil t 1)
        (expand-file-name (match-string 1) doentry-gen-dir)))))

(defun doentry-gen-n-metaf ()
  "Calculates next n based on metaf.py csv file metadata output.
This function is not used by default. `doentry-gen-n-function' can be
set to it to use it for calculating the n instead of
`doentry-gen-n-string'. It requires `doentry-gen-dir' to be set and
for the metadata-listing script metaf.py to be present in
`exec-path'."
  (when doentry-gen-dir
    (with-temp-buffer               ; run without popping buffer
      (shell-command                ; the output will be in *Messages*
       (concat "metaf.py -usx csv --sort cr '" doentry-gen-dir "'")
       t)
      (message "metaf.py output: %s" (buffer-string)))
    (let* ((latest-entry (doentry-gen-n-metaf-first-doentry))
           (next-n (doentry-gen-n-in-string
                    (doentry-gen-get-file-contents latest-entry))))
      (format "%05d" next-n))))
```
`shell-command` normally pops a buffer with the output which is quite annoying. Passing `t` to it after the command makes it insert the output instead, so I wrap it in `with-temp-buffer` and message the resulting buffer content (stdout of the command) so that we can still look at the output if necessary.

Calling metaf with `-u` (`--update`) is significantly faster than sorting the files by modification date, especially since all that changes from run to run is 2 files (the previously created entry, and metaf.json).

You can configure doentry-gen to use this with:
```elisp
(setq doentry-gen-n-function 'doentry-gen-n-metaf)
```
or in `customize-group doentry-gen`.

(Commit [b950d5b](https://github.com/plu5/emacs-doentry/commit/b950d5bf6f48c3de28fb059051d3a878ee218607))

## emacs-doentry: Autoload
`create-doentry` seems like a good usecase for autoload, to make it so that the package isn't loaded until it is called, but I am not sure how to use it. I put an autoload cookie on that function:
```elisp
;;;###autoload
(defun create-doentry ()
  ;; ..
```
but as [phils says](https://stackoverflow.com/questions/36994731/how-autoload-works), "On their own, these comments are just comments."

> A separate process is used to extract the associated definitions into a loaddefs.el file.

[Stefan](https://stackoverflow.com/questions/20084176/where-should-i-add-autoload-cookies-in-my-emacs-lisp-package-is-there-a-definit):

> The `;;;###autoload` cookies simply mark code which needs to be lifted into a `<pkg>-autoloads.el` file.

I don't understand when we are supposed to generate this file and how to load it. It's not something that is supposed to be committed; looking at [markdown-mode](https://github.com/jrblevin/markdown-mode/) for example, there is a `markdown-mode-autoloads.el` for me in `~/.emacs.d/elpa/markdown-mode-20251204.852/markdown-mode-autoloads.el` but not in the repository.

> The package manager processes the autoload cookies for any given package, so package authors can simply add those comments as appropriate.

See also [SE 2015: What's the mechanism to ensure the autoload functions in emacs autoloaded](https://stackoverflow.com/questions/28889446/whats-the-mechanism-to-ensure-the-autoload-functions-in-emacs-autoloaded).

[According to xuchunyang](https://emacs.stackexchange.com/a/8027) I could use `package-install-from-buffer`, but I'd rather not, I'd rather load it in elisp in the normal way.

How to make use of autoload for el files not installed with the package manager? According to tom in the comments of the [previous SE question](https://stackoverflow.com/questions/36994731/how-autoload-works), `(autoload 'my-func-A "my-func")`, where `my-func-A` is the name of the function to autoload, and `my-func` is the name of the module. I guess this needs to be used instead of `require`. Irrespective of any autoload comments; they are irrelevant for this.

In the markdown-mode readme he mentions that too for how to load it if installing by "direct download":
```elisp
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
```
The `t` is to make it visible in `M-x`. For our case:
```elisp
(autoload 'create-doentry "doentry-gen"
   "Generate a doentry file" t)
```
I tested in `emacs -Q` and indeed it works (after first adding the folder that contains doentry-gen to `load-path` of course: `(add-to-list 'load-path "/path/to/emacs-doentry")`), and irrespective of any autoload comment. But I might as well leave the comment for if someone wants to install it as a package.

{% include fin.html %}
