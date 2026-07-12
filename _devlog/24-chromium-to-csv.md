---
layout: post
title: 24 — Chromium to csv
date: 2026-07-06 18:58
modified_date: 2026-07-09 02:26
categories: chromium json csv python
lang: en
redirect_from: /devlog/24
---

## What
After having done stuff with Firefox bookmarks and history in [d23](/devlog/23), I can't suppress the thought of doing something similar for Chrome/Chromium data, for "completion" (and self-torture).

## Length of time it keeps history
IIRC it does not keep history for very long.

Looking at `chrome://history/` :
Today is 6 July 2026 and the earliest thing in history is 7 April 2026 23:26. So it keeps 3 months.

## Data location
The bookmarks are in `~/.config/chromium/Default/Bookmarks` as JSON

History in `~/.config/chromium/Default/History`, SQLite 3 DB

I'm thinking of calling my script chrom2csv and having a similar interface to places2csv.

## Bookmark structure, extract URLs
To begin with, let's try to output a list of URLs, without regard for the order or other fields.

This is conceptually not as easy as it was with places.sqlite, because the bookmarks are in a nested structure. Simplifying and ignoring some fields, it's roughly like this:
```json
{
  "roots": {
    "bookmark_bar": {
      "children": [
        {
          url: "oieafrwo"
        },
        {
          url: "wofiuwhhe"
        },
        {
          "children": [
            {
              url: "wofiuwhhe"
            }
          ]
        }
      ]
    }
  }
}
```

In Firefox bookmarks and folders can be under toolbar, menu, unfiled. In Chromium they're all on the toolbar (bookmarks bar):

> The bookmarks bar is the home of all of the bookmarks the user creates. We wished to avoid creating multiple separate locations for bookmarks as each additional section is yet another location to lose things in.  
—[*chromium.org/user-experience/bookmarks*](https://www.chromium.org/user-experience/bookmarks/)

{% include note.html content='
> [!NOTE]
> Turns out not to be true in the data structure, as bookmarks in the "Other bookmarks" folder, which is ostensibly on the bookmarks bar, are not stored in the bookmarks_bar JSON object.
' %}

```python
#!/usr/bin/env python3
# chrom2csv
# 2026-07-06 19:19
import json
from os.path import expanduser

FILE = expanduser("~/.config/chromium/Default/Bookmarks")

with open(FILE, 'r') as f:
    data = json.load(f)

for thing in data:
    print(thing)
```
There are 3 objects at the top level:
- checksum
- roots
- version

checksum and version are just strings.

```python
for thing in data.get('roots'):
    print(thing)
```
3 objects in roots:
- bookmark_bar
- other
- synced

Ah, so my assumptions are wrong already, if "other" is the "Other bookmarks" folder. I thought it would be under the bookmarks bar object because it presents itself like a subfolder, albeit a sticky one.

> The 'Other Bookmarks' folder is a special, right-aligned always-visible folder that is a dumping ground for bookmarks that the user doesn't want to show in their prime bookmarks area (the left side).  
—[*chromium.org/user-experience/bookmarks*](https://www.chromium.org/user-experience/bookmarks/)

Each of the three (`bookmark_bar`, `other`, `synced`) contain:
- `children`
- `date_added`
- `date_last_used`
- `date_modified`
- `guid`
- `id`
- `name`
- `type`

Which I think is the standard structure for any folder. And indeed the type on each of them is 'folder'.

```python
count = 0

def explore(d):
    global count
    t = d.get('type')
    if t == 'folder':
        for c in d.get('children', []):
            explore(c)
    elif t == 'url':
        count += 1
    else:
        print(t)

explore(data['roots']['bookmark_bar'])
explore(data['roots']['other'])
explore(data['roots']['synced'])
print(count)
```

The only types seem to be "folder" and "url".

I have nothing in other and synced and 249 in bookmark bar.

Counting function without a global variable:
```python
def explore(d):
    t = d.get('type')
    if t == 'folder':
        return sum(explore(c) for c in d.get('children', []))
    elif t == 'url':
        return 1
    return 0
```

Each URL looks like this:
```json
{
  'date_added': '13382870730724551',
  'date_last_used': '13426220424008143',
  'guid': '1fdbffb7-e849-4275-97ff-08ea13cbb506',
  'id': '33',
  'name': '',
  'type': 'url',
  'url': 'https://duckduckgo.com/'
}
```
`date_last_used` is often 0 (which I suppose means the bookmark was never clicked on).

For getting all the URLs it seems like we can do this:
```python
def explore(d):
    t = d.get('type')
    if t == 'folder':
        return [explore(c) for c in d.get('children', [])]
    elif t == 'url':
        return d.get('url')
```
But we end up with some nested lists.

I thought about doing:
```python
def explore(d):
    t = d.get('type')
    if t == 'folder':
        return sum(explore(c) for c in d.get('children', []))
    elif t == 'url':
        return [d.get('url')]
    return []
```
but:
> TypeError: unsupported operand type(s) for +: 'int' and 'list'

and the same thing happens with `sum([[1], [2]])`. Where does the int come from?

> Before you can use sum() with non-numeric values, you need to provide a start value.  
—[*stevenjd*](https://www.reddit.com/r/Python/comments/r1734w/is_there_a_reason_lists_dont_have_a_sum_method/hm1cumv/)

and indeed it works if we pass `[]` as the second argument to `sum`:
```python
def explore(d):
    t = d.get('type')
    if t == 'folder':
        return sum((explore(c) for c in d.get('children', [])), [])
    elif t == 'url':
        return [d.get('url')]
    return []
```

but he goes on:

> The reason that `sum()` intentionally prohibits you from calling it with strings is that summing strings risks *extremely* slow O(N^2) performance. That same applies to summing lists, but the core developers decided that summing lists is sufficiently rare that its not worth prohibiting it, whereas summing strings is a very common anti-pattern.

so I guess I should not be summing lists.

[SE](https://stackoverflow.com/questions/1720421/how-do-i-concatenate-two-lists-in-python)

> `sum` performs concatenation in a pairwise fashion, which means this is a quadratic operation as memory has to be allocated for each step. DO NOT USE if your lists are large.  
—*coldspeed95*

Maybe:
```python
def explore(d):
    t = d.get('type')
    if t == 'folder':
        return [url for c in d.get('children', []) for url in explore(c)]
    elif t == 'url':
        return [d.get('url')]
    return []
```

Or we could avoid lists in this function altogether by yielding each URL, and the caller can convert to a list. Or another function.
```python
def get_urls_generator(d):
    t = d.get('type')
    if t == 'folder':
        for c in d.get('children', []):
            yield from get_urls_generator(c)
    elif t == 'url':
        yield d.get('url')

def get_urls(d):
    return list(get_urls_generator(d))
```

cf [Praveen Gollakota on `yield from g` vs `for v in g: yield v`](https://stackoverflow.com/a/26109157/18396947)

## Interface and csv export
Adapted from places2csv:
```python
#!/usr/bin/env python3
# chrom2csv
# 2026-07-06 19:19
"""Extracts selected information from Chromium bookmarks

Usage:
    chrom2csv b ~/.config/chromium/Default/Bookmarks
"""
import os
import sys
import csv
import json
import argparse

PROG = 'chrom2csv'
DEFAULTSAVENAME = 'bookmarks.csv'
QUIET = False

BOOKMARK_ROOTS = 'roots'
TOPLEVEL_BOOKMARK_FOLDERS = ["bookmark_bar", "other", "synced"]

COMMANDS = {
    "":                         # TOP LEVEL ARGS
    {"args": [
        (["--dryrun"], {"action": "store_true", "help": """
Don't write to disk."""}),
        (["-q", "--quiet"], {"action": "store_true", "help": """
Suppress messages in output."""}),
    ]},
    "b":                        # B (bookmarks)
    {"desc": "extract bookmarks data", "usage": """[-s] [-t SAVE_TO] [-o] [-u]
file""",
     "args": [
         (["file"], {"help": """Bookmarks file.
e.g. ~/.config/chromium/Default/Bookmarks"""}),
         (["-s", "--save"], {"action": "store_true", "help": f"""Save output to
file instead of printing to stdout. Will save to ./{DEFAULTSAVENAME} unless a
different path is provided with the -t/--save-to option."""}),
         (["-t", "--save-to"], {"help": """Path to save output to.
Ignored if -s/--save is not provided."""}),
         (["-o", "--overwrite"], {"action": "store_true", "help": """Overwrite
save file if it already exists. Ignored if -s/--save is not provided."""}),
         (["-u", "--update"], {"action": "store_true", "help": """Update save
file if it already exists. Ignored if -s/--save is not provided.
If both -o/--overwrite and -u/--update are provided,
overwrite will take precedence."""}),
     ]},
}


def msg(text):
    # type: (str) -> None
    """Utility for logs/errors, could be replaced with proper logging"""
    if not QUIET:
        print(f'{PROG}: {text}')


def get_bookmarks_folder_generator(d):
    t = d.get('type')
    if t == 'folder':
        for c in d.get('children', []):
            yield from get_bookmarks_folder_generator(c)
    elif t == 'url':
        yield {'url': d.get('url')}


def get_bookmarks_toplevel_generator(d):
    for g in [get_bookmarks_folder_generator(d.get(f, {}))
              for f in TOPLEVEL_BOOKMARK_FOLDERS]:
        yield from g


def get_bookmarks(d):
    return list(get_bookmarks_toplevel_generator(d[BOOKMARK_ROOTS]))


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


def export_csv(a):
    # type: (list) -> str
    fieldnames = [*list((a[0] if len(a) else {}).keys())]
    rows = [fieldnames]
    for item in a:
        rows.append([str(v) for v in item.values()])

    out = CsvOutput()
    writer = csv.writer(out)
    writer.writerows(rows)

    return str(out)


def cmd_b(args):
    save_to = args.save_to or DEFAULTSAVENAME

    if args.update:
        raise NotImplementedError()

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(save_to):
            if not args.update:
                msg(f'Save file \'{save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

    with open(args.file, 'r') as f:
        data = json.load(f)

    bookmarks = get_bookmarks(data)

    dump = export_csv(bookmarks)

    if args.save and not args.dryrun:
        open_as = 'w' if args.overwrite or args.update else 'x'
        with open(save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {save_to}')
    else:
        print(dump)


def parse_args():
    parser = argparse.ArgumentParser(prog=PROG)
    parser.suggest_on_error = True
    subparsers = parser.add_subparsers(dest="command", required=True)
    descs = []
    for k, v in COMMANDS.items():
        if v.get('desc'):       # description generation
            descs.append(f"{v['desc']} ({k})")
        if k == "":             # arguments on top parser
            for args, kwargs in v['args']:
                parser.add_argument(*args, **kwargs)
            continue
        subparser = subparsers.add_parser(
            k, help=f"[-h|--help] {v.get('usage')}")
        for args, kwargs in v['args']:
            subparser.add_argument(*args, **kwargs)
        # Set handler
        subparser.set_defaults(func=v.get('func'))
    # Add generated description
    if len(descs) > 1:
        descs[-1] = f"or {descs[-1]}"
    parser.description = f"A utility script to \
{' '.join(descs)} from Chromium files."
    # Parse args / print help and quit if no args
    # (Primer https://stackoverflow.com/a/47440202/18396947)
    return parser.parse_args(sys.argv[1:] or ['--help'])


COMMANDS['b']['func'] = cmd_b


def main():
    # type: () -> None
    global QUIET

    args = parse_args()
    QUIET = args.quiet

    args.func(args)


if __name__ == '__main__':
    main()
```

## Update?
How to implement update? I think due to the nature of this structure there won't be much benefit. But maybe we still want this functionality in order to keep existing data that is no longer in the bookmarks file. There are also no last modified dates on the bookmarks. But I guess this is irrelevant since we are not making a query here for bookmarks after a certain time, we necessarily have to traverse the whole structure because god knows where new bookmarks will be nested.

If we have IDs what update could mean is add bookmarks we don't have yet, replace ones we don't have yet, and don't touch the rest of the existing data.

[this is actually not going to work, because what if there are fields that are missing in the existing data?]
[for bookmarks that are still there they would be replaced, for bookmarks that are gone they can be populated with None or something. maybe there would be a warning in that case. or maybe this whole idea is stupid]

We don't yet have IDs so we'll leave it unimplemented for now.

## Bookmarks IDs and format options
To add IDs we can simply yield `{'id': d.get('id'), 'url': d.get('url')}` instead of `{'url': d.get('url')}` in `get_bookmarks_folder_generator`.

But let's refactor this to make the format options and order selectable, like in places2csv.

```python
BFORMATOPTIONS = {
    'i': 'id', 'u': 'url',
}
DEFAULTBFORMATOPTIONS = 'iu'
```

Add CLI arg (copied from places2csv, which was itself copied from metaf):
```python
         (["-f", "--format"], {"default": DEFAULTBFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTBFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in BFORMATOPTIONS.items()])}."""}),
```
and add `[-f FORMAT]` to the usage string.

Then
- change the call to `get_bookmarks` in `cmd_b` to pass also `args.format`
- change `get_bookmarks` to accept `fmt` and pass it on to `get_bookmarks_toplevel_generator`
- change `get_bookmarks_toplevel_generator` to accept `fmt` and pass it on to `get_bookmarks_folder_generator`
- change `get_bookmarks_folder_generator` to accept `fmt` and instead of yielding `{'id': d.get('id'), 'url': d.get('url')}`, yield a new function which we'll call `get_bookmark_information`, passing to it `fmt` and `d`.

Same old logic from places2csv and metaf before it (even though the lambdas are pointless):
```python
def get_bookmark_information(d, fmt):
    res = {}

    dispatch = {
        'i': lambda: d.get('id'),
        'u': lambda: d.get('url'),
    }

    for char in fmt:
        opt = BFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res
```

## Sort
Let's get sorting working next.

CLI option:
```python
         (["--sort"], {"choices":
                       [x for k in BFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: ir -> sort by id, reverse order.)"""}),
```

Add `[--sort SORT]` to usage text

Then this in `cmd_b` right before export csv:
```python
    def sort_key(item):
        k = BFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = ''
        return data_to_sort_by

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        msg(f'Sorting by {args.sort[0]}{" in reverse" if reverse else ""}')
        bookmarks.sort(key=sort_key, reverse=reverse)
```

## Name, guid
We can easily add format options name and guid
```python
BFORMATOPTIONS = {
    'i': 'id', 'u': 'url', 'n': 'name', 'g': 'guid',
}
DEFAULTBFORMATOPTIONS = 'iun'
```
(didn't add guid to the default format options)

Add to dispatch in `get_bookmark_information`:
```python
        'n': lambda: d.get('name'),
        'g': lambda: d.get('guid'),
```

## Date added, date last used
Now for the dates. It's similar, except that I also want to add human-readable date options.
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch', 'C': 'used', 'c': 'used epoch',
    'i': 'id', 'u': 'url', 'n': 'name', 'g': 'guid',
}
DEFAULTBFORMATOPTIONS = 'iunaAcC'
```
Unfortunately we can't use u for used due to it being url, so I made it c like clicked. I didn't want to make it m, because I don't want users to confuse it with date modified, which we unfortunately don't have.
[but c could be confused with "created"]

[maybe it should have been l for last used?]

The format for the epoch time is not the same as in places.sqlite, where it is in microseconds, 16 digits. Here it's 17. Too much for microseconds, not enough to be nanoseconds.
```json
{
  'date_added': '13382870730724551',
  'date_last_used': '13426220424008143',
  'guid': '1fdbffb7-e849-4275-97ff-08ea13cbb506',
  'id': '33',
  'name': '',
  'type': 'url',
  'url': 'https://duckduckgo.com/'
}
```

> Chromium timestamps are counted in (integer) microseconds since 1 January 1601 00:00 UTC. This is the same epoch as Windows uses, but each Windows time unit represents 100 nanoseconds.  
—[*Wikiversity: Chromium browsing history database*](https://en.wikiversity.org/wiki/Chromium_browsing_history_database)

It's microseconds but not the same epoch.

> Conversion to Unix timestamps requires division by 1,000,000 and subtraction of 11,644,473,600 time units.

```python
DATEFORMAT = '%F %T.%f'

def readable_date_from_epoch(e):
    """Takes epoch in microseconds since 1 January 1601 00:00 UTC"""
    # type: (int) -> str
    return datetime.fromtimestamp(e/1000000-11644473600).strftime(DATEFORMAT)
```

Does date added change when you modify a bookmark?

Before name change:
```python
{'date_added': '13427932619283451', 'date_last_used': '0', 'guid': 'b8974efe-af20-4386-8ba1-215d9e6e0a35', 'id': '353', 'meta_info': {'power_bookmark_meta': ''}, 'name': 'test', 'type': 'url', 'url': 'http://google.com/'}
```
After name change:
```python
{'date_added': '13427932619283451', 'date_last_used': '0', 'guid': 'b8974efe-af20-4386-8ba1-215d9e6e0a35', 'id': '353', 'meta_info': {'power_bookmark_meta': ''}, 'name': 'test2', 'type': 'url', 'url': 'http://google.com/'}
```
Nothing changes other than the name. I thought there was a chance it would delete and recreate.

Adding the new fields to `get_bookmark_information`:
```python
    a = int(d.get('date_added'))
    c = int(d.get('date_last_used'))

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(a) if a else None,
        'c': lambda: c,
        'C': lambda: readable_date_from_epoch(c) if c else None,
        'i': lambda: d.get('id'),
        'u': lambda: d.get('url'),
        'n': lambda: d.get('name'),
        'g': lambda: d.get('guid'),
    }
```
The epoch dates are stored as strings in the json.

## Path, position
We've got all the fields now that are on the bookmark object itself (other than meta info, but I don't know what that is), but it'd be also useful to know where a bookmark is located in the hierarchy. For example "other/subfolder", or "bookmark bar/codebases/github".

It's also notable that bookmarks are lacking a position field. Can't they be reordered in a folder? Maybe what happens then is the order of the objects changes in the json.

Also, folder names can contain `/`, so it's maybe not the ideal character to use. It didn't occur to me while working on places2csv. I don't know if I care enough to use some kind of untypeable character. Maybe all unicode characters are permitted, so we might as well choose that one.

To construct the path, `get_bookmarks_folder_generator` can have an argument `path`, and add to it as it recurses through folders:
```python
def get_bookmarks_folder_generator(d, fmt, path=''):
    t = d.get('type')
    if t == 'folder':
        for c in d.get('children', []):
            yield from get_bookmarks_folder_generator(
                c, fmt, f'{path}/{d.get('name')}')
    elif t == 'url':
        yield get_bookmark_information(d, fmt, path)
```
passing it on also to `get_bookmark_information`.

Add an option for it:
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch', 'C': 'used', 'c': 'used epoch',
    'i': 'id', 'u': 'url', 'n': 'name', 'g': 'guid',
    'p': 'path',
}
DEFAULTBFORMATOPTIONS = 'iunaAcCp'
```

Modify `get_bookmark_information` to take an argument `path`, and add to dispatch:
```python
        'p': lambda: path,
```

The resulting path has a leading slash, like "/Bookmarks bar". I think I don't mind it.

How would we go about adding position?

First let me test my theory that it's based on the order.
```json
      "other": {
         "children": [ {
            "date_added": "13427936095544805",
            "date_last_used": "0",
            "guid": "66f7c6b6-f3cd-450d-ad0e-714e46e7fddb",
            "id": "355",
            "meta_info": {
               "power_bookmark_meta": ""
            },
            "name": "test1",
            "type": "url",
            "url": "http://example.com/"
         }, {
            "date_added": "13427932619283451",
            "date_last_used": "0",
            "guid": "b8974efe-af20-4386-8ba1-215d9e6e0a35",
            "id": "353",
            "meta_info": {
               "power_bookmark_meta": ""
            },
            "name": "test2",
            "type": "url",
            "url": "http://google.com/"
         } ],
         "date_added": "13382283568010060",
         "date_last_used": "0",
         "date_modified": "13427936098302259",
         "guid": "82b081ec-3dd3-529c-8475-ab6c344590dd",
         "id": "2",
         "name": "Other bookmarks",
         "type": "folder"
      },
```
Hey look, there's a date modified! Apparently folders have them

After reorder:
```json
      "other": {
         "children": [ {
            "date_added": "13427932619283451",
            "date_last_used": "0",
            "guid": "b8974efe-af20-4386-8ba1-215d9e6e0a35",
            "id": "353",
            "meta_info": {
               "power_bookmark_meta": ""
            },
            "name": "test2",
            "type": "url",
            "url": "http://google.com/"
         }, {
            "date_added": "13427936095544805",
            "date_last_used": "0",
            "guid": "66f7c6b6-f3cd-450d-ad0e-714e46e7fddb",
            "id": "355",
            "meta_info": {
               "power_bookmark_meta": ""
            },
            "name": "test1",
            "type": "url",
            "url": "http://example.com/"
         } ],
         "date_added": "13382283568010060",
         "date_last_used": "0",
         "date_modified": "13427936214847645",
         "guid": "82b081ec-3dd3-529c-8475-ab6c344590dd",
         "id": "2",
         "name": "Other bookmarks",
         "type": "folder"
      },
```

I don't know how to keep track of position. It can't be a recursion variable because it should always be 0 for every recursion, because it only recurses on folders. Effectively we want to number only the leafs.

Well let's start by adding the option:
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch', 'C': 'used', 'c': 'used epoch',
    'i': 'id', 'u': 'url', 'n': 'name', 'g': 'guid',
    'p': 'path', 'o': 'position',
}
DEFAULTBFORMATOPTIONS = 'iunaAcCpo'
```

Make `get_bookmark_information` take a `position` argument, and add to dispatch:
```python
        'o': lambda: position,
```

Attempt:
```python
def get_bookmarks_folder_generator(d, fmt, path=''):
    if d.get('type') == 'folder':
        for c in d.get('children', []):
            path = f'{path}/{d.get('name')}'
            position = 1
            if c.get('type') == 'folder':
                yield from get_bookmarks_folder_generator(
                    c, fmt, f'{path}/{c.get('name')}')
            elif c.get('type') == 'url':
                yield get_bookmark_information(c, fmt, path, position)
                position += 1
```
Path is broken, the folders repeat:
```text
353,http://google.com/,test2,13427932619283451,2026-07-08 00:16:59.283451,0,None,/Other bookmarks,1
355,http://example.com/,test1,13427936095544805,2026-07-08 01:14:55.544806,0,None,/Other bookmarks/Other bookmarks,1
```
(and even more times if you have more things in the same path)

If I remove the `path = f'{path}/{d.get('name')}'` the top-level folder is missing:
```text
353,http://google.com/,test2,13427932619283451,2026-07-08 00:16:59.283451,0,None,,1
355,http://example.com/,test1,13427936095544805,2026-07-08 01:14:55.544806,0,None,,1
```
Previously it was:
```text
353,http://google.com/,test2,13427932619283451,2026-07-08 00:16:59.283451,0,None,/Other bookmarks
```

And as you see, position doesn't even work, it's 1 for both of them despite both being in the same path.

If I increment `position` before the yield, all that happens is they're both 2 instead of 1.

And by the way we should start position from 0 if we want to be consistent with places.sqlite position.

It works if I move it up a level, and same for path:
```python
def get_bookmarks_folder_generator(d, fmt, path=''):
    if d.get('type') == 'folder':
        position = 0
        path = f'{path}/{d.get('name')}'
        for c in d.get('children', []):
            if c.get('type') == 'folder':
                yield from get_bookmarks_folder_generator(
                    c, fmt, f'{path}/{c.get('name')}')
            elif c.get('type') == 'url':
                yield get_bookmark_information(c, fmt, path, position)
                position += 1
```

```text
353,http://google.com/,test2,13427932619283451,2026-07-08 00:16:59.283451,0,None,/Other bookmarks,0
355,http://example.com/,test1,13427936095544805,2026-07-08 01:14:55.544806,0,None,/Other bookmarks,1
```

But we still have a problem with repeating nested folders:
```text
159,https://activecaptain.garmin.com/en-GB/Map,ActiveCaptain Community,13391388839910274,2025-05-11 01:13:59.910275,0,None,/Bookmarks bar/w/w/w/w,12
```
In that case I happen to actually have a folder w inside w; the real path is "/Bookmarks bar/w/w", so it doesn't repeat 4 times, but rather each repeats twice (seemingly).

This works:
```python
def get_bookmarks_folder_generator(d, fmt, path='', recursed=False):
    if d.get('type') == 'folder':
        position = 0
        if not recursed:
            path = f'{path}/{d.get('name')}'
        for c in d.get('children', []):
            if c.get('type') == 'folder':
                yield from get_bookmarks_folder_generator(
                    c, fmt, f'{path}/{c.get('name')}', True)
            elif c.get('type') == 'url':
                yield get_bookmark_information(c, fmt, path, position)
                position += 1
```

But what we should be doing actually is only add to the path once per call:
```python
def get_bookmarks_folder_generator(d, fmt, path=''):
    if d.get('type') == 'folder':
        position = 0
        path = f'{path}/{d.get('name')}'
        for c in d.get('children', []):
            if c.get('type') == 'folder':
                yield from get_bookmarks_folder_generator(c, fmt, path)
            elif c.get('type') == 'url':
                yield get_bookmark_information(c, fmt, path, position)
                position += 1
```

## Update take 2
The only thing left for bookmarks is to implement update. We have seen that folders have date modified, but we're not storing folders, so we don't have that field in the data, and I'm not sure where we would put it. But I suppose we could go off the most recent added / last used date and go off that? It would enable us to avoid recursing into folders that haven't changed.

- parse csv
- exit (1) if we don't have at least one of the date fields, (2) if we don't have either IDs or GUIDs (we need them to be able to replace changed), (3) if the supplied format options are not all present in existing data
- if we have date added sort by it and take most recent, if we have date last used do the same, and if we have both take the most recent out of the two
- pass this date as chromium-compatible epoch into `get_bookmarks_folder_generator`
- `get_bookmarks_folder_generator` to compare this with `date_modified` on each folder it encounters, only recursing into it if `date_modified` is larger
- merge the new data with existing, replacing entries whose IDs are in existing

```python
def parse_csv(s):
    # type: str -> list
    res = []
    rows = list(csv.DictReader(s.splitlines()))
    keys = list(rows[0].keys())
    for row in rows:
        r = {}
        for field in keys:
            r[field] = row[field]
        res.append(r)
    return res


def read_existing(path):
    # type: str -> dict
    existing = None
    with open(path, "r") as f:
        existing = parse_csv(f.read())
    return existing
```

Reverse function for `readable_date_from_epoch`:
```python
def epoch_from_readable_date(d):
    """Returns epoch in microseconds since 1 January 1601 00:00 UTC"""
    # type: (str) -> int
    return int(datetime.fromisoformat(d).timestamp()*1000000)+11644473600
```

A function to help us check which fields we have on the existing data:
```python
def bhasfields(d, fmt, require_all=True):
    # type: (dict, str, bool) -> bool
    f = all if require_all else any
    return f(BFORMATOPTIONS.get(c) in d for c in fmt)
```

Merge function:
```python
def bmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    """Create dictionary from existing and new with only the fields in fmt
    and no duplicate IDs/GUIDs
    """
    if not existing:
        return new
    i = 'id' if bhasfields(existing[0], 'i') else 'guid'
    fields = [BFORMATOPTIONS[c] for c in fmt]
    by_id = {d[i]: {k: d[k] for k in fields} for d in existing}
    for d in new:
        by_id[d[i]] = d
    return list(by_id.values())
```

Start of `cmd_b` with new logic to get `existing` and `update_after`:
```python
    existing = None
    update_after = None

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(save_to):
            if not args.update:
                msg(f'Save file \'{save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

            existing = read_existing(save_to)
            if not len(existing):
                msg('Existing is empty. Either the file is empty or something '
                    'went wrong.' + S_UNDESIREDOVERWRITE)
                exit(1)
            one = existing[0]
            # Check we have at least one of the date fields
            if not bhasfields(one, 'aAcC', require_all=False):
                msg('Existing data has no date fields. Without dates '
                    'in the data there is no reference point, so everything '
                    'would have to be queried from scratch.'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check we have IDs or GUIDs
            if not bhasfields(one, 'ig', require_all=False):
                msg('Existing data has no IDs or GUIDs. Without identifiers '
                    'in the data there is no way to know which bookmarks were '
                    'modified.' + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not bhasfields(one, args.format, require_all=True):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {args.format}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent date in it
                a = c = 0
                if bhasfields(one, 'a', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['a']])
                    a = existing[-1].get(BFORMATOPTIONS['a'])
                elif bhasfields(one, 'A', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['A']])
                    A = existing[-1].get(BFORMATOPTIONS['A'])
                    a = epoch_from_readable_date(A)
                if bhasfields(one, 'c', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['c']])
                    c = existing[-1].get(BFORMATOPTIONS['c'])
                elif bhasfields(one, 'C', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['C']])
                    C = existing[-1].get(BFORMATOPTIONS['C'])
                    c = epoch_from_readable_date(C)
                update_after = a if a > c else c
                msg('Updating bookmarks after '
                    f'{update_after} '
                    f'({readable_date_from_epoch(int(update_after))})')

    with open(args.file, 'r') as f:
        data = json.load(f)

    bookmarks = get_bookmarks(data, args.format, update_after)

    if update_after and not bookmarks:
        msg("No new data")

    bookmarks = bmerge(
        existing, bookmarks, args.format) if update_after else bookmarks
```
["no new data" rather than "no new bookmarks" to avoid confusing, because even if the user did not add or modify any bookmarks before updating, there could still be "new bookmarks" which are ones we already have in the same form, since our `update_after` is only an approximation going off the most recent date we have, which could be less than the folders last modified date]

Passing it on:
```python
def get_bookmarks(d, fmt, update_after=None):
    return list(get_bookmarks_toplevel_generator(
        d[BOOKMARK_ROOTS], fmt, update_after))
```
and on:
```python
def get_bookmarks_toplevel_generator(d, fmt, update_after=None):
    for g in [get_bookmarks_folder_generator(d.get(f, {}), fmt, update_after)
              for f in TOPLEVEL_BOOKMARK_FOLDERS]:
        yield from g
```

Then `get_bookmarks_folder_generator` checking it at the start:
```python
        if update_after and d.get('date_modified', 0) < update_after:
            return
```
also we have to put this argument before the path argument, otherwise it gets passed on to the wrong thing and we get "None" at the beginning of all the paths ("None/Other bookmarks").
```python
def get_bookmarks_folder_generator(d, fmt, update_after=None, path=''):
```

It's failing when it sorts after merging
```sh
$ chrom2csv b ~/.config/chromium/Default/Bookmarks -st poschromb.csv --sort a --update
chrom2csv: Updating bookmarks after 13427936095544805 (2026-07-08 01:14:55.544806)
chrom2csv: Sorting by a
Traceback (most recent call last):
  File "/home/pm/pm/scripts/chrom2csv", line 362, in <module>
    main()
    ~~~~^^
  File "/home/pm/pm/scripts/chrom2csv", line 358, in main
    args.func(args)
    ~~~~~~~~~^^^^^^
  File "/home/pm/pm/scripts/chrom2csv", line 307, in cmd_b
    bookmarks.sort(key=sort_key, reverse=reverse)
    ~~~~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
TypeError: '<' not supported between instances of 'int' and 'str'
```
For some reason in the new data the epochs are numbers instead of strings, even though they are strings in the underlying data.
```python
[{'id': '353', 'url': 'http://google.com/', 'name': 'test2', 'added epoch': 13427932619283451, 'added': '2026-07-08 00:16:59.283451', 'used epoch': 0, 'used': None, 'path': '/Other bookmarks', 'position': 0}, {'id': '355', 'url': 'http://example.com/', 'name': 'test1', 'added epoch': 13427936095544805, 'added': '2026-07-08 01:14:55.544806', 'used epoch': 0, 'used': None, 'path': '/Other bookmarks', 'position': 1}]
```
And is it correct that these two are updating? Date modified on "other" is 13427936214847645 which is 2026-07-08 01:16:54.847645 local time. That's after 2026-07-08 01:14:55.544806, so yes.

Ah, it's because we're doing:
```python
    a = int(d.get('date_added'))
    c = int(d.get('date_last_used'))

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(a) if a else None,
        'c': lambda: c,
        'C': lambda: readable_date_from_epoch(c) if c else None,
```
Need to keep them as strings and only pass as ints to `readable_date_from_epoch`:
```python
    a = d.get('date_added')
    c = d.get('date_last_used')

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(int(a)) if a else None,
        'c': lambda: c,
        'C': lambda: readable_date_from_epoch(int(c)) if c else None,
```
But now on the bookmarks that updated I get 1601-01-01 02:20:54.000000 as C (human-readable last used date) instead of None.
```python
    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(int(a)) if a and int(a) else '',
        'c': lambda: c,
        'C': lambda: readable_date_from_epoch(int(c)) if c and int(c) else '',
```
Changed it also to an empty string instead of None to save some space, and I guess it makes more sense since when the name of the bookmarks is empty for example it is also an empty string, so it's normal to have fields that are empty.

It seems to work now, but I have a doubt:
What if the last used date update on a bookmark, would its parent folder modification date change? If not, then update would result in stale last used dates.

```json
      "other": {
         "children": [ {
            "date_added": "13427936095544805",
            "date_last_used": "13427950743036573",
            "guid": "66f7c6b6-f3cd-450d-ad0e-714e46e7fddb",
            "id": "355",
            "meta_info": {
               "power_bookmark_meta": ""
            },
            "name": "test1",
            "type": "url",
            "url": "http://example.com/"
         } ],
         "date_added": "13382283568010060",
         "date_last_used": "0",
         "date_modified": "13427936214847645",
         "guid": "82b081ec-3dd3-529c-8475-ab6c344590dd",
         "id": "2",
         "name": "Other bookmarks",
         "type": "folder"
      },
```
13427950743036573 > 13427936214847645

There is indeed this problem :-/

I think I will just add "known issues" to the docstring with this issue, because otherwise I don't know how we would do update.

## History DB
Now let's tackle history. There is [this page](https://en.wikiversity.org/wiki/Chromium_browsing_history_database) I already linked that I encountered during my bookmarks researches that gives a good overview for the structure of the history sqlite database. There are two tables that interest us: `urls` and `visits`. The former seems like the equivalent of the table `moz_places` in places.sqlite, and the latter `moz_historyvisits`. I'm also curious to see what the other tables have.

```python
#!/usr/bin/env python3
import sqlite3
from os.path import expanduser

FILE = expanduser("~/.config/chromium/Default/History")

with sqlite3.connect(f"file:{FILE}?mode=ro", uri=True) as con:
    cur = con.cursor()
    res = cur.execute("SELECT name FROM sqlite_master")
    print(res.fetchall())
```

> sqlite3.OperationalError: database is locked

It seems like I have to quit Chromium to be able to access it. This is inconvenient.

I tried suggestions I saw [on SE](https://stackoverflow.com/questions/2740806/python-sqlite-database-is-locked) like doing it in a loop and passing `isolation_level=None` to no avail, it remains locked until I quit Chromium, and becomes locked again when I reopen it.
```python
while True:
    try:
        with sqlite3.connect(f"file:{FILE}?mode=ro", uri=True, isolation_level=None, timeout=1) as con:
            cur = con.cursor()
            res = cur.execute("SELECT name FROM sqlite_master")
            print(res.fetchall())
    except sqlite3.OperationalError:
        print("database locked")
```

It works with immutable, but [Medcalf's warnings of "shit hitting the oscillatory device"](/devlog/23-places-to-csv#reading-sqlite-without-modifying-the-file) are a little bit concerning.
```python
with sqlite3.connect(f"file:{FILE}?immutable=1", uri=True) as con:
    cur = con.cursor()
    res = cur.execute("SELECT name FROM sqlite_master")
    print(res.fetchall())
```
On the wikiversity page he uses `?mode=ro&nolock=1`, which also works.
```python
with sqlite3.connect(f"file:{FILE}?mode=ro&nolock=1", uri=True) as con:
    cur = con.cursor()
    res = cur.execute("SELECT name FROM sqlite_master")
    print(res.fetchall())
```
```python
[('meta',), ('sqlite_autoindex_meta_1',), ('urls',), ('sqlite_sequence',), ('visits',), ('visit_source',), ('visits_url_index',), ('visits_from_index',), ('visits_time_index',), ('visits_originator_id_index',), ('keyword_search_terms',), ('keyword_search_terms_index1',), ('keyword_search_terms_index2',), ('keyword_search_terms_index3',), ('downloads',), ('downloads_url_chains',), ('sqlite_autoindex_downloads_url_chains_1',), ('downloads_slices',), ('sqlite_autoindex_downloads_slices_1',), ('segments',), ('segments_name',), ('segments_url_id',), ('segment_usage',), ('segment_usage_time_slot_segment_id',), ('segments_usage_seg_id',), ('content_annotations',), ('context_annotations',), ('clusters',), ('clusters_and_visits',), ('clusters_for_visit',), ('cluster_keywords',), ('cluster_keywords_cluster_id_index',), ('cluster_visit_duplicates',), ('visited_links',), ('visited_links_index',), ('history_sync_metadata',), ('urls_url_index',)]
```

```python
def q(query):
    with sqlite3.connect(f"file:{FILE}?mode=ro&nolock=1", uri=True) as con:
        con.row_factory = sqlite3.Row
        cur = con.cursor()
        res = cur.execute(query)
    for s in res:
        print(dict(s))
```

`q('SELECT * FROM meta')`
```python
{'key': 'mmap_status', 'value': '-1'}
{'key': 'version', 'value': '70'}
{'key': 'last_compatible_version', 'value': '69'}
{'key': 'early_expiration_threshold', 'value': '13420473126729744'}
```

`urls` sample:
```python
{'id': 39721, 'url': 'http://example.com/', 'title': 'Exemple de domaine', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427950743036573, 'hidden': 0}
{'id': 39722, 'url': 'https://example.com/', 'title': 'Exemple de domaine', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427950743036573, 'hidden': 0}
{'id': 39723, 'url': 'https://iana.org/domains/example', 'title': 'Example Domains', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427987707190041, 'hidden': 0}
{'id': 39724, 'url': 'https://www.iana.org/domains/example', 'title': 'Example Domains', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427987707190041, 'hidden': 0}
{'id': 39725, 'url': 'http://www.iana.org/help/example-domains', 'title': 'Example Domains', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427987707190041, 'hidden': 0}
{'id': 39726, 'url': 'https://www.iana.org/help/example-domains', 'title': 'Example Domains', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427987707190041, 'hidden': 0}
```

`sqlite_sequence`:
```python
{'name': 'urls', 'seq': 39726}
{'name': 'visits', 'seq': 73777}
{'name': 'clusters', 'seq': 10508}
{'name': 'visited_links', 'seq': 32891}
```

`visits` sample:
```python
{'id': 73773, 'url': 39723, 'visit_time': 13427987707190041, 'from_visit': 73774, 'external_referrer_url': '', 'transition': 268435456, 'segment_id': 0, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 32888, 'app_id': ''}
{'id': 73774, 'url': 39724, 'visit_time': 13427987707190041, 'from_visit': 73773, 'external_referrer_url': '', 'transition': -2147483648, 'segment_id': 0, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 32889, 'app_id': ''}
{'id': 73775, 'url': 39725, 'visit_time': 13427987707190041, 'from_visit': 73774, 'external_referrer_url': '', 'transition': -2147483648, 'segment_id': 0, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 32890, 'app_id': ''}
{'id': 73776, 'url': 39726, 'visit_time': 13427987707190041, 'from_visit': 73775, 'external_referrer_url': '', 'transition': -1610612736, 'segment_id': 0, 'visit_duration': 66509548, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 32891, 'app_id': ''}
{'id': 73777, 'url': 9, 'visit_time': 13427990170973524, 'from_visit': 0, 'external_referrer_url': '', 'transition': 805306376, 'segment_id': 0, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 0, 'app_id': ''}
```

`visit_source`: empty

`keyword_search_terms` sample:
```python
{'keyword_id': 30, 'url_id': 28920, 'term': 'Citroën', 'normalized_term': 'citroën'}
{'keyword_id': 30, 'url_id': 28943, 'term': 'kangoo', 'normalized_term': 'kangoo'}
{'keyword_id': 71, 'url_id': 37760, 'term': 'linux', 'normalized_term': 'linux'}
{'keyword_id': 2, 'url_id': 38215, 'term': 'utc time', 'normalized_term': 'utc time'}
{'keyword_id': 2, 'url_id': 38216, 'term': 'utc time', 'normalized_term': 'utc time'}
```

`downloads` sample:
```python
{'id': 149, 'guid': 'e83064d5-f454-473b-9f30-e987415def9e', 'current_path': '/home/pm/Downloads/krita-5.3.0-prealpha-stripped-bundles-dk1.zip', 'target_path': '/home/pm/Downloads/krita-5.3.0-prealpha-stripped-bundles-dk1.zip', 'start_time': 13410358354054149, 'received_bytes': 200428996, 'total_bytes': 200428996, 'state': 1, 'danger_type': 0, 'interrupt_reason': 0, 'hash': b'', 'end_time': 13410358369741672, 'opened': 0, 'last_access_time': 0, 'transient': 0, 'referrer': 'https://disk.yandex.ru/', 'site_url': '', 'embedder_download_data': '\n\x08\n\x00\x12\x00\x18\x00 \x00', 'tab_url': 'https://disk.yandex.ru/d/7vnlhalifPPGBA', 'tab_referrer_url': 'https://krita-artists.org/', 'http_method': '', 'by_ext_id': '', 'by_ext_name': '', 'by_web_app_id': '', 'etag': '', 'last_modified': 'Wed, 10 Apr 2024 16:00:06 GMT', 'mime_type': 'application/zip', 'original_mime_type': 'application/zip'}
{'id': 150, 'guid': 'aca048d1-aef1-4572-81b0-c6de5b04bb03', 'current_path': '/home/pm/Downloads/deevad-bundle_25.01.zip', 'target_path': '/home/pm/Downloads/deevad-bundle_25.01.zip', 'start_time': 13410361434688689, 'received_bytes': 8509341, 'total_bytes': 8509341, 'state': 1, 'danger_type': 0, 'interrupt_reason': 0, 'hash': b'', 'end_time': 13410361456535877, 'opened': 0, 'last_access_time': 0, 'transient': 0, 'referrer': 'https://www.davidrevoy.com/', 'site_url': '', 'embedder_download_data': '\n\x08\n\x00\x12\x00\x18\x00 \x00', 'tab_url': 'https://www.davidrevoy.com/article1060/krita-brushes-2025-01-bundle', 'tab_referrer_url': 'https://www.davidrevoy.com/tag/brush', 'http_method': '', 'by_ext_id': '', 'by_ext_name': '', 'by_web_app_id': '', 'etag': '', 'last_modified': 'Sat, 11 Jan 2025 00:04:08 GMT', 'mime_type': 'application/zip', 'original_mime_type': 'application/zip'}
{'id': 151, 'guid': '334b59cb-8d97-4214-8d6b-e13e3dc88f9f', 'current_path': '/home/pm/Downloads/sword experiment.kra', 'target_path': '/home/pm/Downloads/sword experiment.kra', 'start_time': 13410658271071003, 'received_bytes': 1164067, 'total_bytes': 1164067, 'state': 1, 'danger_type': 0, 'interrupt_reason': 0, 'hash': b'', 'end_time': 13410658271649588, 'opened': 0, 'last_access_time': 0, 'transient': 0, 'referrer': '', 'site_url': '', 'embedder_download_data': '\n\x08\n\x00\x12\x00\x18\x00 \x00', 'tab_url': 'https://drive.google.com/drive/folders/1FN2g2nyaFi3y0kYz-A-G7vOnO2bvg-O6', 'tab_referrer_url': 'https://krita-artists.org/', 'http_method': '', 'by_ext_id': '', 'by_ext_name': '', 'by_web_app_id': '', 'etag': '', 'last_modified': 'Mon, 18 Sep 2023 19:29:21 GMT', 'mime_type': 'application/x-krita', 'original_mime_type': 'application/octet-stream'}
{'id': 152, 'guid': '9df2949a-1969-4a2f-90e8-ea95e1181b82', 'current_path': '/home/pm/Downloads/sword experiment.png', 'target_path': '/home/pm/Downloads/sword experiment.png', 'start_time': 13410658295960612, 'received_bytes': 203148, 'total_bytes': 203148, 'state': 1, 'danger_type': 0, 'interrupt_reason': 0, 'hash': b'', 'end_time': 13410658296085871, 'opened': 1, 'last_access_time': 13410658297985528, 'transient': 0, 'referrer': '', 'site_url': '', 'embedder_download_data': '\n\x08\n\x00\x12\x00\x18\x00 \x00', 'tab_url': 'https://drive.google.com/drive/folders/1FN2g2nyaFi3y0kYz-A-G7vOnO2bvg-O6', 'tab_referrer_url': 'https://krita-artists.org/', 'http_method': '', 'by_ext_id': '', 'by_ext_name': '', 'by_web_app_id': '', 'etag': '', 'last_modified': 'Mon, 18 Sep 2023 19:31:23 GMT', 'mime_type': 'image/png', 'original_mime_type': 'image/png'}
```

`downloads_url_chains` sample:
```python
{'id': 149, 'chain_index': 0, 'url': 'https://downloader.disk.yandex.ru/disk/1c8b3d2aa1aa542ad8b9f111d6ae9d507607cdb75f483fbea55874521baf5914/69417b90/iKGfxzWFLFwxhdS9uTE-y5wjTDdVOy8JLl5rmtcg9qzfwkLFodFNRWrwX2fVYXHhrKgG7bLuGIz2t0IVeg1r3g%3D%3D?uid=0&filename=krita-5.3.0-prealpha-stripped-bundles-dk1.zip&disposition=attachment&hash=mW8yXjX89ylA1yRHhG%2BetdQPrr32Q6YaAohf1c5FhSqqzCAr2ZyuEjE1OK00/WQXq/J6bpmRyOJonT3VoXnDag%3D%3D&limit=0&content_type=application%2Fzip&owner_uid=3735071&fsize=200428996&hid=c7f73fa2a8648f51e2da59e226dba11d&media_type=compressed&tknv=v3'}
{'id': 149, 'chain_index': 1, 'url': 'https://s742sas.storage.yandex.net/rdisk/1c8b3d2aa1aa542ad8b9f111d6ae9d507607cdb75f483fbea55874521baf5914/69417b90/iKGfxzWFLFwxhdS9uTE-y5wjTDdVOy8JLl5rmtcg9qzfwkLFodFNRWrwX2fVYXHhrKgG7bLuGIz2t0IVeg1r3g==?uid=0&filename=krita-5.3.0-prealpha-stripped-bundles-dk1.zip&disposition=attachment&hash=mW8yXjX89ylA1yRHhG%2BetdQPrr32Q6YaAohf1c5FhSqqzCAr2ZyuEjE1OK00/WQXq/J6bpmRyOJonT3VoXnDag%3D%3D&limit=0&content_type=application%2Fzip&owner_uid=3735071&fsize=200428996&hid=c7f73fa2a8648f51e2da59e226dba11d&media_type=compressed&tknv=v3&ts=646136faa0400&s=a9116e097ebd2a84e3ade1a8d8e12ffc455cef675e6cba9043e9601dc9efe3e7&pb=U2FsdGVkX1_gmO3gpoS38eP5xLK_5xDTJjCJzebJFtZTxxOnfAgz-9dhRMpCkNch4fqE7P2IqutH40Rq85Hl4BNmce6NulDDC8DcI2c95iw'}
{'id': 150, 'chain_index': 0, 'url': 'https://www.peppercarrot.com/extras/resources/deevad-bundle_25.01.zip'}
{'id': 151, 'chain_index': 0, 'url': 'https://drive.usercontent.google.com/download?id=1Y9DyjwbJjzw2OSUnqRt--Qtx5NoKvCZf&export=download&authuser=0&confirm=t&uuid=bff94fe6-e0d2-484b-b704-b5d039afae2d&at=ANTm3czMp-QXJtjhdjmqHHH9zbiT:1766184668361'}
{'id': 152, 'chain_index': 0, 'url': 'https://drive.usercontent.google.com/download?id=1Igvlap3ZLt4pHFGan_nypQg3YcfVCJ9P&export=download&authuser=0&confirm=t&uuid=db38e4bd-63d6-4006-ac16-7af4ee179391&at=ANTm3cw0RpWFS9Vcslum-NYE_Z5t:1766184694224'}
{'id': 153, 'chain_index': 0, 'url': 'https://drive.usercontent.google.com/download?id=0B4v3_wNJZZlNWlFCSWlwS1laWHM&export=download&authuser=0&confirm=t&uuid=76bf8a38-0b20-4431-9d4d-f534ebdf839c&at=ANTm3cwYkKgmtoANhzXlCsv0IuBG%3A1766185403582'}
{'id': 154, 'chain_index': 0, 'url': 'https://drive.usercontent.google.com/u/0/uc?id=19J_ChVsSdOof-S5nQ9WQdm1J4qBdHxDi&export=download'}
{'id': 154, 'chain_index': 1, 'url': 'https://drive.usercontent.google.com/download?id=19J_ChVsSdOof-S5nQ9WQdm1J4qBdHxDi&export=download&authuser=0'}
```

`downloads_slices`: empty

`segments` sample:
```python
{'id': 712, 'name': 'http://store.epicgames.com/p/i-have-no-mouth-and-i-must-scream-95c5c2', 'url_id': 39670}
{'id': 713, 'name': 'http://store.epicgames.com/p/river-city-girls-2-77af3a', 'url_id': 39671}
{'id': 714, 'name': 'http://store.steampowered.com/app/2830090/Sixth_Force/', 'url_id': 39708}
{'id': 715, 'name': 'http://store.steampowered.com/app/3496980/Greenhouse_Schism/', 'url_id': 39711}
{'id': 716, 'name': 'http://sv.wikipedia.org/wiki/Marcus_Fjellstr%C3%B6m', 'url_id': 39713}
```

`segments_name`: no such table

`segments_url_id`: no such table

`segment_usage`: no such table

`content_annotations` sample:
```python
{'visit_id': 73696, 'visibility_score': -1, 'floc_protected_score': None, 'categories': '', 'page_topics_model_version': -1, 'annotation_flags': 0, 'entities': '', 'related_searches': '', 'search_normalized_url': '', 'search_terms': '', 'alternative_title': '', 'page_language': '', 'password_state': 1, 'has_url_keyed_image': 0}
{'visit_id': 73698, 'visibility_score': -1, 'floc_protected_score': None, 'categories': '', 'page_topics_model_version': -1, 'annotation_flags': 0, 'entities': '', 'related_searches': '', 'search_normalized_url': '', 'search_terms': '', 'alternative_title': '', 'page_language': 'en', 'password_state': 0, 'has_url_keyed_image': 0}
{'visit_id': 73699, 'visibility_score': -1, 'floc_protected_score': None, 'categories': '', 'page_topics_model_version': -1, 'annotation_flags': 0, 'entities': '', 'related_searches': '', 'search_normalized_url': '', 'search_terms': '', 'alternative_title': '', 'page_language': 'en', 'password_state': 0, 'has_url_keyed_image': 0}
{'visit_id': 73700, 'visibility_score': -1, 'floc_protected_score': None, 'categories': '', 'page_topics_model_version': -1, 'annotation_flags': 0, 'entities': '', 'related_searches': '', 'search_normalized_url': '', 'search_terms': '', 'alternative_title': '', 'page_language': 'en', 'password_state': 0, 'has_url_keyed_image': 0}
{'visit_id': 73701, 'visibility_score': -1, 'floc_protected_score': None, 'categories': '', 'page_topics_model_version': -1, 'annotation_flags': 0, 'entities': '', 'related_searches': '', 'search_normalized_url': '', 'search_terms': '', 'alternative_title': '', 'page_language': 'sv', 'password_state': 0, 'has_url_keyed_image': 0}
```
Almost all of them are blank like that. and the rare ones that aren't seem to be searches, not just google but also duckduckgo or even some sites that aren't search engines.
```python
{'visit_id': 68307, 'visibility_score': -1, 'floc_protected_score': None, 'categories': '', 'page_topics_model_version': -1, 'annotation_flags': 0, 'entities': '', 'related_searches': '', 'search_normalized_url': 'https://www.google.com/search?q=utc+time&sourceid=chrome&ie=UTF-8', 'search_terms': 'utc time', 'alternative_title': '', 'page_language': '', 'password_state': 0, 'has_url_keyed_image': 0}
```

`context_annotations` sample:
```python
{'visit_id': 73771, 'context_annotation_flags': 40, 'duration_since_last_visit': 102156562, 'page_end_reason': 1, 'total_foreground_duration': 35734376, 'browser_type': 1, 'window_id': 1656701027, 'tab_id': 1656701036, 'task_id': 13427985951096594, 'root_task_id': 13427985951096594, 'parent_task_id': -1, 'response_code': 200}
{'visit_id': 73772, 'context_annotation_flags': 0, 'duration_since_last_visit': -1000000, 'page_end_reason': 0, 'total_foreground_duration': -1000000, 'browser_type': 1, 'window_id': 1656701027, 'tab_id': 1656701028, 'task_id': 13426616459078312, 'root_task_id': 13426616459078312, 'parent_task_id': -1, 'response_code': 200}
{'visit_id': 73774, 'context_annotation_flags': 0, 'duration_since_last_visit': 35168912075, 'page_end_reason': 4, 'total_foreground_duration': 27200310, 'browser_type': 0, 'window_id': -1, 'tab_id': -1, 'task_id': -1, 'root_task_id': -1, 'parent_task_id': -1, 'response_code': 0}
{'visit_id': 73776, 'context_annotation_flags': 0, 'duration_since_last_visit': -1000000, 'page_end_reason': 6, 'total_foreground_duration': 67595439, 'browser_type': 1, 'window_id': 1656701027, 'tab_id': 1656701035, 'task_id': 13427987707190041, 'root_task_id': 13427950743036573, 'parent_task_id': 13427950743036573, 'response_code': 200}
{'visit_id': 73777, 'context_annotation_flags': 0, 'duration_since_last_visit': -1000000, 'page_end_reason': 0, 'total_foreground_duration': -1000000, 'browser_type': 1, 'window_id': 1656701027, 'tab_id': 1656701036, 'task_id': 13427985951096594, 'root_task_id': 13427985951096594, 'parent_task_id': -1, 'response_code': 200}
```

`clusters` sample:
```python
{'cluster_id': 10504, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 10505, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 10506, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 10507, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 10508, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
```
They all seem to be empty like that, apart from 1-95. sample:
```python
{'cluster_id': 49, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'auctions.c.yimg.jp', 'raw_label': 'auctions.c.yimg.jp', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 51, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'i.ebayimg.com', 'raw_label': 'i.ebayimg.com', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 53, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'i.ebayimg.com', 'raw_label': 'i.ebayimg.com', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 55, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'i.ebayimg.com', 'raw_label': 'i.ebayimg.com', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 57, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'i.ebayimg.com', 'raw_label': 'i.ebayimg.com', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 59, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'i.ebayimg.com', 'raw_label': 'i.ebayimg.com', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 61, 'should_show_on_prominent_ui_surfaces': 0, 'label': "'bag duffel'", 'raw_label': 'bag duffel', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
```
and it also jumps to 8915 at the end of the first 95:
```python
{'cluster_id': 93, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'twitch.tv', 'raw_label': 'twitch.tv', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 95, 'should_show_on_prominent_ui_surfaces': 0, 'label': 'eu.alienwarearena.com', 'raw_label': 'eu.alienwarearena.com', 'triggerability_calculated': 1, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 8915, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
{'cluster_id': 8916, 'should_show_on_prominent_ui_surfaces': 0, 'label': '', 'raw_label': '', 'triggerability_calculated': 0, 'originator_cache_guid': '', 'originator_cluster_id': 0}
```

`clusters_and_visits`:
```python
{'cluster_id': 10487, 'visit_id': 73701, 'score': 1, 'engagement_score': 3, 'url_for_deduping': 'https://sv.wikipedia.org/', 'normalized_url': 'https://sv.wikipedia.org/wiki/Marcus_Fjellstr%C3%B6m', 'url_for_display': 'sv.wikipedia.org/wiki/Marcus_Fjellström', 'interaction_state': 0}
{'cluster_id': 10487, 'visit_id': 73702, 'score': 1, 'engagement_score': 3, 'url_for_deduping': 'https://sv.wikipedia.org/', 'normalized_url': 'https://sv.wikipedia.org/wiki/Rubus_arcticus_(stipendium)', 'url_for_display': 'sv.wikipedia.org/wiki/Rubus_arcticus_(stipendium)', 'interaction_state': 0}
{'cluster_id': 10487, 'visit_id': 73703, 'score': 1, 'engagement_score': 0, 'url_for_deduping': 'https://en.wikipedia.org/', 'normalized_url': 'https://en.wikipedia.org/wiki/Rubus_Arcticus_(grant)', 'url_for_display': 'en.wikipedia.org/wiki/Rubus_Arcticus_(grant)', 'interaction_state': 0}
{'cluster_id': 10488, 'visit_id': 73707, 'score': 1, 'engagement_score': 5.888270378112793, 'url_for_deduping': 'https://dvdhrm.wordpress.com/', 'normalized_url': 'https://dvdhrm.wordpress.com/2013/08/25/sane-session-switching/', 'url_for_display': 'dvdhrm.wordpress.com/2013/08/25/sane-session-switching/', 'interaction_state': 0}
{'cluster_id': 10489, 'visit_id': 73708, 'score': 1, 'engagement_score': 5.888270378112793, 'url_for_deduping': 'https://dvdhrm.wordpress.com/', 'normalized_url': 'https://dvdhrm.wordpress.com/2013/07/08/thoughts-on-linux-system-compositors/', 'url_for_display': 'dvdhrm.wordpress.com/2013/07/08/thoughts-on-linux-system-compositors/', 'interaction_state': 0}
```
The engagement score seems to be by domain, not by page. I see it increasing over time, and it can be a float like 99.9000015258789, then after it reaches 100 that seems to be the limit. 0 being the lower limit.
```python
{'cluster_id': 10509, 'visit_id': 73776, 'score': 1, 'engagement_score': 0, 'url_for_deduping': 'https://iana.org/', 'normalized_url': 'https://www.iana.org/help/example-domains', 'url_for_display': 'iana.org/help/example-domains', 'interaction_state': 0}
{'cluster_id': 10509, 'visit_id': 73778, 'score': 1, 'engagement_score': 100, 'url_for_deduping': 'https://eu.alienwarearena.com/', 'normalized_url': 'https://eu.alienwarearena.com/control-center', 'url_for_display': 'eu.alienwarearena.com/control-center', 'interaction_state': 0}
```

`clusters_for_visit`: no such table

`cluster_keywords`: there is oddly just this
```python
{'cluster_id': 63, 'keyword': 'backpack duffel', 'type': 4, 'score': 100, 'collections': ''}
{'cluster_id': 61, 'keyword': 'bag duffel', 'type': 4, 'score': 100, 'collections': ''}
{'cluster_id': 46, 'keyword': 'kaka bag', 'type': 4, 'score': 100, 'collections': ''}
```
which are searches from a long time ago, well over a year.

`cluster_visit_duplicates`: empty

`visited_links` sample:
```python
{'id': 32887, 'link_url_id': 39665, 'top_level_url': 'https://eu.alienwarearena.com/control-center', 'frame_url': 'https://eu.alienwarearena.com/', 'visit_count': 2}
{'id': 32888, 'link_url_id': 39723, 'top_level_url': 'https://example.com/', 'frame_url': 'https://example.com/', 'visit_count': 1}
{'id': 32889, 'link_url_id': 39724, 'top_level_url': 'https://example.com/', 'frame_url': 'https://example.com/', 'visit_count': 1}
{'id': 32890, 'link_url_id': 39725, 'top_level_url': 'https://example.com/', 'frame_url': 'https://example.com/', 'visit_count': 1}
{'id': 32891, 'link_url_id': 39726, 'top_level_url': 'https://example.com/', 'frame_url': 'https://example.com/', 'visit_count': 1}
```
The visit count is a bit odd.

`history_sync_metadata`: empty.

Apropos of nothing, I remember from the years I was on Windows, Chrome would sometimes take a tonne of CPU while they were uploading their telemetry.

## History list visits
It's easy to get lost in all the data. Let's just try to get a list of URLs visited.

Wonder if I can just copy the query from the wikiversity page.
```python
q("SELECT datetime(visits.visit_time/1000000-11644473600,'unixepoch') as VisitTime, \
           urls.url, \
           urls.title \
      FROM urls, visits \
     WHERE urls.id = visits.url \
  ORDER BY visits.visit_time DESC")
```
Result sample:
```python
{'VisitTime': '2026-04-09 19:30:11', 'url': 'https://eu.alienwarearena.com/control-center', 'title': 'Control Center | Alienware Arena'}
{'VisitTime': '2026-04-09 16:50:53', 'url': 'https://lists.freedesktop.org/archives/systemd-devel/2013-August/012906.html', 'title': '[systemd-devel] [PATCH 09/10] logind: extract has_vts() from can_multi_session()'}
{'VisitTime': '2026-04-09 16:50:52', 'url': 'file:///home/pm/pm/logs/browser-note', 'title': 'browser-note'}
{'VisitTime': '2026-04-09 16:50:50', 'url': 'https://github.com/plu5', 'title': 'plu5 (±) · GitHub'}
```
The order is reverse chronological, so this is the oldest history, 90 days ago.

It's nice, but I want to get all the information from sqlite and deal with it in Python.
```python
q("SELECT * FROM urls, visits WHERE urls.id = visits.url")
```
Example:
```python
{'id': 9, 'url': 'https://eu.alienwarearena.com/control-center', 'title': 'Control Center | Alienware Arena', 'visit_count': 539, 'typed_count': 3, 'last_visit_time': 13427994723357320, 'hidden': 0, 'visit_time': 13427994723357320, 'from_visit': 0, 'external_referrer_url': '', 'transition': 805306370, 'segment_id': 6, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 0, 'app_id': ''}
```

`urls` entry:
```python
{'id': 39723, 'url': 'https://iana.org/domains/example', 'title': 'Example Domains', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427987707190041, 'hidden': 0}
```
`visits` entry:
```python
{'id': 73773, 'url': 39723, 'visit_time': 13427987707190041, 'from_visit': 73774, 'external_referrer_url': '', 'transition': 268435456, 'segment_id': 0, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 32888, 'app_id': ''}
```

For comparison, the fields we have in places.csv history are visit date, id, fk (place id), url, title, description.

Here we could have id, url, title, visit count, typed count, visit date. Not sure what transition is (and it's sometimes negative).

[[dfir.blog: Chrome Transition Values](https://dfir.blog/chrome-transition-values/): "The short explanation is to take the stored transition value, AND it with 0xFF, and look the resulting value up in a table."]

I'm also interested in the engagement score for `clusters_and_visits`, but I'm a bit confused about the entries in that table.
```python
>>> q("SELECT * FROM clusters_and_visits WHERE cluster_id = 10509")
{'cluster_id': 10509, 'visit_id': 73776, 'score': 1, 'engagement_score': 0, 'url_for_deduping': 'https://iana.org/', 'normalized_url': 'https://www.iana.org/help/example-domains', 'url_for_display': 'iana.org/help/example-domains', 'interaction_state': 0}
{'cluster_id': 10509, 'visit_id': 73778, 'score': 1, 'engagement_score': 100, 'url_for_deduping': 'https://eu.alienwarearena.com/', 'normalized_url': 'https://eu.alienwarearena.com/control-center', 'url_for_display': 'eu.alienwarearena.com/control-center', 'interaction_state': 0}
{'cluster_id': 10509, 'visit_id': 73779, 'score': 1, 'engagement_score': 100, 'url_for_deduping': 'https://eu.alienwarearena.com/', 'normalized_url': 'https://eu.alienwarearena.com/control-center', 'url_for_display': 'eu.alienwarearena.com/control-center', 'interaction_state': 0}
{'cluster_id': 10509, 'visit_id': 73780, 'score': 1, 'engagement_score': 100, 'url_for_deduping': 'https://eu.alienwarearena.com/', 'normalized_url': 'https://eu.alienwarearena.com/account/arp-log', 'url_for_display': 'eu.alienwarearena.com/account/arp-log', 'interaction_state': 0}
{'cluster_id': 10509, 'visit_id': 73784, 'score': 1, 'engagement_score': 100, 'url_for_deduping': 'https://eu.alienwarearena.com/', 'normalized_url': 'https://eu.alienwarearena.com/control-center', 'url_for_display': 'eu.alienwarearena.com/control-center', 'interaction_state': 0}
```

And there is not every visit in there, for example no results for `q("SELECT * FROM clusters_and_visits WHERE visit_id = 73773")`. Maybe should instead join them on url. But no, because the same url can appear multiple times with different scores as time goes on.

I think what I want is to do a join where if the visit ID isn't in clusters, then have it be None or empty, don't exclude that visit.

If we do:
```python
q("SELECT * FROM urls, visits, clusters_and_visits WHERE urls.id = visits.url AND clusters_and_visits.visit_id = visits.id")
```
then it does exclude.

> To include all records from all tables, whether they match or not, you can use a FULL OUTER JOIN.  
—[*GeeksforGeeks: Joining Three or More Tables in SQL*](https://www.geeksforgeeks.org/sql/joining-three-tables-sql/)

Or LEFT OUTER JOIN to include all records from the first table and not the second, and RIGHT is vice versa.

The syntax of join is like `SELECT * FROM table1 alias1 JOIN table2 alias2 ON alias1.id = alias2.id`

Attempt:
```python
q("SELECT * FROM visits v LEFT OUTER JOIN urls u ON v.url = u.id LEFT OUTER JOIN clusters_and_visits c ON v.id = c.visit_id")
```

73773 appears like:
```python
{'id': 73773, 'url': 39723, 'visit_time': 13427987707190041, 'from_visit': 73774, 'external_referrer_url': '', 'transition': 268435456, 'segment_id': 0, 'visit_duration': 0, 'incremented_omnibox_typed_score': 0, 'opener_visit': 0, 'originator_cache_guid': '', 'originator_visit_id': 0, 'originator_from_visit': 0, 'originator_opener_visit': 0, 'is_known_to_sync': 0, 'consider_for_ntp_most_visited': 1, 'visited_link_id': 32888, 'app_id': '', 'title': 'Example Domains', 'visit_count': 1, 'typed_count': 0, 'last_visit_time': 13427987707190041, 'hidden': 0, 'cluster_id': None, 'visit_id': None, 'score': None, 'engagement_score': None, 'url_for_deduping': None, 'normalized_url': None, 'url_for_display': None, 'interaction_state': None}
```
We're missing the actual url. I guess that's why in the wikiversity query he puts the urls table first. But I do want to keep the url id too to use as a field, like fk in places2csv.

> You can use the 'AS' keyword to name a column.  
—[*Kevin Gosse, SE, 2011*](https://stackoverflow.com/a/8244783/18396947)

And I saw on [snook.ca](https://snook.ca/archives/sql/selecting_datab) that you don't even need AS, you can just put the alias after the name of the column like with aliasing tables.

Attempt:
```python
q("SELECT v.id, v.url fk, u.url, u.title, v.visit_time, v.from_visit, v.transition, v.visit_duration, c.engagement_score FROM visits v LEFT OUTER JOIN urls u ON v.url = u.id LEFT OUTER JOIN clusters_and_visits c ON v.id = c.visit_id")
```
Example:
```python
{'id': 73784, 'fk': 9, 'url': 'https://eu.alienwarearena.com/control-center', 'title': 'Control Center | Alienware Arena', 'visit_time': 13427994723357320, 'from_visit': 0, 'transition': 805306370, 'visit_duration': 0, 'engagement_score': 100}
```

## h command
```python
#!/usr/bin/env python3
# chrom2csv
# 2026-07-06 19:19
"""Extracts selected information from Chromium bookmarks and history

Usage:
Bookmarks
    chrom2csv b ~/.config/chromium/Default/Bookmarks -st o.csv --sort a
History
    chrom2csv h ~/.config/chromium/Default/History -st o.csv --sort v

Known issues:
* Stale used dates if using --update and the containing folder hasn't
  changed. This is because only folders have modification dates and
  update uses them to decide which folders to enter, but these
  dates do not change when only use dates change, only if the
  name or url of a bookmark inside the folder changed.
"""
import os
import sys
import csv
import json
import sqlite3
import argparse
from datetime import datetime

PROG = 'chrom2csv'
DEFAULTSAVENAME = 'chrome.csv'
DATEFORMAT = '%F %T.%f'
QUIET = False

BOOKMARK_ROOTS = 'roots'
TOPLEVEL_BOOKMARK_FOLDERS = ["bookmark_bar", "other", "synced"]

BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch', 'C': 'used', 'c': 'used epoch',
    'i': 'id', 'u': 'url', 'n': 'name', 'g': 'guid',
    'p': 'path', 'o': 'position',
}
DEFAULTBFORMATOPTIONS = 'iunaAcCpo'

HFORMATOPTIONS = {
    'V': 'visit', 'v': 'visit epoch',
    'i': 'id', 'u': 'url', 't': 'title',
    'r': 'from', 'n': 'transition', 'd': 'duration', 's': 'engagement score',
    'f': 'fk',
}
DEFAULTHFORMATOPTIONS = 'ifutvVds'
INTFIELDS = ['visit epoch', 'id', 'from', 'transition', 'duration',
             'engagement score', 'fk',]

S_UNDESIREDOVERWRITE = """
Exiting to avoid undesired overwriting.
Run with --overwrite if you wish to overwrite.
"""

# Args that we need in several commands but can't make top-level due
# to argument-order issues
REPEATEDARGS = [
    # Maybe there should be a different defaultsavename for each command
    (["-s", "--save"], {"action": "store_true", "help": f"""Save output to
file instead of printing to stdout. Will save to ./{DEFAULTSAVENAME} unless a
different path is provided with the -t/--save-to option."""}),
    (["-t", "--save-to"], {"help": """Path to save output to.
Ignored if -s/--save is not provided."""}),
    (["-o", "--overwrite"], {"action": "store_true", "help": """Overwrite
save file if it already exists. Ignored if -s/--save is not provided."""}),
    (["-u", "--update"], {"action": "store_true", "help": """Update save
file if it already exists. Ignored if -s/--save is not provided.
If both -o/--overwrite and -u/--update are provided,
overwrite will take precedence."""}),
]

COMMANDS = {
    "":                         # TOP LEVEL ARGS
    {"args": [
        (["--dryrun"], {"action": "store_true", "help": """
Don't write to disk."""}),
        (["-q", "--quiet"], {"action": "store_true", "help": """
Suppress messages in output."""}),
    ]},
    "b":                        # B (bookmarks)
    {"desc": "extract bookmarks data", "usage": """[-f FORMAT] [--sort SORT]
[-s] [-t SAVE_TO] [-o] [-u]
file""",
     "args": [
         (["file"], {"help": """Bookmarks file.
e.g. ~/.config/chromium/Default/Bookmarks"""}),
         (["-f", "--format"], {"default": DEFAULTBFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTBFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in BFORMATOPTIONS.items()])}."""}),
         (["--sort"], {"choices":
                       [x for k in BFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: ir -> sort by id, reverse order.)"""}),
         *REPEATEDARGS,
     ]},
    "h":                        # H (history)
    {"desc": "extract history data", "usage": """[-f FORMAT] [--sort SORT]
[-s] [-t SAVE_TO] [-o] [-u]
file""",
     "args": [
         (["file"], {"help": """History file.
e.g. ~/.config/chromium/Default/History"""}),
         (["-f", "--format"], {"default": DEFAULTHFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTHFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in HFORMATOPTIONS.items()])}."""}),
         (["--sort"], {"choices":
                       [x for k in HFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: ir -> sort by id, reverse order.)"""}),
         *REPEATEDARGS,
     ]},
}


def msg(text):
    # type: (str) -> None
    """Utility for logs/errors, could be replaced with proper logging"""
    if not QUIET:
        print(f'{PROG}: {text}')


def readable_date_from_epoch(e):
    """Takes epoch in microseconds since 1 January 1601 00:00 UTC"""
    # type: (int) -> str
    return datetime.fromtimestamp(e/1000000-11644473600).strftime(DATEFORMAT)


def epoch_from_readable_date(d):
    """Returns epoch in microseconds since 1 January 1601 00:00 UTC"""
    # type: (str) -> int
    return int(datetime.fromisoformat(d).timestamp()*1000000)+11644473600


def get_bookmark_information(d, fmt, path, position):
    res = {}

    a = d.get('date_added')
    c = d.get('date_last_used')

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(int(a)) if a and int(a) else '',
        'c': lambda: c,
        'C': lambda: readable_date_from_epoch(int(c)) if c and int(c) else '',
        'i': lambda: d.get('id'),
        'u': lambda: d.get('url'),
        'n': lambda: d.get('name'),
        'g': lambda: d.get('guid'),
        'p': lambda: path,
        'o': lambda: position,
    }

    for char in fmt:
        opt = BFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res


def get_bookmarks_folder_generator(d, fmt, update_after=None, path=''):
    if d.get('type') == 'folder':
        if update_after and d.get('date_modified', 0) < update_after:
            return
        position = 0
        path = f'{path}/{d.get('name')}'
        for c in d.get('children', []):
            if c.get('type') == 'folder':
                yield from get_bookmarks_folder_generator(c, fmt, path)
            elif c.get('type') == 'url':
                yield get_bookmark_information(c, fmt, path, position)
                position += 1


def get_bookmarks_toplevel_generator(d, fmt, update_after=None):
    for g in [get_bookmarks_folder_generator(d.get(f, {}), fmt, update_after)
              for f in TOPLEVEL_BOOKMARK_FOLDERS]:
        yield from g


def get_bookmarks(d, fmt, update_after=None):
    return list(get_bookmarks_toplevel_generator(
        d[BOOKMARK_ROOTS], fmt, update_after))


def get_visit_information(d, fmt):
    res = {}

    v = d.get('visit_time')

    dispatch = {
        'v': lambda: v,
        'V': lambda: readable_date_from_epoch(int(v)) if v and int(v) else '',
        'i': lambda: d.get('id'),
        'u': lambda: d.get('url'),
        't': lambda: d.get('title'),
        'r': lambda: d.get('from_visit'),
        'n': lambda: d.get('transition'),
        'd': lambda: d.get('visit_duration'),
        's': lambda: d.get('engagement_score'),
        'f': lambda: d.get('fk'),
    }

    for char in fmt:
        opt = HFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res


def parse_csv(s):
    # type: str -> list
    res = []
    rows = list(csv.DictReader(s.splitlines()))
    keys = list(rows[0].keys())
    for row in rows:
        r = {}
        for field in keys:
            r[field] = row[field]
        res.append(r)
    return res


def read_existing(path):
    # type: str -> dict
    existing = None
    with open(path, "r") as f:
        existing = parse_csv(f.read())
    return existing


def bhasfields(d, fmt, require_all=True):
    # type: (dict, str, bool) -> bool
    f = all if require_all else any
    return f(BFORMATOPTIONS.get(c) in d for c in fmt)


def bmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    """Create dictionary from existing and new with only the fields in fmt
    and no duplicate IDs/GUIDs
    """
    if not existing:
        return new
    i = 'id' if bhasfields(existing[0], 'i') else 'guid'
    fields = [BFORMATOPTIONS[c] for c in fmt]
    by_id = {d[i]: {k: d[k] for k in fields} for d in existing}
    for d in new:
        by_id[d[i]] = d
    return list(by_id.values())


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


def export_csv(a):
    # type: (list) -> str
    fieldnames = [*list((a[0] if len(a) else {}).keys())]
    rows = [fieldnames]
    for item in a:
        rows.append([str(v) for v in item.values()])

    out = CsvOutput()
    writer = csv.writer(out)
    writer.writerows(rows)

    return str(out)


def cmd_b(args):
    save_to = args.save_to or DEFAULTSAVENAME

    existing = None
    update_after = None

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(save_to):
            if not args.update:
                msg(f'Save file \'{save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

            existing = read_existing(save_to)
            if not len(existing):
                msg('Existing is empty. Either the file is empty or something '
                    'went wrong.' + S_UNDESIREDOVERWRITE)
                exit(1)
            one = existing[0]
            # Check we have at least one of the date fields
            if not bhasfields(one, 'aAcC', require_all=False):
                msg('Existing data has no date fields. Without dates '
                    'in the data there is no reference point, so everything '
                    f'would have to be queried from scratch. {one}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check we have IDs or GUIDs
            if not bhasfields(one, 'ig', require_all=False):
                msg('Existing data has no IDs or GUIDs. Without identifiers '
                    'in the data there is no way to know which bookmarks were '
                    'modified.' + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not bhasfields(one, args.format, require_all=True):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {args.format}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent date in it
                a = c = 0
                if bhasfields(one, 'a', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['a']])
                    a = existing[-1].get(BFORMATOPTIONS['a'])
                elif bhasfields(one, 'A', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['A']])
                    A = existing[-1].get(BFORMATOPTIONS['A'])
                    a = epoch_from_readable_date(A)
                if bhasfields(one, 'c', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['c']])
                    c = existing[-1].get(BFORMATOPTIONS['c'])
                elif bhasfields(one, 'C', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['C']])
                    C = existing[-1].get(BFORMATOPTIONS['C'])
                    c = epoch_from_readable_date(C)
                update_after = a if a > c else c
                msg('Updating bookmarks after '
                    f'{update_after} '
                    f'({readable_date_from_epoch(int(update_after))})')

    with open(args.file, 'r') as f:
        data = json.load(f)

    bookmarks = get_bookmarks(data, args.format, update_after)

    if update_after and not bookmarks:
        msg("No new data")

    bookmarks = bmerge(
        existing, bookmarks, args.format) if update_after else bookmarks

    def sort_key(item):
        k = BFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = ''
        return data_to_sort_by

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        msg(f'Sorting by {args.sort[0]}{" in reverse" if reverse else ""}')
        bookmarks.sort(key=sort_key, reverse=reverse)

    dump = export_csv(bookmarks)

    if args.save and not args.dryrun:
        open_as = 'w' if args.overwrite or args.update else 'x'
        with open(save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {save_to}')
    else:
        print(dump)


def cmd_h(args):
    if args.update:
        raise NotImplementedError()

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(args.save_to):
            if not args.update:
                msg(f'Save file \'{args.save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

    with sqlite3.connect(
            f"file:{args.file}?mode=ro&nolock=1", uri=True) as con:
        con.row_factory = sqlite3.Row
        con.text_factory = lambda data: str(data, errors="surrogateescape")
        cur = con.cursor()
        res = cur.execute("""SELECT v.id, v.url fk, u.url, u.title,
        v.visit_time, v.from_visit, v.transition, v.visit_duration,
        c.engagement_score FROM visits v
        LEFT OUTER JOIN urls u ON v.url = u.id
        LEFT OUTER JOIN clusters_and_visits c ON v.id = c.visit_id""")

    visits = [get_visit_information(dict(s), args.format) for s in res]

    def sort_key(item):
        k = HFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = 0 if k in INTFIELDS else ''
        return data_to_sort_by

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        msg(f'Sorting by {args.sort[0]}{" in reverse" if reverse else ""}')
        visits.sort(key=sort_key, reverse=reverse)

    dump = export_csv(visits)

    if args.save and not args.dryrun:
        open_as = 'w' if args.overwrite or args.update else 'x'
        with open(args.save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {args.save_to}')
    else:
        print(dump)


def parse_args():
    parser = argparse.ArgumentParser(prog=PROG)
    parser.suggest_on_error = True
    subparsers = parser.add_subparsers(dest="command", required=True)
    descs = []
    for k, v in COMMANDS.items():
        if v.get('desc'):       # description generation
            descs.append(f"{v['desc']} ({k})")
        if k == "":             # arguments on top parser
            for args, kwargs in v['args']:
                parser.add_argument(*args, **kwargs)
            continue
        subparser = subparsers.add_parser(
            k, help=f"[-h|--help] {v.get('usage')}")
        for args, kwargs in v['args']:
            subparser.add_argument(*args, **kwargs)
        # Set handler
        subparser.set_defaults(func=v.get('func'))
    # Add generated description
    if len(descs) > 1:
        descs[-1] = f"or {descs[-1]}"
    parser.description = f"A utility script to \
{' '.join(descs)} from Chromium files."
    # Parse args / print help and quit if no args
    # (Primer https://stackoverflow.com/a/47440202/18396947)
    return parser.parse_args(sys.argv[1:] or ['--help'])


COMMANDS['b']['func'] = cmd_b
COMMANDS['h']['func'] = cmd_h


def main():
    # type: () -> None
    global QUIET

    args = parse_args()
    QUIET = args.quiet

    args.func(args)


if __name__ == '__main__':
    main()
```

It was easy because sqlite does most of the work, which I didn't take advantage of in places2csv, and now makes me think I could rewrite it better. But it doesn't feel right for several reasons:
- The query is the same despite args.format. There's no need to join `clusters_and_visits` for example if s is not in args.format.
- There's no point going through `get_visit_information` other than for the readable date, but we're seen earlier sqlite is capable of doing that.

For the second point, it might be leave it for symmetry with the way bookmarks are done and with places2csv and metaf.
[but if we do the first thing might as well do both]

Making the query dynamic does appeal to me. But how to do it in a non-spaghetti way?

Attempt:
```python
def hquery(fmt):
    select = []

    q = {
        'v': f'v.visit_time "{HFORMATOPTIONS['v']}"',
        'V': f"strftime('{DATEFORMAT}', \
v.visit_time/1000000-11644473600, 'unixepoch') \"{HFORMATOPTIONS['V']}\"",
        'i': f'v.id "{HFORMATOPTIONS['i']}"',
        'u': f'u.url "{HFORMATOPTIONS['u']}"',
        't': f'u.title "{HFORMATOPTIONS['t']}"',
        'r': f'v.from_visit "{HFORMATOPTIONS['r']}"',
        'n': f'v.transition "{HFORMATOPTIONS['n']}"',
        'd': f'v.visit_duration "{HFORMATOPTIONS['d']}"',
        's': f'c.engagement_score "{HFORMATOPTIONS['s']}"',
        'f': f'v.url "{HFORMATOPTIONS['f']}"',
    }

    for char in fmt:
        opt = HFORMATOPTIONS.get(char)
        if not opt or char not in q:
            continue
        select.append(q[char])

    res = []
    if len(select):
        res.append(f'SELECT {', '.join(select)} from visits v')
        if 'u' in fmt or 't' in fmt:
            res.append('LEFT OUTER JOIN urls u ON v.url = u.id')
        if 's' in fmt:
            res.append(
                'LEFT OUTER JOIN clusters_and_visits c ON v.id = c.visit_id')

    return ' '.join(res)
```
Example result:
```text
73773,39723,https://iana.org/domains/example,Example Domains,13427987707190041,2026-07-08 12:35:07.07.000,0,None
```
The date format isn't good. DATEFORMAT is `%F %T.%f`. With `%Y-%m-%d %H:%M:%f` instead:
```text
73773,39723,https://iana.org/domains/example,Example Domains,13427987707190041,2026-07-08 12:35:07.000,0,None
```
It doesn't really have microseconds. Before (with `get_visit_information`):
```text
73773,39723,https://iana.org/domains/example,Example Domains,13427987707190041,2026-07-08 15:35:07.190041,0,None
```

One thing we could do is make the field be the epoch time and later modify it.
```python
'V': f'v.visit_time "{HFORMATOPTIONS['V']}"',
```
```python
def hformat(d):
    if d.get('V'):
        d['V'] = readable_date_from_epoch(d['V'])
    return d
```

## History update
There is only one thing left: the updating logic. This should be easier than bookmarks, because past visits don't change. So it should suffice to look at the id or date and filter the query to only rows after it.

Utility function for checking fields:
```python
def hhasfields(d, fmt, require_all=True):
    # type: (dict, str, bool) -> bool
    f = all if require_all else any
    return f(HFORMATOPTIONS.get(c) in d for c in fmt)
```

Also now that we have INTFIELDS going to change `parse_csv` to convert them to int
```python
        for field in keys:
            r[field] = int(row[field]) if field in INTFIELDS else row[field]
```
but this is broken as some of our fields can be None, especially engagement score.
```python
        for field in keys:
            r[field] = int(row[field]) if row[field] and field in INTFIELDS \
                else row[field]
```
This still doesn't work for some reason.

> ValueError: invalid literal for int() with base 10: 'None'

Ah, it's a string "None".
```python
        for field in keys:
            r[field] = row[field] if field not in INTFIELDS \
                or row[field] in (None, 'None') else int(row[field])
```
As stupid as it feels to do this.

> ValueError: invalid literal for int() with base 10: '88.58626556396484'

I put transition in INTFIELDS wrongly. Engagement score is also a float sometimes.

The only reason we need to convert them is for sorting new+existing data (because existing is proper types, while existing is strings because it comes from the csv which has not type distinction). What we could do is convert everything to a string in sort, even though it's slightly less efficient.
```python
    def sort_key(item):
        k = HFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = ''
        # To avoid issues sorting existing+new data, because existing
        # data is all strings due to csv having no types distinction
        return str(data_to_sort_by)
```
Then we can get rid of INTFIELDS.

Merge function (copied from places2csv):
```python
def hmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    # Merge visits with existing but only the fields in fmt as there
    # may be fields in existing that are not in current fmt
    fields = [HFORMATOPTIONS[c] for c in fmt]
    return [{k: d[k] for k in fields} for d in existing] + new
```

Start of `cmd_h`:
```python
    existing = None
    update_after = None

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(args.save_to):
            if not args.update:
                msg(f'Save file \'{args.save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

            existing = read_existing(args.save_to)
            if not len(existing):
                msg('Existing is empty. Either the file is empty or something '
                    'went wrong.' + S_UNDESIREDOVERWRITE)
                exit(1)
            one = existing[0]
            # Check we have dates or IDs
            if not hhasfields(one, 'vVi', require_all=False):
                msg('Existing data has no dates or IDs. One of the dates or '
                    'ID fields needs to be present to have a reference point '
                    f'to update from. {one}' + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not hhasfields(one, args.format, require_all=True):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {args.format}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent ID/date in it
                if hhasfields(one, 'i'):
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['i']])
                    update_after = {'id': existing[-1][HFORMATOPTIONS['i']]}
                elif hhasfields(one, 'v'):
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['v']])
                    update_after = {'epoch': existing[-1][HFORMATOPTIONS['v']]}
                elif hhasfields(one, 'V'):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['V']])
                    V = existing[-1].get(BFORMATOPTIONS['V'])
                    update_after = {'epoch': epoch_from_readable_date(V)}
                msg('Updating bookmarks after '
                    f'{update_after}' +
                    (f' ({readable_date_from_epoch(update_after['epoch'])})'
                     if 'epoch' in update_after else ''))

    with sqlite3.connect(
            f"file:{args.file}?mode=ro&nolock=1", uri=True) as con:
        con.row_factory = sqlite3.Row
        con.text_factory = lambda data: str(data, errors="surrogateescape")
        cur = con.cursor()
        res = cur.execute(hquery(args.format, update_after))

    visits = [hformat(dict(s)) for s in res]

    if update_after and not visits:
        msg("No new visits")

    visits = hmerge(existing, visits, args.format) if update_after else visits
```

Then modify `hquery` to take `update_after` and add to the query based on it
```python
        if update_after and 'id' in update_after:
            res.append(f'WHERE v.id > {update_after['id']}')
        elif update_after and 'epoch' in update_after:
            res.append(f'WHERE v.visit_time > {update_after['epoch']}')
```

## Script
I think this is all.
```python
#!/usr/bin/env python3
# chrom2csv
# 2026-07-06 19:19
"""Extracts selected information from Chromium bookmarks and history

Usage:
Bookmarks
    chrom2csv b ~/.config/chromium/Default/Bookmarks -st o.csv --sort a
History
    chrom2csv h ~/.config/chromium/Default/History -st o.csv --sort v

Known issues:
* Stale used dates if using --update and the containing folder hasn't
  changed. This is because only folders have modification dates and
  update uses them to decide which folders to enter, but these
  dates do not change when only use dates change, only if the
  name or url of a bookmark inside the folder changed.
"""
import os
import sys
import csv
import json
import sqlite3
import argparse
from datetime import datetime

PROG = 'chrom2csv'
DEFAULTSAVENAME = 'chrome.csv'
DATEFORMAT = '%F %T.%f'
QUIET = False

BOOKMARK_ROOTS = 'roots'
TOPLEVEL_BOOKMARK_FOLDERS = ["bookmark_bar", "other", "synced"]

BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch', 'C': 'used', 'c': 'used epoch',
    'i': 'id', 'u': 'url', 'n': 'name', 'g': 'guid',
    'p': 'path', 'o': 'position',
}
DEFAULTBFORMATOPTIONS = 'iunaAcCpo'

HFORMATOPTIONS = {
    'V': 'visit', 'v': 'visit epoch',
    'i': 'id', 'u': 'url', 't': 'title',
    'r': 'from', 'n': 'transition', 'd': 'duration', 's': 'engagement score',
    'f': 'fk',
}
DEFAULTHFORMATOPTIONS = 'ifutvVds'

S_UNDESIREDOVERWRITE = """
Exiting to avoid undesired overwriting.
Run with --overwrite if you wish to overwrite.
"""

# Args that we need in several commands but can't make top-level due
# to argument-order issues
REPEATEDARGS = [
    # Maybe there should be a different defaultsavename for each command
    (["-s", "--save"], {"action": "store_true", "help": f"""Save output to
file instead of printing to stdout. Will save to ./{DEFAULTSAVENAME} unless a
different path is provided with the -t/--save-to option."""}),
    (["-t", "--save-to"], {"help": """Path to save output to.
Ignored if -s/--save is not provided."""}),
    (["-o", "--overwrite"], {"action": "store_true", "help": """Overwrite
save file if it already exists. Ignored if -s/--save is not provided."""}),
    (["-u", "--update"], {"action": "store_true", "help": """Update save
file if it already exists. Ignored if -s/--save is not provided.
If both -o/--overwrite and -u/--update are provided,
overwrite will take precedence."""}),
]

COMMANDS = {
    "":                         # TOP LEVEL ARGS
    {"args": [
        (["--dryrun"], {"action": "store_true", "help": """
Don't write to disk."""}),
        (["-q", "--quiet"], {"action": "store_true", "help": """
Suppress messages in output."""}),
    ]},
    "b":                        # B (bookmarks)
    {"desc": "extract bookmarks data", "usage": """[-f FORMAT] [--sort SORT]
[-s] [-t SAVE_TO] [-o] [-u]
file""",
     "args": [
         (["file"], {"help": """Bookmarks file.
e.g. ~/.config/chromium/Default/Bookmarks"""}),
         (["-f", "--format"], {"default": DEFAULTBFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTBFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in BFORMATOPTIONS.items()])}."""}),
         (["--sort"], {"choices":
                       [x for k in BFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: ir -> sort by id, reverse order.)"""}),
         *REPEATEDARGS,
     ]},
    "h":                        # H (history)
    {"desc": "extract history data", "usage": """[-f FORMAT] [--sort SORT]
[-s] [-t SAVE_TO] [-o] [-u]
file""",
     "args": [
         (["file"], {"help": """History file.
e.g. ~/.config/chromium/Default/History"""}),
         (["-f", "--format"], {"default": DEFAULTHFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTHFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in HFORMATOPTIONS.items()])}."""}),
         (["--sort"], {"choices":
                       [x for k in HFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: ir -> sort by id, reverse order.)"""}),
         *REPEATEDARGS,
     ]},
}


def msg(text):
    # type: (str) -> None
    """Utility for logs/errors, could be replaced with proper logging"""
    if not QUIET:
        print(f'{PROG}: {text}')


def readable_date_from_epoch(e):
    """Takes epoch in microseconds since 1 January 1601 00:00 UTC"""
    # type: (int) -> str
    return datetime.fromtimestamp(e/1000000-11644473600).strftime(DATEFORMAT)


def epoch_from_readable_date(d):
    """Returns epoch in microseconds since 1 January 1601 00:00 UTC"""
    # type: (str) -> int
    return int(datetime.fromisoformat(d).timestamp()*1000000)+11644473600


def get_bookmark_information(d, fmt, path, position):
    res = {}

    a = d.get('date_added')
    c = d.get('date_last_used')

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(int(a)) if a and int(a) else '',
        'c': lambda: c,
        'C': lambda: readable_date_from_epoch(int(c)) if c and int(c) else '',
        'i': lambda: d.get('id'),
        'u': lambda: d.get('url'),
        'n': lambda: d.get('name'),
        'g': lambda: d.get('guid'),
        'p': lambda: path,
        'o': lambda: position,
    }

    for char in fmt:
        opt = BFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res


def get_bookmarks_folder_generator(d, fmt, update_after=None, path=''):
    if d.get('type') == 'folder':
        if update_after and d.get('date_modified', 0) < update_after:
            return
        position = 0
        path = f'{path}/{d.get('name')}'
        for c in d.get('children', []):
            if c.get('type') == 'folder':
                yield from get_bookmarks_folder_generator(c, fmt, path)
            elif c.get('type') == 'url':
                yield get_bookmark_information(c, fmt, path, position)
                position += 1


def get_bookmarks_toplevel_generator(d, fmt, update_after=None):
    for g in [get_bookmarks_folder_generator(d.get(f, {}), fmt, update_after)
              for f in TOPLEVEL_BOOKMARK_FOLDERS]:
        yield from g


def get_bookmarks(d, fmt, update_after=None):
    return list(get_bookmarks_toplevel_generator(
        d[BOOKMARK_ROOTS], fmt, update_after))


def hquery(fmt, update_after=None):
    select = []

    q = {
        'v': f'v.visit_time "{HFORMATOPTIONS['v']}"',
        # formatted later in hformat
        'V': f'v.visit_time "{HFORMATOPTIONS['V']}"',
        'i': f'v.id "{HFORMATOPTIONS['i']}"',
        'u': f'u.url "{HFORMATOPTIONS['u']}"',
        't': f'u.title "{HFORMATOPTIONS['t']}"',
        'r': f'v.from_visit "{HFORMATOPTIONS['r']}"',
        'n': f'v.transition "{HFORMATOPTIONS['n']}"',
        'd': f'v.visit_duration "{HFORMATOPTIONS['d']}"',
        's': f'c.engagement_score "{HFORMATOPTIONS['s']}"',
        'f': f'v.url "{HFORMATOPTIONS['f']}"',
    }

    for char in fmt:
        opt = HFORMATOPTIONS.get(char)
        if not opt or char not in q:
            continue
        select.append(q[char])

    res = []
    if len(select):
        res.append(f'SELECT {', '.join(select)} from visits v')
        if 'u' in fmt or 't' in fmt:
            res.append('LEFT OUTER JOIN urls u ON v.url = u.id')
        if 's' in fmt:
            res.append(
                'LEFT OUTER JOIN clusters_and_visits c ON v.id = c.visit_id')
        if update_after and 'id' in update_after:
            res.append(f'WHERE v.id > {update_after['id']}')
        elif update_after and 'epoch' in update_after:
            res.append(f'WHERE v.visit_time > {update_after['epoch']}')

    return ' '.join(res)


def hformat(d):
    V = HFORMATOPTIONS['V']
    if d.get(V):
        d[V] = readable_date_from_epoch(d[V])
    return d


def parse_csv(s):
    # type: (str) -> list
    res = []
    rows = list(csv.DictReader(s.splitlines()))
    keys = list(rows[0].keys())
    for row in rows:
        r = {}
        for field in keys:
            r[field] = row[field]
        res.append(r)
    return res


def read_existing(path):
    # type: (str) -> dict
    existing = None
    with open(path, "r") as f:
        existing = parse_csv(f.read())
    return existing


def bhasfields(d, fmt, require_all=True):
    # type: (dict, str, bool) -> bool
    f = all if require_all else any
    return f(BFORMATOPTIONS.get(c) in d for c in fmt)


def hhasfields(d, fmt, require_all=True):
    # type: (dict, str, bool) -> bool
    f = all if require_all else any
    return f(HFORMATOPTIONS.get(c) in d for c in fmt)


def bmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    """Create dictionary from existing and new with only the fields in fmt
    and no duplicate IDs/GUIDs
    """
    if not existing:
        return new
    i = 'id' if bhasfields(existing[0], 'i') else 'guid'
    fields = [BFORMATOPTIONS[c] for c in fmt]
    by_id = {d[i]: {k: d[k] for k in fields} for d in existing}
    for d in new:
        by_id[d[i]] = d
    return list(by_id.values())


def hmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    # Merge visits with existing but only the fields in fmt as there
    # may be fields in existing that are not in current fmt
    fields = [HFORMATOPTIONS[c] for c in fmt]
    return [{k: d[k] for k in fields} for d in existing] + new


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


def export_csv(a):
    # type: (list) -> str
    fieldnames = [*list((a[0] if len(a) else {}).keys())]
    rows = [fieldnames]
    for item in a:
        rows.append([str(v) for v in item.values()])

    out = CsvOutput()
    writer = csv.writer(out)
    writer.writerows(rows)

    return str(out)


def cmd_b(args):
    save_to = args.save_to or DEFAULTSAVENAME

    existing = None
    update_after = None

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(save_to):
            if not args.update:
                msg(f'Save file \'{save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

            existing = read_existing(save_to)
            if not len(existing):
                msg('Existing is empty. Either the file is empty or something '
                    'went wrong.' + S_UNDESIREDOVERWRITE)
                exit(1)
            one = existing[0]
            # Check we have at least one of the date fields
            if not bhasfields(one, 'aAcC', require_all=False):
                msg('Existing data has no date fields. Without dates '
                    'in the data there is no reference point, so everything '
                    f'would have to be queried from scratch. {one}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check we have IDs or GUIDs
            if not bhasfields(one, 'ig', require_all=False):
                msg('Existing data has no IDs or GUIDs. Without identifiers '
                    'in the data there is no way to know which bookmarks were '
                    'modified.' + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not bhasfields(one, args.format, require_all=True):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {args.format}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent date in it
                a = c = 0
                if bhasfields(one, 'a', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['a']])
                    a = existing[-1].get(BFORMATOPTIONS['a'])
                elif bhasfields(one, 'A', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['A']])
                    A = existing[-1].get(BFORMATOPTIONS['A'])
                    a = epoch_from_readable_date(A)
                if bhasfields(one, 'c', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['c']])
                    c = existing[-1].get(BFORMATOPTIONS['c'])
                elif bhasfields(one, 'C', require_all=False):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['C']])
                    C = existing[-1].get(BFORMATOPTIONS['C'])
                    c = epoch_from_readable_date(C)
                update_after = a if a > c else c
                msg('Updating bookmarks after '
                    f'{update_after} '
                    f'({readable_date_from_epoch(int(update_after))})')

    with open(args.file, 'r') as f:
        data = json.load(f)

    bookmarks = get_bookmarks(data, args.format, update_after)

    if update_after and not bookmarks:
        msg("No new data")

    bookmarks = bmerge(
        existing, bookmarks, args.format) if update_after else bookmarks

    def sort_key(item):
        k = BFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = ''
        return data_to_sort_by

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        msg(f'Sorting by {args.sort[0]}{" in reverse" if reverse else ""}')
        bookmarks.sort(key=sort_key, reverse=reverse)

    dump = export_csv(bookmarks)

    if args.save and not args.dryrun:
        open_as = 'w' if args.overwrite or args.update else 'x'
        with open(save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {save_to}')
    else:
        print(dump)


def cmd_h(args):
    existing = None
    update_after = None

    # Fail fast if save file already exists and overwrite is False
    # unless update is True
    if args.save and not args.overwrite:
        if os.path.exists(args.save_to):
            if not args.update:
                msg(f'Save file \'{args.save_to}\' already exists. '
                    'Run with -u/--update if you wish to update it. '
                    'Run with -o/--overwrite if you wish to overwrite it. '
                    'Run with -t/--save-to to specify a different save file.')
                exit(1)

            existing = read_existing(args.save_to)
            if not len(existing):
                msg('Existing is empty. Either the file is empty or something '
                    'went wrong.' + S_UNDESIREDOVERWRITE)
                exit(1)
            one = existing[0]
            # Check we have dates or IDs
            if not hhasfields(one, 'vVi', require_all=False):
                msg('Existing data has no dates or IDs. One of the dates or '
                    'ID fields needs to be present to have a reference point '
                    f'to update from. {one}' + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not hhasfields(one, args.format, require_all=True):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {args.format}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent ID/date in it
                if hhasfields(one, 'i'):
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['i']])
                    update_after = {'id': existing[-1][HFORMATOPTIONS['i']]}
                elif hhasfields(one, 'v'):
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['v']])
                    update_after = {'epoch': existing[-1][HFORMATOPTIONS['v']]}
                elif hhasfields(one, 'V'):
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['V']])
                    V = existing[-1].get(BFORMATOPTIONS['V'])
                    update_after = {'epoch': epoch_from_readable_date(V)}
                msg('Updating bookmarks after '
                    f'{update_after}' +
                    (f' ({readable_date_from_epoch(update_after['epoch'])})'
                     if 'epoch' in update_after else ''))

    with sqlite3.connect(
            f"file:{args.file}?mode=ro&nolock=1", uri=True) as con:
        con.row_factory = sqlite3.Row
        con.text_factory = lambda data: str(data, errors="surrogateescape")
        cur = con.cursor()
        res = cur.execute(hquery(args.format, update_after))

    visits = [hformat(dict(s)) for s in res]

    if update_after and not visits:
        msg("No new visits")

    visits = hmerge(existing, visits, args.format) if update_after else visits

    def sort_key(item):
        k = HFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = ''
        # To avoid issues sorting existing+new data, because existing
        # data is all strings due to csv having no types distinction
        return str(data_to_sort_by)

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        msg(f'Sorting by {args.sort[0]}{" in reverse" if reverse else ""}')
        visits.sort(key=sort_key, reverse=reverse)

    dump = export_csv(visits)

    if args.save and not args.dryrun:
        open_as = 'w' if args.overwrite or args.update else 'x'
        with open(args.save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {args.save_to}')
    else:
        print(dump)


def parse_args():
    parser = argparse.ArgumentParser(prog=PROG)
    parser.suggest_on_error = True
    subparsers = parser.add_subparsers(dest="command", required=True)
    descs = []
    for k, v in COMMANDS.items():
        if v.get('desc'):       # description generation
            descs.append(f"{v['desc']} ({k})")
        if k == "":             # arguments on top parser
            for args, kwargs in v['args']:
                parser.add_argument(*args, **kwargs)
            continue
        subparser = subparsers.add_parser(
            k, help=f"[-h|--help] {v.get('usage')}")
        for args, kwargs in v['args']:
            subparser.add_argument(*args, **kwargs)
        # Set handler
        subparser.set_defaults(func=v.get('func'))
    # Add generated description
    if len(descs) > 1:
        descs[-1] = f"or {descs[-1]}"
    parser.description = f"A utility script to \
{' '.join(descs)} from Chromium files."
    # Parse args / print help and quit if no args
    # (Primer https://stackoverflow.com/a/47440202/18396947)
    return parser.parse_args(sys.argv[1:] or ['--help'])


COMMANDS['b']['func'] = cmd_b
COMMANDS['h']['func'] = cmd_h


def main():
    # type: () -> None
    global QUIET

    args = parse_args()
    QUIET = args.quiet

    args.func(args)


if __name__ == '__main__':
    main()
```

I'll put this and future updates in [dotfiles/scripts/chrom2csv](https://github.com/plu5/dotfiles/blob/main/pm/scripts/chrom2csv).

{% include fin.html %}
