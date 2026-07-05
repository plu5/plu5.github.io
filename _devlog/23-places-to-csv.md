---
layout: post
title: 23 — Places to CSV
date: 2026-06-28 23:38
modified_date: 2026-07-05 06:22
categories: firefox sqlite csv
lang: en
redirect_from: /devlog/23
---

## What I'm trying to do
I want to extract interesting history data from old places.sqlite files. Informed by my experiences with my script metaf (which collects metadata of files on disk), I think a nice format for the output would be csv; it's space efficient, human readable, machine readable, and widely supported.

## Reading SQLite without modifying the file
There's `mode=ro` or `immutable=1`. The latter is for if the file is not going to change on disk. From Python it can be specified like this:
```python
con = sqlite3.connect("file:tutorial.db?mode=ro", uri=True)
```
([ref: sqlite3 Python docs](https://docs.python.org/3/library/sqlite3.html#how-to-work-with-sqlite-uris))

[https://www.sqlite.org/uri.html](https://www.sqlite.org/uri.html)

[Keith Medcalf](https://sqlite.org/forum/info/02f7bda329f41e30) on immutable vs read-only:

> "more=ro" means that someone else can update the database, and that if they do so, then the current page cache will be invalidated and all page requests will generate an I/O request to the OS, although the OS may still be able to satisfy the request from the OS level cache without actually perforoming a physical I/O operation, the CPU must still enter Supervisor mode to perform the I/O operation.  
However, with "immutable=1" no checks are done to see if the database is being updated by someone else and if someone else does update the database then shit will likely break loose and hit the oscillatory device, smearing everything in sight with excrement ...  
See [https://sqlite.org/c3ref/open.html](https://sqlite.org/c3ref/open.html)

## Tables in places.sqlite
```python
#!/usr/bin/env python3
import sqlite3

FILE = "/home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite"

con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
cur = con.cursor()
res = cur.execute("SELECT name FROM sqlite_master")
print(res.fetchall())
```

```python
[('moz_origins',), ('sqlite_autoindex_moz_origins_1',), ('moz_places',), ('moz_places_extra',), ('moz_historyvisits',), ('moz_historyvisits_extra',), ('moz_inputhistory',), ('sqlite_autoindex_moz_inputhistory_1',), ('moz_bookmarks',), ('moz_bookmarks_deleted',), ('sqlite_autoindex_moz_bookmarks_deleted_1',), ('moz_keywords',), ('sqlite_autoindex_moz_keywords_1',), ('sqlite_sequence',), ('moz_anno_attributes',), ('sqlite_autoindex_moz_anno_attributes_1',), ('moz_annos',), ('moz_items_annos',), ('moz_meta',), ('moz_places_metadata',), ('moz_places_metadata_search_queries',), ('sqlite_autoindex_moz_places_metadata_search_queries_1',), ('moz_previews_tombstones',), ('sqlite_stat1',), ('moz_places_url_hashindex',), ('moz_places_hostindex',), ('moz_places_visitcount',), ('moz_places_frecencyindex',), ('moz_places_lastvisitdateindex',), ('moz_places_guid_uniqueindex',), ('moz_places_originidindex',), ('moz_places_altfrecencyindex',), ('moz_historyvisits_placedateindex',), ('moz_historyvisits_fromindex',), ('moz_historyvisits_dateindex',), ('moz_bookmarks_itemindex',), ('moz_bookmarks_parentindex',), ('moz_bookmarks_itemlastmodifiedindex',), ('moz_bookmarks_dateaddedindex',), ('moz_bookmarks_guid_uniqueindex',), ('moz_keywords_placepostdata_uniqueindex',), ('moz_annos_placeattributeindex',), ('moz_items_annos_itemattributeindex',), ('moz_places_metadata_placecreated_uniqueindex',), ('moz_places_metadata_referrerindex',), ('moz_newtab_story_click',), ('moz_newtab_story_impression',), ('moz_newtab_story_click_newtab_click_timestampindex',), ('moz_newtab_story_impression_newtab_impression_timestampindex',), ('moz_newtab_shortcuts_interaction',), ('moz_newtab_shortcuts_interaction_timestampindex',), ('moz_newtab_shortcuts_interaction_placeidindex',)]
```

## `moz_places` and `moz_historyvisits`
```python
con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
con.row_factory = sqlite3.Row
cur = con.cursor()
res = cur.execute("SELECT * FROM moz_places")
for s in res:
    print(dict(s))
con.close()
```
```python
{'id': 90989, 'url': 'https://github.com/plu5/retype/pull/67/changes', 'title': 'feat: Add command fillChars by plu5 · Pull Request #67 · plu5/retype', 'rev_host': 'moc.buhtig.', 'visit_count': 1, 'hidden': 0, 'typed': 0, 'frecency': 20775, 'last_visit_date': 1780391829389774, 'guid': 'JCJVNCWMBJ4v', 'foreign_count': 0, 'url_hash': 47360578746634, 'description': None, 'preview_image_url': None, 'site_name': None, 'origin_id': 27, 'recalc_frecency': 0, 'alt_frecency': None, 'recalc_alt_frecency': 1}
```

no first visit date? iirc there is a way to find out in mzhistoryview

`moz_places_extra`:
empty

`moz_historyvisits`:
```python
# ..
{'id': 124066, 'from_visit': 124065, 'place_id': 82686, 'visit_date': 1782695514062324, 'visit_type': 5, 'session': 0, 'source': 0, 'triggeringPlaceId': None}
{'id': 124067, 'from_visit': 0, 'place_id': 95366, 'visit_date': 1782695998141012, 'visit_type': 2, 'session': 0, 'source': 3, 'triggeringPlaceId': None}
```

`SELECT * FROM moz_historyvisits WHERE place_id = 90989`:
```python
{'id': 118391, 'from_visit': 118390, 'place_id': 90989, 'visit_date': 1780391829389774, 'visit_type': 1, 'session': 0, 'source': 0, 'triggeringPlaceId': None}
```

## How to structure the data
It would be useful to have all the dates a page was visited, but it could be a lot, resulting on a lot of data on a single entry. Maybe would be better then to create multiple entries to have a chronological view.

Idea for fields:

url,title,description,count,date,date epoch

(no worries about the possible presence of commas in the fields, python's csv modules handles it automatically by wrapping in quotes if needed)

Don't really care about the IDs. But maybe I should. Would be nice to be able to re-run and update what's changed, like metaf. But unlike metaf, other than the count column nothing should change in the history, only rows added. So no ID and get rid of count to avoid having to keep it up to date.

Revised:

url,title,description,date,localdate

Where localdate is a formatted human-readable date in the user's local timezone.

Actually, how can there even be a description for history entries? I don't think I have ever seen that, and upon verifying I don't see description in the UI for them. Even bookmarks don't have a description field. In the data for most of them it's None, but

```python
{'id': 6, 'url': 'https://www.mozilla.org/en-US/privacy/firefox/', 'title': 'Firefox Privacy Notice — Mozilla', 'rev_host': 'gro.allizom.www.', 'visit_count': 1, 'hidden': 0, 'typed': 0, 'frecency': 20249, 'last_visit_date': 1734995982979343, 'guid': '69dJnPsIxKlC', 'foreign_count': 0, 'url_hash': 47358032558425, 'description': '\n  Our Privacy Notices describe the data our products and services receive, share, and use, as well as choices available to you.\n', 'preview_image_url': 'https://www.mozilla.org/media/img/m24/og.5b9d3fdd1709.png', 'site_name': None, 'origin_id': 1, 'recalc_frecency': 0, 'alt_frecency': None, 'recalc_alt_frecency': 1}
```

The dates are in microseconds. Divide by a million before passing it to datetime.
```python
>>> datetime.fromtimestamp(1735033579463075/1000000).strftime('%F %T')
'2024-12-24 11:46:19'
```

## Proof of concept
It seems we can't have another cur.execute inside the loop, it stops after the first one. Since even the first visit appears in historyvisits, we would have duplicates that way in any case. First go through places saving the data we need, including the ID, then iterate through it getting the historyvisits for each ID.
```python
#!/usr/bin/env python3
import csv
import sqlite3
from datetime import datetime

FILE = "/home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite"


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
    # type: list -> str
    fieldnames = [*list((a[0] if len(a) else {}).keys())]
    rows = [fieldnames]
    for item in a:
        rows.append([str(v) for v in item.values()])

    out = CsvOutput()
    writer = csv.writer(out)
    writer.writerows(rows)

    return str(out)


con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
con.row_factory = sqlite3.Row
cur = con.cursor()
res = cur.execute("SELECT * FROM moz_places")
items = []
for s in res:
    d = dict(s)
    items.append({'id': d.get('id'),
                  'url': d.get('url'), 'title': d.get('title')})

visits = []
for item in items:
    cross = cur.execute(
        f"SELECT * FROM moz_historyvisits WHERE place_id = {item['id']}")
    for s in cross:
        d = dict(s)
        date = d.get('visit_date')
        localdate = datetime.fromtimestamp(date/1000000).strftime('%F %T') \
            if date else None
        new_item = item.copy()
        new_item['date'] = date
        new_item['localdate'] = localdate
        visits.append(new_item)

visits.sort(key=lambda d: d['date'])
print(export_csv(visits))

con.close()
```

## Save, update, interface
save to disk

update existing file

metaf has the feature of being able to choose the fields you want, perhaps should do the same here

adapting metaf so it will have a similar interface to it in general

for update could adjust the sql query to only get over a certain id but the problem is i made id optional to save in the format. could say it's only supported if there is id in the output. but another problem is if update is ran with differnet format options. what metaf does in that case is chagne the fileds to match that one and also updates modification dates on existing, but for us it's different since hte cost isn't getting the fields but the sql queries hence why we don't want to have to update existing ideally

i guess load them from disk, check if there is an id and if not error that we can't update without id-- actually no, just message that witohut id in the format option we have to obtain all the rows so it's not any more optimised than overwrite / doing it from scratch. then check if the format options provided are the same or less than what's on disk, in which case sort by id, and only get the entries over that id.

wait, another issue is there could be new visits for old ids, so we can't really do it that way. instead we could look at last visit date. query to only get entries whose last visit date is greater than the newest visit date in our data. for missing format options, we could do a reverse query to what we do usually: get the entry from the place id on the visit.

so:
- read existing data
- if no date, message that without date in the data we have to query everything so update is no better than starting over
- if it's human readable date rather than micro epoch that's fine also, we can calculate the micro epoch from it
- if format options contain options that are not in the data, this also means starting over
- otherwise, sort data in order to get the most recent date in it
- query entries with last visit date over that date
- for each of the resulting entries, query visits over that date
- merge visits with existing but only the fields in format options as there may be fields in existing that are not in current format options

but it may well be that reading and parsing the csv from disk is slower than making the sql queries, making update pointless or worse than starting over.
i guess the result could be different if some entries/visits are now gone; update would preserve them while overwrite would not. this isn't rare, because firefox by default does delete history.

{% include note.html content='
> [!NOTE]
> I was wrong, update is still much faster. And just like metaf it\'s useful to run it with --update to change sort or fields order.
' %}

example for the date calculation:
1734998809968932,2024-12-24 02:06:49

the latter is obtained from the former with:
```python
datetime.fromtimestamp(1734998809968932/1000000).strftime('%F %T')
```
how to do the reverse?
```python
int(datetime.fromisoformat('2024-12-24 02:06:49').timestamp()*1000000)
```
gives 1734998809000000, which is 2024-12-24 02:06:49 local time. it will do.

actually i think we might have an issue with that visit appearing twice in the updated results.

maybe rather than adding deduplication logic it would be best to add nanoseconds to our date format.
```python
datetime.fromtimestamp(1734998809968932/1000000).strftime('%F %T.%N')
```
I guess that's not supported, I get a literal %N :-/

but it's on microseconds anyway, not nanoseconds. and strftime does support %f:
```python
datetime.fromtimestamp(1734998809968932/1000000).strftime('%F %T.%f')
```
'2024-12-24 02:06:49.968932'

and back again:
```python
int(datetime.fromisoformat('2024-12-24 02:06:49.968932').timestamp()*1000000)
```
1734998809968932

no loss of precision

i don't know if you can tell but my mental health is at a low point while working on this. hence little effort spelling and formatting. i can barely get myself to move forwards. maybe it's fine and i should be like this always, not caring about superficial things and just pressing on.

```python
#!/usr/bin/env python3
# places2csv
# 2026-06-29 03:37
"""Extracts selected information from places.sqlite

Usage:
    places2csv ~/.mozilla/firefox/*.default-release/places.sqlite -st ~/out.csv --sort v  # noqa: E501

(the noqa is just to avoid flake8 shouting at me for line length, you
don't need it in the command, but it won't hurt either since # is a
comment in most shells)

Note that the same id can appear multiple times (it generates a row
per visit, the id is the place id, and the same place can have
multiple visits)
"""
import os
import csv
import sqlite3
import argparse
from datetime import datetime

PROG = 'places2csv'
DEFAULTSAVENAME = 'places.csv'
# Caution: If the date format is changed to less precision, there will
# be an issue with duplicates when updating data with no epoch field
DATEFORMAT = '%F %T.%f'

FORMATOPTIONS = {
    'V': 'visit', 'v': 'visit epoch',
    'i': 'id', 'u': 'url', 't': 'title', 'd': 'description',
}
DEFAULTFORMATOPTIONS = 'iutvV'
INTFIELDS = [FORMATOPTIONS['v'], FORMATOPTIONS['i']]

QUIET = False


def msg(text):
    # type: (str) -> None
    """Utility for logs/errors, could be replaced with proper logging"""
    if not QUIET:
        print(f'{PROG}: {text}')


def readable_date_from_epoch(e):
    """Takes epoch in microseconds"""
    # type: (int) -> str
    return datetime.fromtimestamp(e/1000000).strftime(DATEFORMAT)


def epoch_from_readable_date(d):
    """Returns epoch in microseconds"""
    # type: (str) -> int
    return int(datetime.fromisoformat(d).timestamp()*1000000)


def get_visit_information(entry_dict, visit_dict, fmt):
    # type: (dict, dict, str) -> dict
    res = {}

    v = visit_dict.get('visit_date')

    # This architecture is from metaf but doesn't make as much sense
    # here since our calls are not heavy. It would probably cheaper to
    # preset all of them instead of having the lambdas. But maybe in
    # future we would want to add a field that would be heavier.
    dispatch = {
        'v': lambda: v,  # or 0 instead of None?
        'V': lambda: readable_date_from_epoch(v) if v else None,
        'i': lambda: entry_dict.get('id'),  # is this int or str?
        'u': lambda: entry_dict.get('url'),
        't': lambda: entry_dict.get('title'),
        'd': lambda: entry_dict.get('description'),
    }
    # ^ not sure if fallbacks should be None or 0 / ''. I want to be
    # able to tell if it didn't exist in the dictionary so I'm leaving
    # None for now but I'm not sure how csv handles it.

    for char in fmt:
        opt = FORMATOPTIONS.get(char)
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
            r[field] = int(row[field]) if field in INTFIELDS else row[field]
        res.append(r)
    return res


def read_existing(path):
    # type: str -> dict | None
    existing = None
    with open(path, "r") as f:
        existing = parse_csv(f.read())
    return existing


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


def merge(existing, new, fmt):
    # type: (list, list, str) -> list
    # Merge visits with existing but only the fields in fmt as there
    # may be fields in existing that are not in current fmt
    fields = [FORMATOPTIONS[c] for c in fmt]
    return [{k: d[k] for k in fields} for d in existing] + new


def parse_args():
    parser = argparse.ArgumentParser(prog=PROG)
    parser.add_argument(
        'places_sqlite',
        help='places.sqlite file')
    parser.add_argument(
        '-q', '--quiet', action='store_true',
        help='Suppress messages in output.')
    parser.add_argument(
        '-f', '--format', default=DEFAULTFORMATOPTIONS,
        help='Which metadata fields to include and in which order. '
        f'Default: {DEFAULTFORMATOPTIONS}. '
        'Options: '
        f'{", ".join([f"{k} ({v})" for k, v in FORMATOPTIONS.items()])}.')
    parser.add_argument(
        '--sort', choices=[x for k in FORMATOPTIONS for x in (k, k + 'r')],
        help='Sort by a given metadata field. Options are the same as for '
        '-f/--format, or followed by r for reverse order. '
        '(Example: vr -> sort by visit epoch, reverse order.)')
    parser.add_argument(
        '-s', '--save', action='store_true',
        help='Save output to file instead of printing to stdout. '
        f'Will save to ./{DEFAULTSAVENAME} unless a different path '
        'is provided with the -t/--save-to option.')
    parser.add_argument(
        '-t', '--save-to',
        help='Path to save output to. '
        'Ignored if -s/--save is not provided.')
    parser.add_argument(
        '-o', '--overwrite', action='store_true',
        help='Overwrite save file if it already exists. '
        'Ignored if -s/--save is not provided.')
    parser.add_argument(
        '-u', '--update', action='store_true',
        help='Update save file if it already exists. '
        'Ignored if -s/--save is not provided. If both -o/--overwrite '
        'and -u/--update are provided, overwrite will take precedence.')
    parser.add_argument(
        '--dryrun', action='store_true',
        help='Don\'t write to disk.')
    return parser.parse_args()


def main():
    # type: () -> None
    global QUIET

    args = parse_args()
    QUIET = args.quiet

    save_to = args.save_to or DEFAULTSAVENAME
    fmt = args.format or DEFAULTFORMATOPTIONS

    update_after = None
    existing = None

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
                msg('existing is empty')
            one = existing[0]
            V = one.get(FORMATOPTIONS['V'])
            v = one.get(FORMATOPTIONS['v'])
            # Check format options are all there
            one['visit_date'] = v or epoch_from_readable_date(V)
            test = get_visit_information(existing[0], existing[0], fmt)
            if None in test.values():
                msg('Existing data does not contain all format '
                    f'options, so we have to query everything. {test} | {one}')
            # Check we have dates
            elif not v and not V:
                msg('Existing data has no visit dates. Without dates in the '
                    'data there is no reference point, we have to query '
                    'everything.')
            else:
                # Sort data in order to get the most recent date in it
                if v:
                    existing.sort(key=lambda r: r[FORMATOPTIONS['v']])
                else:
                    existing.sort(key=lambda r: r[FORMATOPTIONS['V']])
                V = existing[-1].get(FORMATOPTIONS['V'])
                v = existing[-1].get(FORMATOPTIONS['v'])
                if not v:
                    v = epoch_from_readable_date(V)
                update_after = v
                msg(f'Adding visits after {v} ({readable_date_from_epoch(v)})')

    con = sqlite3.connect(f"file:{args.places_sqlite}?mode=ro", uri=True)
    con.row_factory = sqlite3.Row
    cur = con.cursor()

    query = "SELECT * FROM moz_places"
    query += f' WHERE last_visit_date > {update_after}' if update_after else ''
    res = cur.execute(query)
    entries = [dict(s) for s in res]

    # Get visits for each entry
    visits = []
    for entry in entries:
        query = ("SELECT * FROM moz_historyvisits "
                 f'WHERE place_id = {entry['id']}')
        query += f' AND visit_date > {update_after}' if update_after else ''
        cross = cur.execute(query)
        for s in cross:
            visits.append(get_visit_information(entry, dict(s), fmt))

    con.close()

    visits = merge(existing, visits, fmt) if update_after else visits

    def sort_key(item):
        k = FORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data, can't sort by it")
            data_to_sort_by = False
        # REMARK(plu5): I set it ^ to False because one can't compare
        # None to None, but comparing booleans is fine.
        return data_to_sort_by

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        visits.sort(key=sort_key, reverse=reverse)

    dump = export_csv(visits)

    if args.save and not args.dryrun:
        open_as = 'w' if args.overwrite or args.update else 'x'
        with open(save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {save_to}')
    else:
        print(dump)


if __name__ == '__main__':
    main()
```

I will put it and future updates in [dotfiles/scripts/places2csv](https://github.com/plu5/dotfiles/blob/main/pm/scripts/places2csv).

## Problem with decoding
`dict(s)` where we turn a sqlite object into a Python dictionary can fail with `sqlite3.OperationalError` apparently.

> Could not decode to UTF-8 column 'description' with text

This only happens with some old files. It could be that they are corrupted. But I thought if an SQLite file corrupts, the whole thing becomes unreadable?

[SE](https://stackoverflow.com/questions/22751363/sqlite3-operationalerror-could-not-decode-to-utf-8-column)

[docs](https://docs.python.org/fr/3/library/sqlite3.html)

> Because of SQLite's flexible typing, it is not uncommon to encounter table columns with the TEXT data type containing non-UTF-8 encodings, or even arbitrary data.

> For invalid UTF-8 or arbitrary data in stored in TEXT table columns, you can use the following technique, borrowed from the [Guide Unicode](https://docs.python.org/3/howto/unicode.html#unicode-howto):
```python
con.text_factory = lambda data: str(data, errors="surrogateescape")
```

And the "Guide Unicode" says:

> The `surrogateescape` error handler will decode any non-ASCII bytes as code points in a special range running from U+DC80 to U+DCFF. These code points will then turn back into the same bytes when the surrogateescape error handler is used to encode the data and write it back out.

## Space comparison
A 62.9 MB places.sqlite resulted in a 19 MB csv file for me. Obviously we're not saving all data, this is not me trying to compare csv and sqlite, just giving an idea for output size. That places.sqlite is of about a year and a half. I also have a places.sqlite of about a decade which is 388 MB, which results in a csv file of 101 MB. That's a surprisingly large proportion considering we're only storing a small subset of the data.

The main interest for me is the accessibility of the resulting data, as in that format we can even read and and search through it with just a text editor, while still retaining the ability to do higher effort analysis with a script or spreadsheet software. Previously I had used MZHistoryView but it's a bit of a faff.

## Bookmarks data
There seem to be bookmarks and other things in the same file, so maybe we could do something with this. I already backup bookmarks by simply symlinking the subfolder bookmarkbackups in the profile folder, which contains the last 15 jsonlz4 backups Firefox makes automatically every day. It could be interesting to have a csv of bookmarks to be able to search them easily. Firefox already has export options, but they tend to be heavy because it saves the favicon for each bookmark. They're not all 16x16 either, some are large and it's a huge wall of text of base64.

Ok fine, I wanted to stop but you've convinced me, let's look at bookmarks while we're here.

```python
#!/usr/bin/env python3
import sqlite3

FILE = "/home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite"

con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
con.row_factory = sqlite3.Row
cur = con.cursor()
res = cur.execute("SELECT * FROM moz_bookmarks")
for i, s in enumerate(res):
    print(dict(s))
    if i > 50:
        break
con.close()
```

The first few are kind of cryptic
```python
{'id': 1, 'type': 2, 'fk': None, 'parent': 0, 'position': 0, 'title': '', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1782791981930000, 'guid': 'root________', 'syncStatus': 1, 'syncChangeCounter': 1}
{'id': 2, 'type': 2, 'fk': None, 'parent': 1, 'position': 0, 'title': 'menu', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1734995982912000, 'guid': 'menu________', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3, 'type': 2, 'fk': None, 'parent': 1, 'position': 1, 'title': 'toolbar', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1782791981930000, 'guid': 'toolbar_____', 'syncStatus': 1, 'syncChangeCounter': 122}
{'id': 4, 'type': 2, 'fk': None, 'parent': 1, 'position': 2, 'title': 'tags', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1734995982604000, 'guid': 'tags________', 'syncStatus': 1, 'syncChangeCounter': 1}
{'id': 5, 'type': 2, 'fk': None, 'parent': 1, 'position': 3, 'title': 'unfiled', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1778813661389000, 'guid': 'unfiled_____', 'syncStatus': 1, 'syncChangeCounter': 9}
{'id': 6, 'type': 2, 'fk': None, 'parent': 1, 'position': 4, 'title': 'mobile', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982612000, 'lastModified': 1734995982903000, 'guid': 'mobile______', 'syncStatus': 1, 'syncChangeCounter': 2}
```
followed by some default bookmarks maybe
```python
{'id': 7, 'type': 2, 'fk': None, 'parent': 2, 'position': 0, 'title': 'Mozilla Firefox', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982912000, 'lastModified': 1734995982912000, 'guid': 'MIaQgFa765sG', 'syncStatus': 0, 'syncChangeCounter': 1}
{'id': 8, 'type': 1, 'fk': 2, 'parent': 7, 'position': 0, 'title': 'Get Help', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982912000, 'lastModified': 1734995982912000, 'guid': 'uq5zZQUkV9j2', 'syncStatus': 0, 'syncChangeCounter': 1}
{'id': 9, 'type': 1, 'fk': 3, 'parent': 7, 'position': 1, 'title': 'Customize Firefox', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982912000, 'lastModified': 1734995982912000, 'guid': 'ukx-MvRU5b9l', 'syncStatus': 0, 'syncChangeCounter': 1}
{'id': 10, 'type': 1, 'fk': 4, 'parent': 7, 'position': 2, 'title': 'Get Involved', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982912000, 'lastModified': 1734995982912000, 'guid': '15mxM6XRJMtt', 'syncStatus': 0, 'syncChangeCounter': 1}
{'id': 11, 'type': 1, 'fk': 5, 'parent': 7, 'position': 3, 'title': 'About Us', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982912000, 'lastModified': 1734995982912000, 'guid': 'HL4pE_lBL-Md', 'syncStatus': 0, 'syncChangeCounter': 1}
```
first one I can attest to:
```python
{'id': 12, 'type': 1, 'fk': 30, 'parent': 24, 'position': 8, 'title': 'Custom Arch Linux setup with Openbox | Yash Agarwal', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1735002439338000, 'lastModified': 1735059516250000, 'guid': 'iQDyUCVeScKz', 'syncStatus': 1, 'syncChangeCounter': 2}
```

I've never used sync on this machine and I'm not connected to an account. Not sure why syncStatus is different. On the first few it was 1 also for some reason.

When you look at a bookmark on Firefox UI (Library), it has the following fields:
- Name
- URL
- Tags
- Keyword

I've never really used tags. Keyword is a really useful feature. It's similar to how you can select a search engine in the address bar. For example I have it set up that when I type @gimg it does a google image search with whatever I type next, and when I type @gh it goes to my GitHub profile. The @ at the start is not required, I could have just made it "gh", it's just nicer that way that it's not going to pop up when you're typing something normal.

Presumably `keyword_id` is pointing to a row in another table. But where is the URL and tags?

It's also going to be hard to show where the bookmark is located. Maybe it can be in a path-like syntax on a field, like "Bookmarks Toolbar/folder/subfolder" or something like that.

I feel like this is stupid given Firefox already has export functionality and with its export functionality you can actually import the bookmarks back. It would probably be better to do HTML export and then modify the result to make all the favicons empty.

Maybe do an HTML export and see what information is needed there so that we are able to include it all in our output, then it would be possible to convert it to HTML to be able to import.

[~~Or JSON (other than HTML there's also JSON import and export)~~]
[or not? apparently it's only for backups]

Let's look at keyword id first. Looking at the [tables in places.sqlite](#tables-in-places-sqlite), there is one called `moz_keywords`.
```python
con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
con.row_factory = sqlite3.Row
cur = con.cursor()
res = cur.execute("SELECT * FROM moz_keywords")
for i, s in enumerate(res):
    print(dict(s))
    if i > 50:
        break
con.close()
```
I don't have that many, this is everything:
```python
{'id': 1, 'keyword': 'edit', 'place_id': 58130, 'post_data': ''}
{'id': 2, 'keyword': 'tlfr', 'place_id': 35283, 'post_data': ''}
{'id': 5, 'keyword': '@gh', 'place_id': 431, 'post_data': ''}
{'id': 12, 'keyword': '@icaps', 'place_id': 71571, 'post_data': ''}
{'id': 13, 'keyword': '@ifav', 'place_id': 71681, 'post_data': ''}
{'id': 14, 'keyword': '@ipers', 'place_id': 71697, 'post_data': ''}
{'id': 15, 'keyword': '@ihr', 'place_id': 56723, 'post_data': ''}
{'id': 16, 'keyword': '@irefs', 'place_id': 5038, 'post_data': ''}
{'id': 17, 'keyword': '@wipsite', 'place_id': 11367, 'post_data': ''}
{'id': 19, 'keyword': '@r', 'place_id': 74312, 'post_data': ''}
{'id': 20, 'keyword': '@vids', 'place_id': 62831, 'post_data': ''}
{'id': 21, 'keyword': '@cara', 'place_id': 63977, 'post_data': ''}
{'id': 24, 'keyword': '@gimgw', 'place_id': 339, 'post_data': ''}
```
`place_id`!!! I guess that `moz_places` is for bookmarks as well, then? That explains where the URL is.

By the way, you will notice that my @gimg keyword isn't there. Because I guess the ones for search engines is something separate. @gimgw is just a bookmark I had made to google.com/imghp for when I need to go on the page itself, to use reverse image search for example.

Looking at the tables again, I wonder what `moz_annos` is.
```python
con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
con.row_factory = sqlite3.Row
cur = con.cursor()
res = cur.execute("SELECT * FROM moz_annos")
for i, s in enumerate(res):
    print(dict(s))
    if i > 50:
        break
con.close()
```
I can't tell. Is it local files? Ah, it's downloads! Or is it? It seems to be just image files and occasionally HTML, but maybe that's just because this I save images and pages a lot more often than download zip files and stuff like that? Yes, if I remove the limit I'm seeing zips too in the more recent stuff.
```python
{'id': 1034, 'place_id': 92537, 'anno_attribute_id': 1, 'content': 'file:///home/pm/Downloads/retype-1.7.0-prerelease3-macos-x86.zip', 'flags': 0, 'expiration': 4, 'type': 3, 'dateAdded': 1781129923350000, 'lastModified': 1781129923350000}
{'id': 1035, 'place_id': 92537, 'anno_attribute_id': 2, 'content': '{"state":1,"deleted":false,"endTime":1781129934173,"fileSize":25396353}', 'flags': 0, 'expiration': 4, 'type': 3, 'dateAdded': 1781129934199000, 'lastModified': 1781129934199000}
{'id': 1036, 'place_id': 92915, 'anno_attribute_id': 1, 'content': "file:///home/pm/Downloads/PC%20_%20Computer%20-%20Assassin's%20Creed_%20Chronicles%20-%20Enemies%20-%20Chinese%20Males.zip", 'flags': 0, 'expiration': 4, 'type': 3, 'dateAdded': 1781325617135000, 'lastModified': 1781325617135000}
{'id': 1037, 'place_id': 92915, 'anno_attribute_id': 2, 'content': '{"state":1,"deleted":false,"endTime":1781325617453,"fileSize":688330}', 'flags': 0, 'expiration': 4, 'type': 3, 'dateAdded': 1781325617485000, 'lastModified': 1781325617485000}
{'id': 1038, 'place_id': 94206, 'anno_attribute_id': 3, 'content': 'windows-1252', 'flags': 0, 'expiration': 4, 'type': 3, 'dateAdded': 1782106724472000, 'lastModified': 1782106724472000}
{'id': 1039, 'place_id': 94207, 'anno_attribute_id': 3, 'content': 'windows-1252', 'flags': 0, 'expiration': 4, 'type': 3, 'dateAdded': 1782107233428000, 'lastModified': 1782107233428000}
```
Not sure what the "windows-1252" stuff is. It's an encoding.

Let's look for tags next. I don't think I've used any, but I can add one to a bookmark to test.

It's not super relevant, but in researching this I came across [sbs20/firefox-tags](https://github.com/sbs20/firefox-tags) which reorganises bookmarks in a JSON export. Written using jQuery, it's a little bit nostalgic.

> Remember, it's not my fault.  
—*Sam Strachan, 2015*

I can't really find anything, so let's look through tables haphazardly.
```python
def dump(name):
    con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
    con.row_factory = sqlite3.Row
    cur = con.cursor()
    res = cur.execute(f"SELECT * FROM {name}")
    for i, s in enumerate(res):
        print(dict(s))
        if i > 50:
            break
    con.close()
```

`dump('moz_origins')` sample:
```python
{'id': 1, 'prefix': 'https://', 'host': 'www.mozilla.org', 'frecency': 1, 'recalc_frecency': 0, 'alt_frecency': None, 'recalc_alt_frecency': 1, 'block_until_ms': None, 'block_pages_until_ms': None}
{'id': 2, 'prefix': 'https://', 'host': 'support.mozilla.org', 'frecency': 1, 'recalc_frecency': 0, 'alt_frecency': None, 'recalc_alt_frecency': 1, 'block_until_ms': None, 'block_pages_until_ms': None}
{'id': 3, 'prefix': 'https://', 'host': 'www.google.com', 'frecency': 1889060, 'recalc_frecency': 0, 'alt_frecency': None, 'recalc_alt_frecency': 1, 'block_until_ms': None, 'block_pages_until_ms': None}
{'id': 4, 'prefix': 'https://', 'host': 'www.box-look.org', 'frecency': 1, 'recalc_frecency': 0, 'alt_frecency': None, 'recalc_alt_frecency': 1, 'block_until_ms': None, 'block_pages_until_ms': None}
```

`dump('moz_places_extra')` : empty

`dump('moz_historyvisits_extra')` : empty

`dump('moz_inputhistory')` sample:
```python
{'place_id': 72804, 'input': 'braille', 'use_count': 0.17578766331789694}
{'place_id': 75351, 'input': 'defen', 'use_count': 0.14144876007543802}
{'place_id': 75357, 'input': 'int', 'use_count': 0.3828327635885509}
{'place_id': 76814, 'input': 'help', 'use_count': 0.40799641456977426}
{'place_id': 76980, 'input': 'debian', 'use_count': 0.12979048789711933}
```

`dump('moz_bookmarks_deleted')` : empty

`dump('moz_anno_attributes')` only 3 rows:
```python
{'id': 1, 'name': 'downloads/destinationFileURI'}
{'id': 2, 'name': 'downloads/metaData'}
{'id': 3, 'name': 'URIProperties/characterSet'}
```

`dump('moz_items_annos')` : empty

`dump('moz_meta')` only 3 rows:
```python
{'key': 'bookmarks/lastusedfolders', 'value': 'data:application/json;base64,WyJGOEhMNXF1SkRkTi0iLCJOMVVFd19Wc29nQkoiLCJYdHhJREFWRDMyTmgiLCJDcnFZT0JPeDRqVWgiLCJUMWkwV1JVMUlBMmgiLCJ3NGt5MzE4dEFzWEsiLCIwaVIxZUdCb1hxRUkiXQ=='}
{'key': 'origin_frecency_threshold', 'value': 20821}
{'key': 'origins_frecency_last_decay_timestamp', 'value': 1782230813795.0}
```

`dump('moz_places_metadata')` sample:
```python
{'id': 29120, 'place_id': 32887, 'referrer_place_id': None, 'created_at': 1751281967079, 'updated_at': 1751479743807, 'total_view_time': 13678, 'typing_time': 0, 'key_presses': 0, 'scrolling_time': 7137, 'scrolling_distance': 2017, 'document_type': 0, 'search_query_id': None}
{'id': 29127, 'place_id': 31831, 'referrer_place_id': None, 'created_at': 1751284119477, 'updated_at': 1751458966548, 'total_view_time': 1186, 'typing_time': 0, 'key_presses': 0, 'scrolling_time': 0, 'scrolling_distance': 0, 'document_type': 0, 'search_query_id': None}
{'id': 29223, 'place_id': 6601, 'referrer_place_id': None, 'created_at': 1751293279761, 'updated_at': 1751449705268, 'total_view_time': 4033, 'typing_time': 0, 'key_presses': 0, 'scrolling_time': 0, 'scrolling_distance': 0, 'document_type': 0, 'search_query_id': None}
{'id': 29224, 'place_id': 31246, 'referrer_place_id': None, 'created_at': 1751293710877, 'updated_at': 1751453060765, 'total_view_time': 11470, 'typing_time': 0, 'key_presses': 0, 'scrolling_time': 4923, 'scrolling_distance': 3543, 'document_type': 0, 'search_query_id': None}
{'id': 29233, 'place_id': 31141, 'referrer_place_id': 31140, 'created_at': 1751295120755, 'updated_at': 1751449700935, 'total_view_time': 23099, 'typing_time': 0, 'key_presses': 0, 'scrolling_time': 5031, 'scrolling_distance': 1954, 'document_type': 0, 'search_query_id': None}
```
I'm not sure what's the purpose of saving that kind of data.

`dump('moz_places_metadata_search_queries')` : empty

`dump('moz_previews_tombstones')` : empty

`dump('moz_places_url_hashindex')` : "no such table"

`dump('moz_places_hostindex')` : "no such table"

`dump('moz_places_visitcount')` : "no such table"

`dump('moz_places_frecencyindex')` : "no such table"

`dump('moz_places_lastvisitdateindex')` : "no such table"

`dump('moz_places_guid_uniqueindex')` : "no such table"

`dump('moz_places_originidindex')` : "no such table"

`dump('moz_places_altfrecencyindex')` : "no such table"

`dump('moz_historyvisits_fromindex')` : "no such table"

`dump('moz_historyvisits_dateindex')` : "no such table"

`dump('moz_bookmarks_itemindex')` : "no such table"

`dump('moz_bookmarks_parentindex')` : "no such table"

`dump('moz_bookmarks_itemlastmodifiedindex')` : "no such table"

`dump('moz_bookmarks_dateaddedindex')` : "no such table"

`dump('moz_bookmarks_guid_uniqueindex')` : "no such table"

`dump('moz_keywords_placepostdata_uniqueindex')` : "no such table"

`dump('moz_annos_placeattributeindex')` : "no such table"

`dump('moz_items_annos_itemattributeindex')` : "no such table"

`dump('moz_places_metadata_placescreated_uniqueindex')` : "no such table"

`dump('moz_places_metadata_referrerindex')` : "no such table"

The rest are `newtab_*` for tracking news stories they put there and shortcuts interaction, I don't use this and it doesn't interest me.

Maybe search for the tag like a strings search through a binary. [synthesizerpatel mentions](https://stackoverflow.com/a/21844624/18396947) the Python builtin module [mmap](https://docs.python.org/3/library/mmap.html).
```python
from mmap import mmap

with open(FILE, "r+b") as f:
    print(mmap(f.fileno(), 0).find(b'PLU5TAGTEST2026'))
```
It prints 59147026, and shockingly quickly.
Let's try to print what's around it.
```python
with open(FILE, "r+b") as f:
    mm = mmap(f.fileno(), 0)
    p = 59147026
    print(mm[p-500:p+500])
```
I see in the results something I had written just today in one of my bookmarks. I reckon then that this is in `moz_bookmarks` right at the end. Let's print it all without limiting it to the first 50:
```python
con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
con.row_factory = sqlite3.Row
cur = con.cursor()
res = cur.execute("SELECT * FROM moz_bookmarks")
for s in res:
    print(dict(s))
con.close()
```
Last four:
```python
{'id': 3475, 'type': 1, 'fk': 95593, 'parent': 625, 'position': 2334, 'title': "lien direct à l'image mozilla firefox participer | volunteer.a75736e2c2871646.png (PNG Image, 800\xa0×\xa0763 pixels) – Scaled (82%)", 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782837905156000, 'lastModified': 1782837905156000, 'guid': 'IoP62ifv3xLO', 'syncStatus': 1, 'syncChangeCounter': 1}
{'id': 3478, 'type': 2, 'fk': None, 'parent': 4, 'position': 0, 'title': 'PLU5TAGTEST2026', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782842634017000, 'lastModified': 1782842634018000, 'guid': 'MpPLY7-JtpbP', 'syncStatus': 1, 'syncChangeCounter': 2}
{'id': 3479, 'type': 1, 'fk': 63977, 'parent': 3478, 'position': 0, 'title': None, 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782842634018000, 'lastModified': 1782842634018000, 'guid': 'T11TJ6Nl9xuY', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3480, 'type': 1, 'fk': 39273, 'parent': 625, 'position': 2335, 'title': 'au: Last Morning Watch VI. - YouTube', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782845570899000, 'lastModified': 1782845576060000, 'guid': 'DJvMHfRY-t-3', 'syncStatus': 1, 'syncChangeCounter': 2}
```
The tag was added as if it's its own bookmark, when it was in fact a tag added on an old bookmark which we don't see here.

Let's add that same tag on the last bookmark ("au: Last Morning Watch VI. - YouTube", [link](https://www.youtube.com/watch?v=dfc19WYJt4g) if you're curious; it's a song) and see if it changes at all.

These are the last four now:
```python
{'id': 3478, 'type': 2, 'fk': None, 'parent': 4, 'position': 0, 'title': 'PLU5TAGTEST2026', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782842634017000, 'lastModified': 1782848806844000, 'guid': 'MpPLY7-JtpbP', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3479, 'type': 1, 'fk': 63977, 'parent': 3478, 'position': 0, 'title': None, 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782842634018000, 'lastModified': 1782842634018000, 'guid': 'T11TJ6Nl9xuY', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3480, 'type': 1, 'fk': 39273, 'parent': 625, 'position': 2335, 'title': 'au: Last Morning Watch VI. - YouTube', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782845570899000, 'lastModified': 1782845576060000, 'guid': 'DJvMHfRY-t-3', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3481, 'type': 1, 'fk': 39273, 'parent': 3478, 'position': 1, 'title': None, 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782848806844000, 'lastModified': 1782848806844000, 'guid': '72C_rTIwf1aq', 'syncStatus': 1, 'syncChangeCounter': 2}
```
3479 and 3481 are empty entries which both have 3478 as parent; the tag. And the tag has parent 4:
```python
{'id': 4, 'type': 2, 'fk': None, 'parent': 1, 'position': 2, 'title': 'tags', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1782842634029000, 'guid': 'tags________', 'syncStatus': 1, 'syncChangeCounter': 4}
```
[and also type 2, but I'm not sure if that's the only thing]

The bookmark itself (3480) before being tagged:
```python
{'id': 3480, 'type': 1, 'fk': 39273, 'parent': 625, 'position': 2335, 'title': 'au: Last Morning Watch VI. - YouTube', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782845570899000, 'lastModified': 1782845576060000, 'guid': 'DJvMHfRY-t-3', 'syncStatus': 1, 'syncChangeCounter': 2}
```
After:
```python
{'id': 3480, 'type': 1, 'fk': 39273, 'parent': 625, 'position': 2335, 'title': 'au: Last Morning Watch VI. - YouTube', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782845570899000, 'lastModified': 1782845576060000, 'guid': 'DJvMHfRY-t-3', 'syncStatus': 1, 'syncChangeCounter': 3}
```
Nothing changed other than syncChangeCounter.

Both the bookmark and the empty entry that was added after it after I added the tag have fk 39273.

[A gist by Ole Jørgen Brønner](https://gist.github.com/olejorgenb/9418bef65c65cd1f489557cfc08dde96) links to some resources, notably [Places.sqlite.schema3.pdf](https://wiki.mozilla.org/images/d/d5/Places.sqlite.schema3.pdf).

> Tags result in two new entries in
`moz_bookmarks`. The first one is the tag,
with parent=4 (tags), and fk=NULL. The
second entry follows the first one and has
the previous tag as its parent, and fk points
to the proper entry in `moz_places`.

Also useful for us:

> If you enter a description for a
bookmark, that description is saved under
`moz_items_annos.content`.

This document is over 15 years old by the way, but it seems like places.sqlite is structurally still the same.

(Author Jacques Boucher, 2008-11-03)

Idea for how to get a list of the bookmarks with associated tags:
- iterate `moz_bookmarks`
- ignore entries with title None (but actually a bookmark can have an empty title. maybe it would be '' rather than None in that case?)
- for each entry, look at all the other entries with the same fk, and if their parent's parent is id 4 (tags), the parent is a tag

A bit hacky

Let me save a bookmark with an empty title to test the theory that only non-bookmarks have title None

Nope
```python
{'id': 3482, 'type': 1, 'fk': 95653, 'parent': 625, 'position': 2336, 'title': None, 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782851237724000, 'lastModified': 1782851237724000, 'guid': 'btAvXn4hCII3', 'syncStatus': 1, 'syncChangeCounter': 1}
```

So how to tell real bookmarks apart from the tag references? Check the parent?

## Comparison with HTML export
Next what I'm going to do is export the bookmarks and check how the tags show up there.

I was about to say "they don't" because they don't show up on the web page, but they are there in the source:
```html
                <DT><A HREF="https://www.youtube.com/watch?v=dfc19WYJt4g" ADD_DATE="1782845570" LAST_MODIFIED="1782845576" ICON_URI="https://www.youtube.com/favicon.ico" ICON="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAAAtklEQVQ4T2NkoBAwUqifAW7AfwYGAaBhBlADHXAYfAAqfgGo8QOIDTYAqDkBSM0n0TWJQM0LGIGaQbaeJ1EzTLkhyIAAIG89mQYkggxoAGqux2rAgwdAzwF9dwDmdQxVjfgN+A80HgQ2bmRgKChgYAAZiAqINODgQYhLSDYApAFk84YNuIII7ALcgSgATBofwNGNC4ADkbJoBBlNUUKCuQ0tKYNcBUra6AAWn6hJGZ8nCclRnBsBzJ03KZWC+NsAAAAASUVORK5CYII=" TAGS="PLU5TAGTEST2026">au: Last Morning Watch VI. - YouTube</A>
```
Bookmarks with no name don't show up visibly as they are represented as an empty A tag (`<a></a>`):
```html
                <DT><A HREF="https://gist.github.com/olejorgenb/9418bef65c65cd1f489557cfc08dde96" ADD_DATE="1782851237" LAST_MODIFIED="1782851237" ICON_URI="https://gist.github.com/favicon.ico" ICON="data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAABAAAAAQCAYAAAAf8/9hAAACTElEQVQ4T41TTWjUQBTOTBKbFbe72STrrgeDF/FgZQ8KVrAsvaqoR6keili9etCLePXgyZNaFBR/8CCC4sWTYotUUHCvRWjRS8Qkm82qTdmdyfjNmi2xyGLgkTfv53vvfW+GKJu+crk8pWnaKULItBCiLt3QPeivGWNPOp3OQj6F5A6qbZr3VE07sxk0f+aMPQyiaBY2PgDPnKplWYsapZOjkoc+lqZLYRgeliADAFR+hMozfcYWGOeXut3ukVKpNKPrOpN+Soj73fevFgqFxYJhXNc1bQqdPEYnp0mlUjmoq+qSDOxzPt9uty9A3QZJIWtZVXn+BRGIv43481n8JEH1B8O5qaYd8Dzv46gxarVaU3D+RsZIPki1Wl0hQuziadrDCHviOF4dBYAtuVt0fRmzjwlCVknVcRIcDC7E1yAI3P8h0bbtLyohO4WirEuANQAUABADoJLNPgqH2pYVqZSOAyAhjuMsU0XZLTOwnhNYz4tR2ah+FNVfyhiw/FkC3FWEOBuEIS+PA5TSZhRF7/4FsqNYPMQM4xk6rmVbuCMBGr313qf4R9zDlf0JkWMkIGsaK30vA6E3cTeeOpZl54F7jO37c5Fsex5tzcEwQSltpGl6DjKLjaxIf71e358kyYcxXd/IR/s3fN+/uPEWKqb5FskTWGfDdd1uq9Xq5KoVQfY3BG/N9v8ct/Ck1POPSXZyE3zMwR6Dk734exlIbbvjDHS8yGthFF0Zgv8FII2WaV4Gkcf7/f4xzN2WNrwLU6f0vqD0FbZ0K8/Db0WP+i2+MwfDAAAAAElFTkSuQmCC"></A>
```

It's not clear to me how it keeps track of the location of each bookmark. It's flat other than indentation of the lines and number of DL and DT tags, which are just open with no closing tags. Actually the DL tags do close, and DT don't (not a single `</DT>` in the entire file).

## Bookmarks subcommand
I guess that what I will do for the interface is have separate subcommands; h for history and b for bookmarks.

But I'm not feeling super good about switching to that because it's more complicated and the order of arguments becomes counter-intuitive because there's the global arguments vs command arguments and the placement matters.

I think I will only make `--quiet` and `--dryrun` global and the others repeat for each command even though they're the same (`--save`, `--saveto`, `--overwrite`, `--update`, and also `--format` and `--sort`, though those last two I suppose will be different in the field options available for history vs bookmarks), otherwise you would not be able to give them at the end of the command and it would be confusing. Maybe there's another way to solve that problem but from what I remember from last time I looked into it there is no good way.

I thought to at least make the places.sqlite file a global argument but it makes for unintuitive order too, can't put the file before `--sort` for example:
```sh
$ places2csv h /home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite --sort t
usage: places2csv h [-h] [-f FORMAT] [--sort {V,Vr,v,vr,i,ir,u,ur,t,tr,d,dr}] [-s] [-t SAVE_TO] [-o] [-u]
places2csv h: error: argument --sort: expected one argument
$ places2csv h --sort t /home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite
```
and then while testing this noticed sorting by title is broken.
```sh
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
[..]
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
Traceback (most recent call last):
  File "/home/pm/pm/scripts/places2csv", line 386, in <module>
    main()
    ~~~~^^
  File "/home/pm/pm/scripts/places2csv", line 382, in main
    args.func(args)
    ~~~~~~~~~^^^^^^
  File "/home/pm/pm/scripts/places2csv", line 359, in cmd_h
    visits.sort(key=sort_key, reverse=reverse)
    ~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
TypeError: '<' not supported between instances of 'bool' and 'str'
```
I will deal with this [later](#sort-titles-bug).

Interface before:
```sh
usage: places2csv [-h] [-q] [-f FORMAT]
                  [--sort {V,Vr,v,vr,i,ir,u,ur,t,tr,d,dr}]
                  [-s] [-t SAVE_TO] [-o] [-u]
                  [--dryrun]
                  places_sqlite
```
after:
```sh
usage: places2csv [-h] [--dryrun] [-q] {h,b} ...

A utility script to extract history data (h) or extract
bookmarks data (b) from a places.sqlite file.

positional arguments:
  {h,b}
    h          [-h|--help] [-f FORMAT] [--sort SORT]
               [-s] [-t SAVE_TO] [-o] [-u]
               places_sqlite
    b          [-h|--help] [-f FORMAT] [--sort SORT]
               [-s] [-t SAVE_TO] [-o] [-u]
               places_sqlite

options:
  -h, --help   show this help message and exit
  --dryrun     Don't write to disk.
  -q, --quiet  Suppress messages in output.
```
Usage from `places2csv h -h`:
```sh
usage: places2csv h [-h] [-s] [-t SAVE_TO] [-o] [-u]
                    [-f FORMAT]
                    [--sort {V,Vr,v,vr,i,ir,u,ur,t,tr,d,dr}]
                    places_sqlite
```

## Base bookmarks implementation
- Collect the results of `SELECT * FROM moz_bookmarks` into an array
- Iterate over it taking the `fk` of each one to find the corresponding entry in `moz_places`

It appears that for some `moz_bookmarks` entries, `fk` is None. It seems to be folders.
```python
{'id': 1, 'type': 2, 'fk': None, 'parent': 0, 'position': 0, 'title': '', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1783005307411000, 'guid': 'root________', 'syncStatus': 1, 'syncChangeCounter': 1}
{'id': 2, 'type': 2, 'fk': None, 'parent': 1, 'position': 0, 'title': 'menu', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1734995982912000, 'guid': 'menu________', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3, 'type': 2, 'fk': None, 'parent': 1, 'position': 1, 'title': 'toolbar', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1783005307411000, 'guid': 'toolbar_____', 'syncStatus': 1, 'syncChangeCounter': 122}
{'id': 4, 'type': 2, 'fk': None, 'parent': 1, 'position': 2, 'title': 'tags', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1782842634029000, 'guid': 'tags________', 'syncStatus': 1, 'syncChangeCounter': 4}
{'id': 5, 'type': 2, 'fk': None, 'parent': 1, 'position': 3, 'title': 'unfiled', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1778813661389000, 'guid': 'unfiled_____', 'syncStatus': 1, 'syncChangeCounter': 9}
{'id': 6, 'type': 2, 'fk': None, 'parent': 1, 'position': 4, 'title': 'mobile', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982612000, 'lastModified': 1734995982903000, 'guid': 'mobile______', 'syncStatus': 1, 'syncChangeCounter': 2}
{'id': 7, 'type': 2, 'fk': None, 'parent': 2, 'position': 0, 'title': 'Mozilla Firefox', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982912000, 'lastModified': 1734995982912000, 'guid': 'MIaQgFa765sG', 'syncStatus': 0, 'syncChangeCounter': 1}
{'id': 19, 'type': 2, 'fk': None, 'parent': 78, 'position': 2, 'title': 'cpp', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1735054048372000, 'lastModified': 1737730573978000, 'guid': '2fAO49WNRh9m', 'syncStatus': 1, 'syncChangeCounter': 4}
```
1-7 are there by default, 19 was created by me.

possible fields?
id, fk, parent, position, title, url, add date / dateadded, last modified, tags, keyword, guid(?), path (parents hierarchy)

I can already foresee problems, like what would happen if the user updates a file which was generated with the bookmarks command with the history command or vice versa. But keeping them in the same csv file doesn't make sense either.

I didn't think it would be so complicated. I don't know how to manage this.

Let's start by only supporting id, title, and url format options to begin with and not worry about the others yet.
```python
BFORMATOPTIONS = {
    'i': 'id', 'u': 'url', 't': 'title',
}
DEFAULTBFORMATOPTIONS = 'iut'
```

Next, write a function similar to the `get_visit_information` function we had for history to extract the requested information from the dictionary of the entry in `moz_bookmarks` and `moz_places` (the URL is only present in the latter).
```python
def get_bookmark_information(bookmark_dict, place_dict, fmt):
    # type: (dict, dict, str) -> dict
    res = {}

    dispatch = {
        'i': lambda: bookmark_dict.get('id'),
        'u': lambda: place_dict.get('url'),
        't': lambda: bookmark_dict.get('title'),
    }

    for char in fmt:
        opt = BFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res
```
The dispatch thing is still pretty stupid here. And if the only reason `moz_places` is queried is for the URL, why do it ahead of time? It would make more sense to make that call within this function. Except if there are multiple fields that all need information from `moz_places`. But even then we could just check if one of those options is in the format and only query once.

Just carry on and don't worry about this for now.

```python
def cmd_b(args):
    if args.save or args.overwrite or args.update:
        raise NotImplementedError(
            "save, overwrite, update not implemented yet for bookmarks")

    fmt = args.format or DEFAULTBFORMATOPTIONS

    con = sqlite3.connect(f"file:{args.places_sqlite}?mode=ro", uri=True)
    con.text_factory = lambda data: str(data, errors="surrogateescape")
    con.row_factory = sqlite3.Row
    cur = con.cursor()

    query = "SELECT * FROM moz_bookmarks"
    res = cur.execute(query)
    entries = [dict(s) for s in res]

    # Get place for each bookmark
    bookmarks = []
    for entry in entries:
        if not entry['fk']:
            continue
        query = ("SELECT * FROM moz_places "
                 f'WHERE id = {entry['fk']}')
        cross = cur.execute(query)
        for s in cross:
            bookmarks.append(get_bookmark_information(entry, dict(s), fmt))

    con.close()

    def sort_key(item):
        k = BFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data, can't sort by it")
            data_to_sort_by = False
        return data_to_sort_by

    if args.sort:
        reverse = True if len(args.sort) > 1 and args.sort[1] == 'r' else False
        bookmarks.sort(key=sort_key, reverse=reverse)

    dump = export_csv(bookmarks)
    print(dump)
```
It works nicely. And we could easily add saving and overwriting, but the hard part is updating.

## Bookmarks update
With history we do it by dates but we don't have a date field yet.

Actually why even use dates when IDs exist? If you look at [the section where I implemented update for history](#save-update-interface), I did think of IDs first, but dismissed it because there could be "new visits for old IDs". I was talking about the IDs in `moz_places`. But the IDs in `moz_historyvisits` could have been used. If we had an option for them in FORMATOPTIONS, that is, and we don't. I guess that should be added actually. And not sure `moz_places` IDs should be in `moz_places`. I found them kind of useful because they show you if it's the first visit for that site or not based on whether the ID incremented, and enable you to easily see all the visits for the same site.

So to start let's make the update functionality based on the ID.

Or not. There is a more fundamental problem. Bookmarks can be changed. We can't just add the new rows ignoring existing ones like we do for history.

There does exist a field `lastModified`. We could add this to our format options and if it is present then update by querying only entries after the last `lastModified`.

So let's first add the date fields:
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch',
    'M': 'modified', 'm': 'modified epoch',
    'i': 'id', 'u': 'url', 't': 'title',
}
DEFAULTBFORMATOPTIONS = 'iutaAmM'
```
Add to `get_bookmark_information`:
```python
    a = bookmark_dict.get('dateAdded')
    m = bookmark_dict.get('lastModified')

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(a) if a else None,
        'm': lambda: m,
        'M': lambda: readable_date_from_epoch(m) if m else None,
        'i': lambda: bookmark_dict.get('id'),
        'u': lambda: place_dict.get('url'),
        't': lambda: bookmark_dict.get('title'),
    }
```

And now for updating we can use the same logic as we already use for history, but with checking m/M instead of v/V.
```python
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
                msg('existing is empty')
            one = existing[0]
            M = one.get(BFORMATOPTIONS['M'])
            m = one.get(BFORMATOPTIONS['m'])
            # Check format options are all there
            one['lastModified'] = m or epoch_from_readable_date(M)
            test = get_bookmark_information(existing[0], existing[0], fmt)
            if None in test.values():
                msg('Existing data does not contain all format '
                    f'options, so we have to query everything. {test} | {one}')
            # Check we have dates
            elif not m and not M:
                msg('Existing data has no modification dates. Without dates '
                    'in the data there is no reference point, we have to '
                    'query everything.')
            else:
                # Sort data in order to get the most recent date in it
                if m:
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['m']])
                else:
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['M']])
                m = existing[-1].get(BFORMATOPTIONS['m'])
                if not m:
                    M = existing[-1].get(BFORMATOPTIONS['M'])
                    m = epoch_from_readable_date(M)
                update_after = m
                msg('Updating bookmarks after '
                    f'{m} ({readable_date_from_epoch(m)})')
```

Now we have the `update_after` date, how to actually update? With history we query for only entries after it and then merge with existing data, but in our case there may be bookmarks that are already in existing data.

First modify the query so that the only bookmarks we get were ones that were updated:
```python
    query = "SELECT * FROM moz_bookmarks"
    query += f' WHERE lastModified > {update_after}' if update_after else ''
```

For history, new visits are merged with existing data:
```python
    visits = merge(existing, visits, fmt) if update_after else visits
```
where merge does:
```python
def merge(existing, new, fmt):
    # type: (list, list, str) -> list
    # Merge visits with existing but only the fields in fmt as there
    # may be fields in existing that are not in current fmt
    fields = [HFORMATOPTIONS[c] for c in fmt]
    return [{k: d[k] for k in fields} for d in existing] + new
```

For bookmarks we need something similar, but to also replace entries in the existing data with their new version in the new data. The problem is we can't know for sure two entries are the same one unless there are IDs in the data. It could also be devined with creation date as I suppose it's very unlikely to have several bookmarks created at the exact same time down to the microsecond, but I'm not comfortable relying on that, it can maybe happen if they were created using some sort of automation.

What I'll do then is also add a check that there are IDs in the data, and add to the update help string that IDs and modified dates are required for update.
```python
            id = one.get(BFORMATOPTIONS['i'])
            # Check format options are all there
            one['lastModified'] = m or epoch_from_readable_date(M)
            one['dateAdded'] = m or epoch_from_readable_date(M)
            # ^ (not correct but it's just to give it some date)
            test = get_bookmark_information(existing[0], existing[0], fmt)
            if None in test.values():
                msg('Existing data does not contain all format '
                    f'options, so we have to query everything. {test} | {one}')
            # Check we have dates
            elif not m and not M:
                msg('Existing data has no modification dates. Without dates '
                    'in the data there is no reference point, we have to '
                    'query everything.')
            # Check we have IDs
            elif not id:
                msg('Existing data has no IDs. Without IDs in the data'
                    'there is no way to know which bookmarks were modified.')
```
I also don't like the fact we currently proceed even if the fields needed are missing, which will essentially overwrite, because the existing data is not used in that case. I guess I will deal with this [later](#update-safeguard).

Merge with existing after `con.close()`:
```python
bookmarks = bmerge(existing, bookmarks, fmt) if update_after else bookmarks
```
And will rename the existing `merge` function to `hmerge`.

In `bmerge`, in order to make sure we only have one entry per ID, we can create a dictionary where the keys are the IDs and the objects are the entries.
```python
def bmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    """Create dictionary from existing and new with only the fields in fmt
    and no duplicate IDs
    """
    fields = [BFORMATOPTIONS[c] for c in fmt]
    by_id = {d['id']: {k: d[k] for k in fields} for d in existing}
    for d in new:
        by_id[d['id']] = d
    return list(by_id.values())
```

It breaks on `readable_date_from_epoch` because the epoch dates in the existing data are imported as strings. Let's add the new epoch date fields to `INTFIELDS`:
```python
INTFIELDS = (HFORMATOPTIONS['v'], HFORMATOPTIONS['i'],
             BFORMATOPTIONS['m'], BFORMATOPTIONS['a'])
```
My `parse_csv` function turns those fields into ints:
```python
def parse_csv(s):
    # type: str -> list
    res = []
    rows = list(csv.DictReader(s.splitlines()))
    keys = list(rows[0].keys())
    for row in rows:
        r = {}
        for field in keys:
            r[field] = int(row[field]) if field in INTFIELDS else row[field]
        res.append(r)
    return res
```

## Update safeguard
update safeguard -- fail if columns are present that are not in the options. this will also avoid accidentally updating history with bookmarks command and vice versa.
[for both bookmarks and history]

or simply modify the field checks we already have in order to update exit without doing the update. updating history requires visit date, which can't be there for bookmarks as they don't have visit dates. bookmarks require last modified date and IDs. And for all format options to be present, and the default format options are different for history and bookmarks, so that will also make it fail.

I simply added an `exit(1)` after each one of the `msg`s in the update checks, and changed the text in the messages to better reflect the fact we won't actually update. At this point I can imagine a `--force` argument to proceed with the update anyway, but I don't fancy adding it actually, you can run it with `--overwrite` instead of `--update` in that case.

Problem:
Where we do:
```python
            V = one.get(HFORMATOPTIONS['V'])
            v = one.get(HFORMATOPTIONS['v'])
            # Check format options are all there
            one['visit_date'] = v or epoch_from_readable_date(V)
```
in the history command, and:
```python
            one = existing[0]
            M = one.get(BFORMATOPTIONS['M'])
            m = one.get(BFORMATOPTIONS['m'])
            id = one.get(BFORMATOPTIONS['i'])
            # Check format options are all there
            one['lastModified'] = m or epoch_from_readable_date(M)
            one['dateAdded'] = m or epoch_from_readable_date(M)
            # ^ (not correct but it's just to give it some date)
```
in the bookmark command, this fails if there is not V or v, or not M or m. We should move the dates check earlier. The fields check should be the last one. Now that we exit anyway, there's no need to have an if/elif/else chain, so this is easy to reorder.

## Don't write if no change?
I thought about not writing to disk if there are no new entries when running `--update`, but I realised it's more complicated than this, since we have sort options and we know nothing and can't easily tell what the order is currently. After having merged and sorted, we could compare the result with existing and avoid writing if it would be identical, but is it worth it just to spare a disk write? It also means another potential source of bugs, so I'm leaning towards no.

But what I will do is add an informative `msg` that are no new entries when that is the case, as that's easy enough to do.

## Sort titles bug
For both history and bookmarks, some entries don't have titles. When I try to sort by title, it fails:
```sh
$ places2csv h /home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite -st posplaces.csv --update --sort t
places2csv: Adding visits after 1783114509326936 (2026-07-04 00:35:09.326936)
places2csv: Sorting by t
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
places2csv: title not in entry data, can't sort by it
Traceback (most recent call last):
  File "/home/pm/pm/scripts/places2csv", line 522, in <module>
    main()
    ~~~~^^
  File "/home/pm/pm/scripts/places2csv", line 518, in main
    args.func(args)
    ~~~~~~~~~^^^^^^
  File "/home/pm/pm/scripts/places2csv", line 349, in cmd_h
    visits.sort(key=sort_key, reverse=reverse)
    ~~~~~~~~~~~^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
TypeError: '<' not supported between instances of 'bool' and 'str'
```
If I change the fallback to an empty string instead of False:
```python
    def sort_key(item):
        k = HFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = ''
        return data_to_sort_by
```
```sh
$ places2csv h /home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite -st posplaces.csv --update --sort t
places2csv: Adding visits after 1783114509326936 (2026-07-04 00:35:09.326936)
places2csv: Sorting by t
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: title not in entry data
places2csv: Wrote to posplaces.csv
```
Then all the entries with None as the title appear at the top. Sample:
```text
id,url,title,visit epoch,visit
96123,https://www.wordreference.com/fren/faintly,None,1783126057145305,2026-07-04 03:47:37.145305
87344,https://www.wordreference.com/redirect/translation.aspx?w=masque&dict=fren,None,1783126476787497,2026-07-04 03:54:36.787497
96128,https://www.wordreference.com/redirect/translation.aspx?w=washing-up+liquid&dict=enfr,None,1783126491636035,2026-07-04 03:54:51.636035
96130,https://www.wordreference.com/enfr/vinaigre,None,1783126520691811,2026-07-04 03:55:20.691811
```

But I'm guessing this will fail as soon as we try to sort by something else that's not a string field? i.e. one of the int fields
```sh
$ places2csv h /home/pm/.mozilla/firefox/4afxl8gu.default-release/places.sqlite -st posplaces.csv --update --sort v
places2csv: Adding visits after 1783165125567421 (2026-07-04 14:38:45.567421)
places2csv: No new visits
places2csv: Sorting by v
places2csv: Wrote to posplaces.csv
```
Apparently not. But I guess it's just because there are no entries that are missing visit date, so the fallback is never used.

I guess a better fallback would be:
```python
            data_to_sort_by = 0 if k in INTFIELDS else ''
```

## Bookmarks path
I want to add a field to bookmarks to show location in the folders hierarchy. I guess we can work this out by going up through `parent` until it's None?

Let's try to work out the path for one bookmark manually. The Fjellström bookmark was from earlier ("au: Last Morning Watch VI. - YouTube") was id 3480
```python
def q(query):
    con = sqlite3.connect(f"file:{FILE}?mode=ro", uri=True)
    con.row_factory = sqlite3.Row
    cur = con.cursor()
    res = cur.execute(query)
    for s in res:
        print(dict(s))
    con.close()
```

`q("SELECT * FROM moz_bookmarks WHERE id = 3480")`
```python
{'id': 3480, 'type': 1, 'fk': 39273, 'parent': 625, 'position': 2335, 'title': 'au: Last Morning Watch VI. - YouTube', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782845570899000, 'lastModified': 1782845576060000, 'guid': 'DJvMHfRY-t-3', 'syncStatus': 1, 'syncChangeCounter': 3}
```

`q("SELECT * FROM moz_bookmarks WHERE id = 625")`
```python
{'id': 625, 'type': 2, 'fk': None, 'parent': 73, 'position': 10, 'title': 'cc', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1748930864470000, 'lastModified': 1783167947146000, 'guid': 'N1UEw_VsogBJ', 'syncStatus': 1, 'syncChangeCounter': 2354}
```

`q("SELECT * FROM moz_bookmarks WHERE id = 73")`
```python
{'id': 73, 'type': 2, 'fk': None, 'parent': 3, 'position': 17, 'title': 'ref', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1735986134566000, 'lastModified': 1783167947146000, 'guid': 'F8HL5quJDdN-', 'syncStatus': 1, 'syncChangeCounter': 39}
```

`q("SELECT * FROM moz_bookmarks WHERE id = 3")`
```python
{'id': 3, 'type': 2, 'fk': None, 'parent': 1, 'position': 1, 'title': 'toolbar', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1783167947146000, 'guid': 'toolbar_____', 'syncStatus': 1, 'syncChangeCounter': 122}
```

`q("SELECT * FROM moz_bookmarks WHERE id = 1")`
```python
{'id': 1, 'type': 2, 'fk': None, 'parent': 0, 'position': 0, 'title': '', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1734995982604000, 'lastModified': 1783167947146000, 'guid': 'root________', 'syncStatus': 1, 'syncChangeCounter': 1}
```

And there is no id 0, so this is the end. Though we can probably stop at 1 because everything will be under root, I'm guessing.

So the path is toolbar/ref/cc, which I can confirm is correct.

A function to give us this:
```python
def get_bookmark_path(cur, bookmark):
    # type: (sqlite3.Cursor, dict) -> str
    parents = []
    p_id = bookmark.get('parent', 0)
    while p_id > 1:
        cur.execute(f"SELECT * FROM moz_bookmarks WHERE id = {p_id}")
        parent = cur.fetchone()
        if not parent:
            msg(f'Parent {p_id} not found')
            return '?'
        parent = dict(parent)
        parents.append(parent.get('title', '?'))
        p_id = parent.get('parent', 0)
    return '/'.join(reversed(parents))
```

Add path to format options:
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch',
    'M': 'modified', 'm': 'modified epoch',
    'i': 'id', 'u': 'url', 't': 'title', 'p': 'path',
}
DEFAULTBFORMATOPTIONS = 'iutaAmMp
```

Modify `get_bookmark_information` to handle path, and also take cur:
```python
def get_bookmark_information(cur, bookmark_dict, place_dict, fmt):
    # type: (sqlite3.Cursor, dict, dict, str) -> dict
    res = {}

    a = bookmark_dict.get('dateAdded')
    m = bookmark_dict.get('lastModified')

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(a) if a else None,
        'm': lambda: m,
        'M': lambda: readable_date_from_epoch(m) if m else None,
        'i': lambda: bookmark_dict.get('id'),
        'u': lambda: place_dict.get('url'),
        't': lambda: bookmark_dict.get('title'),
        'p': lambda: get_bookmark_path(cur, bookmark_dict),
    }

    for char in fmt:
        opt = BFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res
```
Now we'll finally get some benefit from doing the dispatch like this, given that it's extra queries that are not necessary when p isn't in the format options.

Modify the call in `cmd_b` to pass in cur:
```python
            bookmarks.append(
                get_bookmark_information(cur, entry, dict(s), fmt))
```

Problem: We also use `get_bookmark_information` to test which fields we have on the existing data when updating:
```python
test = get_bookmark_information(existing[0], existing[0], fmt)
```
and we can't pass cur here.

## Better way to check fields on existing data
Which brings light to the fact this method of verifying the existence of fields is rather stupid. Surely we should check the columns? I'm not sure why I did it this way to begin with.

`existing[0]`:
```python
{'id': 8, 'url': 'https://support.mozilla.org/products/firefox', 'title': 'Get Help', 'added epoch': 1734995982912000, 'added': '2024-12-24 01:19:42.912000', 'modified epoch': 1734995982912000, 'modified': '2024-12-24 01:19:42.912000', 'path': 'menu/Mozilla Firefox'}
```

Instead of this:
```python
            # Check format options are all there
            one['lastModified'] = m or epoch_from_readable_date(M)
            one['dateAdded'] = m or epoch_from_readable_date(M)
            # ^ (not correct but it's just to give it some date)
            test = get_bookmark_information(0, existing[0], existing[0], fmt)
            if None in test.values():
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {test} | {one}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
```
Could use a function that checks the corresponding key for each character in fmt is present:
```python
def bhasfields(d, fmt):
    # type: (dict, str) -> bool
    for c in fmt:
        if BFORMATOPTIONS.get(c) not in d:
            return False
    return True
```
then:
```python
            # Check format options are all there
            if not bhasfields(one, fmt):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {fmt}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
```

And for history the same function just with `HFORMATOPTIONS` instead of `BFORMATOPTIONS`, named `hhasfields`.

## Bookmarks tags
As we've seen [earlier](#bookmarks-data), tags themselves and tags affiliation is stored as entries on `moz_bookmarks`, so our current bookmarks output is wrong. The "tag reference" looks like another bookmark for the same link:
```text
3480,https://www.youtube.com/watch?v=dfc19WYJt4g,au: Last Morning Watch VI. - YouTube,1782845570899000,2026-06-30 21:52:50.899000,1782845576060000,2026-06-30 21:52:56.060000,toolbar/ref/cc
3481,https://www.youtube.com/watch?v=dfc19WYJt4g,None,1782848806844000,2026-06-30 22:46:46.844000,1782848806844000,2026-06-30 22:46:46.844000,tags/PLU5TAGTEST2026
```
It's kind of interesting that it stores the date I added the tag. This is information we would lose if we convert this to a tags field on the same entry. Unless we store that information too in the field. For example `PLU5TAGTEST2026:1782848806844000` and separate it from other tags with some other symbol-- except that `:` itself could appear in the tag name. Regardless if we store the dates or not, this is a problem; how to store a list of tags in just one csv field?

There will probably be some characters that you can't input in tags that we could use as a separator, like `\x1e` (`\u001e`) which I used as a separator for rofi in [one of my other scripts](https://github.com/plu5/dotfiles/blob/main/pm/scripts/copy-dunst-history).

I'm kind of worried about the "tag reference" entries in `moz_bookmarks`, because we can't easily detect that's what they are without extra queries. I'm thinking of just leaving them, and adding another column that would identify which bookmark they are tagging. Looking at 3481, I suppose that the path being "tags/PLU5TAGTEST2026" already identifies it as a tag reference.

This is all the data on 3480 and 3481:
```python
{'id': 3480, 'type': 1, 'fk': 39273, 'parent': 625, 'position': 2335, 'title': 'au: Last Morning Watch VI. - YouTube', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782845570899000, 'lastModified': 1782845576060000, 'guid': 'DJvMHfRY-t-3', 'syncStatus': 1, 'syncChangeCounter': 3}
{'id': 3481, 'type': 1, 'fk': 39273, 'parent': 3478, 'position': 1, 'title': None, 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782848806844000, 'lastModified': 1782848806844000, 'guid': '72C_rTIwf1aq', 'syncStatus': 1, 'syncChangeCounter': 2}
```
How can you tell from 3481 that it's a tag reference for 3480? The fact they have the same fk is surely not sufficient, since we can have several bookmarks to the same "place".

I just created another bookmark to the same link, different title, didn't put any tags on it, and the PLU5TAGTEST2026 tag appeared. So it seems the tags are based on the "place", and different bookmarks to the same URL will have the same tags.

I think I am just going to leave these entries as is, even though they are not real bookmarks. Could add an option to filter them out if someone cares. Since it would increase processing time and remove information, I prefer not to for my use case. The path can indicate if it's a tag reference or a real bookmark, and the URL shows what "place" it's tagging.

But how does the tag itself appear? ID 3478. It's actually not there in the csv output. Data:
```python
{'id': 3478, 'type': 2, 'fk': None, 'parent': 4, 'position': 0, 'title': 'PLU5TAGTEST2026', 'keyword_id': None, 'folder_type': None, 'dateAdded': 1782842634017000, 'lastModified': 1782842634018000, 'guid': 'MpPLY7-JtpbP', 'syncStatus': 1, 'syncChangeCounter': 2}
```
I guess because it has no fk.

Folders will not be in the output either for the same reason.

For the list of tags field we're going to have an issue with updating; lastModified on a bookmark doesn't change when adding/removing a tag.

I'm not going to add that field then. There are too many problems. Instead, to find out the tags of something use the path field, look through the rows with a particular URL, and the ones whose path starts with `tags/[NAME]` are the tags on it, with [NAME] giving the name of each tag.

## Bookmarks keywords
Bookmarks have `keyword_id` which we can then look up in `moz_keywords`.

Rows in `moz_keywords` look like this:
```python
{'id': 1, 'keyword': 'edit', 'place_id': 58130, 'post_data': ''}
```

Adapting the `get_bookmark_path` function:
```python
def get_bookmark_keyword(cur, bookmark):
    # type: (sqlite3.Cursor, dict) -> str | None
    keyword_id = bookmark.get('keyword_id')
    if not keyword_id:
        return None
    cur.execute(f"SELECT * FROM moz_keywords WHERE id = {keyword_id}")
    keyword = cur.fetchone()
    if not keyword or not keyword.get('keyword'):
        msg(f'Keyword {keyword_id} not found')
        return '?'
    return keyword.get('keyword')
```
It's normal for `keyword_id` to be None, most bookmarks don't have a keyword. However, if it's not None, we should be able to find that ID in `moz_keywords`, if not then something is weird. Here I only alert about it via a message and make the keyword be '?' to notify something is weird, it's not critical so I chose to not throw an error.

Adding a field for it:
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch',
    'M': 'modified', 'm': 'modified epoch',
    'i': 'id', 'u': 'url', 't': 'title', 'p': 'path', 'k': 'keyword',
}
DEFAULTBFORMATOPTIONS = 'iutaAmMpk'
```

Adding it to dispatch in `get_bookmark_information`:
```python
    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(a) if a else None,
        'm': lambda: m,
        'M': lambda: readable_date_from_epoch(m) if m else None,
        'i': lambda: bookmark_dict.get('id'),
        'u': lambda: place_dict.get('url'),
        't': lambda: bookmark_dict.get('title'),
        'p': lambda: get_bookmark_path(cur, bookmark_dict),
        'k': lambda: get_bookmark_keyword(cur, bookmark_dict),
    }
```

Let's verify that changing the keyword on a bookmark changes lastModified. shit, it doesn't

and also `keyword_id` is always None ???

`q("SELECT * FROM moz_bookmarks WHERE keyword_id > 0")` -- no results

According to [Places.sqlite.schema3.pdf](https://wiki.mozilla.org/images/d/d5/Places.sqlite.schema3.pdf), `keyword_id` should be the ID in `moz_keywords`, I'm not hallucinating here. Admittedly this document is quite old, so maybe it's not used anymore?

The rows in `moz_keywords` contain `place_id`:
```python
{'id': 1, 'keyword': 'edit', 'place_id': 58130, 'post_data': ''}
```
Like tags, they could be now on each "place" instead of on a bookmark; meaning creating another bookmark to the same URL should give it the same keyword.

Created one and that is indeed the case.

That is unfortunate because it creates the same issue with tags for updates. And unlike tags, they don't appear in bookmarks at all, so there is no workaround.

But since keywords exist in their own table, there is a way we could deal with this. Go through that table (once), construct a `place_id` to `keyword` dictionary, and in `get_bookmark_information` get the keyword based on the place id. Then in bmerge we also need to access the dictionary and check if we need to change the keyword on any existing bookmarks. But I don't know how bmerge is going to access that. And we don't want to go through the keywords if `k` isn't in the format options. Ideally `get_bookmark_information` alone should handle this, but it doesn't get called for existing bookmarks when updating.

It's also not that easy because we don't know the fk for existing bookmarks. We can look at the URL, but this means another query.

I'm trying to think of a simpler and less coupled way of doing it.

I think don't concern bmerge with this, and instead before it gets called, and in fact before the loop over entries, do something like:
```python
keywords = {}
if 'k' in fmt:
    keywords = get_keyword_alist(cur)
    update_keywords(existing or [], keywords)
```
with the keywords alist also passed to `get_bookmark_information` (that way we can avoid doing the query more than once).

```python
def get_keyword_alist(cur):
    # type: (sqlite3.Cursor) -> dict[int, str]
    alist = {}
    res = cur.execute("SELECT * FROM moz_keywords")
    for s in res:
        k = dict(s)
        alist[k.get('place_id')] = k.get('keyword')
    return alist
```
```python
def update_keywords(existing, keywords):
    # type: (list, dict) -> None
    changed = False
    for b in existing:
        k = keywords.get(b.get('fk'))
        if k and k != b.get(BFORMATOPTIONS['k']):
            b[BFORMATOPTIONS['k']] = k
            changed = True
    if changed:
        msg('Keywords changed')
```

And `get_bookmark_keyword` should now be:
```python
def get_bookmark_keyword(bookmark, keywords):
    # type: (dict, dict) -> str | None
    return keywords.get(bookmark.get('fk'))
```
But we may as well at this point remove the function and put `keywords.get(bookmark_dict.get('fk'))` directly in dispatch.

And pass keywords to `get_bookmark_information`:
```python
            bookmarks.append(
                get_bookmark_information(cur, entry, dict(s), fmt, keywords))
```
I made it an optional argument defaulting to {}.

Wait, no, this doesn't work. I forgot that existing bookmarks don't have an fk field. Forgot we have to search by URL. We have to change the implementation of `update_keywords`.

And what if we don't have the URL field?

In update we check that id and last modified date are present, so we can use id to make a query to get the fk. For every single existing bookmark :-(

This is very unpleasant, especially as they are not expected to change much, so it's very expensive for something that very rarely derives benefit.

We could add field fk and force it to be present to update if k is in the format options. I don't like that either.

It would be cheaper to do the reverse; take the `place_id`s in `moz_keywords`, and get the corresponding bookmarks.

Another idea is for update to force URLs to be present if k is in format options, make the keywords alist be `url:keyword` instead of `id:keyword`, then for both adding and updating it's as simple as `keywords.get(bookmark_dict.get('url'))`. Forcing URLs to be present makes more sense, since we know keywords are associated with each URL and not each bookmark.

```python
            # If keywords in format options, check we have URLs
            if 'k' in fmt and BFORMATOPTIONS['u'] not in one:
                msg('Updating keywords requires URLs in the data.')
                exit(1)
```

Modify the functions to use URL instead of place id:
```python
def get_keyword_alist(cur):
    # type: (sqlite3.Cursor) -> dict[str, str]
    alist = {}
    res = cur.execute("SELECT * FROM moz_keywords")
    for s in res:
        k = dict(s)
        alist[k.get('url')] = k.get('keyword')
    return alist
```
Oh wait. Keywords don't have URLs. We have to do another query here for each keyword to get the URL, so it's not as simple as I thought.

So what to do? It seems like there is no way to avoid additional queries.

This is so much harder than I thought it would be. Bookmarks in general. I came into this just wanting to do history then was like let's add the option for bookmarks as well while we're at it seeing as it's in the same file, and now it's 4 days later [before that, history took 2] and I'm still at it T_T

There's no crying in places.sqlite. Just get something working, even if it's imperfect.

Since other than update there is no need for the keyword alist to have the URLs, let's keep it `id:keyword`. Then `update_keywords` can be passed `cur` and do the extra queries to find the URLs.

```python
def update_keywords(cur, existing, keywords):
    # type: (sqlite3.Cursor, dict, dict) -> None
    changed = False
    url2keyword = None
    for b in existing:
        k = None
        if 'fk' in b:           # Update based on fk
            k = keywords.get(b.get('fk'))
        else:                   # Update based on URL
            if url2keyword is None:
                url2keyword = {}
                for id, keyword in keywords.items():
                    cur.execute(f"SELECT * FROM moz_places WHERE id = {id}")
                    place = cur.fetchone()
                    if not place:
                        msg(f"update_keywords: Place {id} not found")
                        continue
                    place = dict(place)
                    url2keyword[place.get('url')] = keyword
            k = url2keyword.get(b.get('url'))

        if k and k != b.get(BFORMATOPTIONS['k']):
            b[BFORMATOPTIONS['k']] = k
            changed = True
    if changed:
        msg('Keywords changed')
```
I left fk in there as I plan to later add it as an optional field.

Which means that instead of requiring URLs we can permit them to not be present if there is an fk column:
```python
            # If keywords in format options, check we have URLs or fk
            if 'k' in fmt and not ('url' in one or 'fk' in one):
                msg('Updating keywords requires fk or URLs in the data.')
                exit(1)
```

And now pass `cur` to `update_keywords` as well:
```python
    keywords = {}
    if 'k' in fmt:
        keywords = get_keyword_alist(cur)
        update_keywords(cur, existing or {}, keywords)
```

That does the job. And it's instant for me, but I only have 14 keywords (one more than you will have seen in my list before, as I have added another for testing).

## Bookmarks fk
Let's add the fk field then. Which I don't plan in having in the default options.
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch',
    'M': 'modified', 'm': 'modified epoch',
    'i': 'id', 'u': 'url', 't': 'title', 'p': 'path', 'k': 'keyword',
    'f': 'fk',
}
```
Then simply add this line to dispatch in `get_bookmark_information`:
```python
        'f': lambda: bookmark_dict.get('fk'),
```

## Bookmarks position
Another field I want to add is `position`. I'll put it after `path`. I think it gives the bookmark's position in the folder it's in. Normally it would be in the order added, but not always, since the user can change the order by dragging bookmarks to another position.
```python
BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch',
    'M': 'modified', 'm': 'modified epoch',
    'i': 'id', 'u': 'url', 't': 'title',
    'p': 'path', 'o': 'position', 'k': 'keyword',
    'f': 'fk',
}
DEFAULTBFORMATOPTIONS = 'iutaAmMpok'
```
The addition to dispatch is trivial again:
```python
        'o': lambda: bookmark_dict.get('position'),
```

## History id
Currently, the id field in history is actually the place id (i.e. the equivalent of fk in bookmarks), and not the visit id. Maybe we should rename it to fk, and add a field for the real id. I'm more interested in fk, as for historyvisits the id is little more than the row number. In the default options then I will only have fk, with id being an optional field one could add.
```python
HFORMATOPTIONS = {
    'V': 'visit', 'v': 'visit epoch',
    'i': 'id', 'u': 'url', 't': 'title', 'd': 'description',
    'f': 'fk',
}
DEFAULTHFORMATOPTIONS = 'futvV'
```

We should add it to int fields too:
```python
INTFIELDS = (HFORMATOPTIONS['v'], HFORMATOPTIONS['i'], HFORMATOPTIONS['f'],
             BFORMATOPTIONS['m'], BFORMATOPTIONS['a'])
```
(no need to add it for BFORMATOPTIONS, such as we didn't add `i` in BFORMATOPTIONS, as the field names are the same)

The dispatch entries (in `get_visit_information` this time):
```python
        'f': lambda: entry_dict.get('id'),
        'i': lambda: visit_dict.get('id'),
```

## History update based on id
You will remember that for history, since visits can't change, all we do to update is add the new visits, based on the latest date in existing data. The same could be achieved using id -- not fk, but the actual id in the historyvisits table.

This will require some rethinking, as currently for both bookmarks and history what we do is use a variable called `update_after` that is set to None, which when updating is set to the epoch time of latest date in the data (visit date for history, last modification date for bookmarks) and used in the queries.

Maybe the simplest thing here is to rename it to `update_after_epoch` and add another variable `update_after_id`, and set only one of them, id preferentially if it's available, date otherwise. (and if neither is available, refuse to update)
```python
            existing = read_existing(save_to)
            if not len(existing):
                msg('Existing is empty. Either the file is empty or something '
                    'went wrong.' + S_UNDESIREDOVERWRITE)
                exit(1)
            one = existing[0]
            has_v = hhasfields(one, 'v')
            has_V = hhasfields(one, 'V')
            has_i = hhasfields(one, 'i')
            # Check we have IDs or dates
            if not (has_v or has_V or has_i):
                msg('Existing data has no visit dates or id. Without one of '
                    'these fields there is no reference point, so everything '
                    'would have to be queried from scratch.'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not hhasfields(one, fmt):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {fmt}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                if has_i:
                    # Sort data in order to get the most recent date in it
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['i']])
                    update_after_id = existing[-1].get(HFORMATOPTIONS['i'])
                    msg(f'Adding visits after id {update_after_id}')
                elif has_v:
                    # Sort data in order to get the most recent date in it
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['v']])
                    v = existing[-1].get(HFORMATOPTIONS['v'])
                    V = readable_date_from_epoch(v)
                    update_after_epoch = v
                    msg(f'Adding visits after epoch {v} ({V})')
                elif has_V:
                    # Sort data in order to get the most recent date in it
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['V']])
                    V = existing[-1].get(HFORMATOPTIONS['V'])
                    v = epoch_from_readable_date(V)
                    update_after_epoch = v
                    msg(f'Adding visits after epoch {v} ({V})')
                if v:
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['v']])
```

Looking at the queries, this actually isn't going to work, because we're querying `moz_places` and it has no idea about visit IDs. Maybe we should instead be querying visits, but then we would have to query the place for each one, and this would be less efficient since there are necessarily more visits than places (or equal).

Scratch that idea.

## Avoid SQL injection
I shouldn't have been using f-strings in queries, but instead pass the variables as arguments to execute:
```python
cur.execute("SELECT * FROM moz_places WHERE id = ?", (id,))
```

I will change all my queries to that format.

## Bookmark path bug
On one of my old places.sqlite files, `get_bookmark_path` fails with:
```python
  File "/home/pm/pm/scripts/places2csv", line 176, in get_bookmark_path
    return '/'.join(reversed(parents))
           ~~~~~~~~^^^^^^^^^^^^^^^^^^^
TypeError: sequence item 1: expected str instance, NoneType found
```

This is the function is question:
```python
def get_bookmark_path(cur, bookmark):
    # type: (sqlite3.Cursor, dict) -> str
    parents = []
    p_id = bookmark.get('parent', 0)
    while p_id > 1:
        cur.execute("SELECT * FROM moz_bookmarks WHERE id = ?", (p_id,))
        parent = cur.fetchone()
        if not parent:
            msg(f'Parent {p_id} not found')
            return '?'
        parent = dict(parent)
        parents.append(parent.get('title', '?'))
        p_id = parent.get('parent', 0)
    return '/'.join(reversed(parents))
```
and I don't see the "Parent id not found" message in the output.

If I print parents, this is what I see on the one that fails:
```python
['g', None, 'Bookmarks Toolbar']
```
I'm not sure how this is possible. It should have either not found or question mark. Or, wait, I guess the title itself is None? Like a folder with no name?

I will change `parents.append(parent.get('title', '?'))` to `parents.append(parent.get('title', '?') or '')`

## Script

```python
#!/usr/bin/env python3
# places2csv
# 2026-06-29 03:37
"""Extracts selected information from places.sqlite

Usage:
History default format and sort by visit date
    places2csv h ~/.mozilla/firefox/*.default-release/places.sqlite -st ~/outh.csv --sort v  # noqa: E501
Bookmarks default format and sort by added date
    places2csv b ~/.mozilla/firefox/*.default-release/places.sqlite -st ~/outb.csv --sort a  # noqa: E501
"""
import os
import sys
import csv
import sqlite3
import argparse
from datetime import datetime

PROG = 'places2csv'
DEFAULTSAVENAME = 'places.csv'
# Caution: If the date format is changed to less precision, there will
# be an issue with duplicates when updating data with no epoch field
DATEFORMAT = '%F %T.%f'
QUIET = False

HFORMATOPTIONS = {
    'V': 'visit', 'v': 'visit epoch',
    'i': 'id', 'u': 'url', 't': 'title', 'd': 'description',
    'f': 'fk',
}
DEFAULTHFORMATOPTIONS = 'futvV'

BFORMATOPTIONS = {
    'A': 'added', 'a': 'added epoch',
    'M': 'modified', 'm': 'modified epoch',
    'i': 'id', 'u': 'url', 't': 'title',
    'p': 'path', 'o': 'position', 'k': 'keyword',
    'f': 'fk',
}
DEFAULTBFORMATOPTIONS = 'iutaAmMpok'

INTFIELDS = (HFORMATOPTIONS['v'], HFORMATOPTIONS['i'], HFORMATOPTIONS['f'],
             BFORMATOPTIONS['m'], BFORMATOPTIONS['a'])

S_UNDESIREDOVERWRITE = """
Exiting to avoid undesired overwriting.
Run with --overwrite if you wish to overwrite.
"""

# Args that we need in several commands but can't make top-level due
# to argument-order issues
REPEATEDARGS = [
    (["places_sqlite"], {"help": """places.sqlite file."""}),
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
    "h":                        # H (history)
    {"desc": "extract history data", "usage": """[-f FORMAT] [--sort SORT]
[-s] [-t SAVE_TO] [-o] [-u] places_sqlite""",
     "args": [
         *REPEATEDARGS,
         (["-f", "--format"], {"default": DEFAULTHFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTHFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in HFORMATOPTIONS.items()])}."""}),
         (["--sort"], {"choices":
                       [x for k in HFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: vr -> sort by visit epoch, reverse order.)"""}),
         ]},
    "b":                        # B (bookmarks)
    {"desc": "extract bookmarks data", "usage": """[-f FORMAT] [--sort SORT]
[-s] [-t SAVE_TO] [-o] [-u] places_sqlite""",
     "args": [
         *REPEATEDARGS,
         (["-f", "--format"], {"default": DEFAULTBFORMATOPTIONS, "help": f"""
Which metadata fields to include and in which order. Default: {DEFAULTBFORMATOPTIONS}.
Options: {", ".join([f"{k} ({v})" for k, v in BFORMATOPTIONS.items()])}."""}),
         (["--sort"], {"choices":
                       [x for k in BFORMATOPTIONS for x in (k, k + 'r')],
                       "help": """Sort by a given metadata field.
Options are the same as for -f/--format, or followed by r for reverse order.
(Example: vr -> sort by visit epoch, reverse order.)"""}),
     ]},
}


def msg(text):
    # type: (str) -> None
    """Utility for logs/errors, could be replaced with proper logging"""
    if not QUIET:
        print(f'{PROG}: {text}')


def readable_date_from_epoch(e):
    """Takes epoch in microseconds"""
    # type: (int) -> str
    return datetime.fromtimestamp(e/1000000).strftime(DATEFORMAT)


def epoch_from_readable_date(d):
    """Returns epoch in microseconds"""
    # type: (str) -> int
    return int(datetime.fromisoformat(d).timestamp()*1000000)


def get_visit_information(entry_dict, visit_dict, fmt):
    # type: (dict, dict, str) -> dict
    res = {}

    v = visit_dict.get('visit_date')

    # This architecture is from metaf but doesn't make as much sense
    # here since our calls are not heavy. It would probably cheaper to
    # preset all of them instead of having the lambdas. But maybe in
    # future we would want to add a field that would be heavier.
    dispatch = {
        'v': lambda: v,  # or 0 instead of None?
        'V': lambda: readable_date_from_epoch(v) if v else None,
        'f': lambda: entry_dict.get('id'),
        'i': lambda: visit_dict.get('id'),
        'u': lambda: entry_dict.get('url'),
        't': lambda: entry_dict.get('title'),
        'd': lambda: entry_dict.get('description'),
    }
    # ^ not sure if fallbacks should be None or 0 / ''. I want to be
    # able to tell if it didn't exist in the dictionary so I'm leaving
    # None for now but I'm not sure how csv handles it.

    for char in fmt:
        opt = HFORMATOPTIONS.get(char)
        if not opt or char not in dispatch:
            continue
        res[opt] = dispatch[char]()

    return res


def get_bookmark_path(cur, bookmark):
    # type: (sqlite3.Cursor, dict) -> str
    parents = []
    p_id = bookmark.get('parent', 0)
    while p_id > 1:
        cur.execute("SELECT * FROM moz_bookmarks WHERE id = ?", (p_id,))
        parent = cur.fetchone()
        if not parent:
            msg(f'Parent {p_id} not found')
            return '?'
        parent = dict(parent)
        parents.append(parent.get('title', '?') or '')
        p_id = parent.get('parent', 0)
    return '/'.join(reversed(parents))


def get_keyword_alist(cur):
    # type: (sqlite3.Cursor) -> dict[int, str]
    alist = {}
    res = cur.execute("SELECT * FROM moz_keywords")
    for s in res:
        k = dict(s)
        alist[k.get('place_id')] = k.get('keyword')
    return alist


def update_keywords(cur, existing, keywords):
    # type: (sqlite3.Cursor, dict, dict) -> None
    changed = False
    url2keyword = None
    for b in existing:
        k = None
        if 'fk' in b:           # Update based on fk
            k = keywords.get(b.get('fk'))
        else:                   # Update based on URL
            if url2keyword is None:
                url2keyword = {}
                for id, keyword in keywords.items():
                    cur.execute("SELECT * FROM moz_places WHERE id = ?", (id,))
                    place = cur.fetchone()
                    if not place:
                        msg(f"update_keywords: Place {id} not found")
                        continue
                    place = dict(place)
                    url2keyword[place.get('url')] = keyword
            k = url2keyword.get(b.get('url'))

        if k and k != b.get(BFORMATOPTIONS['k']):
            b[BFORMATOPTIONS['k']] = k
            changed = True
    if changed:
        msg('Keywords changed')


def get_bookmark_information(cur, bookmark_dict, place_dict, fmt, keywords={}):
    # type: (sqlite3.Cursor, dict, dict, str, dict) -> dict
    res = {}

    a = bookmark_dict.get('dateAdded')
    m = bookmark_dict.get('lastModified')

    dispatch = {
        'a': lambda: a,
        'A': lambda: readable_date_from_epoch(a) if a else None,
        'm': lambda: m,
        'M': lambda: readable_date_from_epoch(m) if m else None,
        'i': lambda: bookmark_dict.get('id'),
        'u': lambda: place_dict.get('url'),
        't': lambda: bookmark_dict.get('title'),
        'p': lambda: get_bookmark_path(cur, bookmark_dict),
        'k': lambda: keywords.get(bookmark_dict.get('fk')),
        'f': lambda: bookmark_dict.get('fk'),
        'o': lambda: bookmark_dict.get('position'),
    }

    for char in fmt:
        opt = BFORMATOPTIONS.get(char)
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
            r[field] = int(row[field]) if field in INTFIELDS else row[field]
        res.append(r)
    return res


def read_existing(path):
    # type: str -> dict | None
    existing = None
    with open(path, "r") as f:
        existing = parse_csv(f.read())
    return existing


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


def hhasfields(d, fmt):
    # type: (dict, str) -> bool
    for c in fmt:
        if HFORMATOPTIONS.get(c) not in d:
            return False
    return True


def bhasfields(d, fmt):
    # type: (dict, str) -> bool
    for c in fmt:
        if BFORMATOPTIONS.get(c) not in d:
            return False
    return True


def hmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    # Merge visits with existing but only the fields in fmt as there
    # may be fields in existing that are not in current fmt
    fields = [HFORMATOPTIONS[c] for c in fmt]
    return [{k: d[k] for k in fields} for d in existing] + new


def bmerge(existing, new, fmt):
    # type: (list, list, str) -> list
    """Create dictionary from existing and new with only the fields in fmt
    and no duplicate IDs
    """
    fields = [BFORMATOPTIONS[c] for c in fmt]
    by_id = {d['id']: {k: d[k] for k in fields} for d in existing}
    for d in new:
        by_id[d['id']] = d
    return list(by_id.values())


def cmd_h(args):
    save_to = args.save_to or DEFAULTSAVENAME
    fmt = args.format or DEFAULTHFORMATOPTIONS

    update_after = None
    existing = None

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
            V = one.get(HFORMATOPTIONS['V'])
            v = one.get(HFORMATOPTIONS['v'])
            # Check we have dates
            if not v and not V:
                msg('Existing data has no visit dates. Without dates in the '
                    'data there is no reference point, so everything would '
                    'have to be queried from scratch.' + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check format options are all there
            if not hhasfields(one, fmt):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {fmt}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent date in it
                if v:
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['v']])
                else:
                    existing.sort(key=lambda r: r[HFORMATOPTIONS['V']])
                V = existing[-1].get(HFORMATOPTIONS['V'])
                v = existing[-1].get(HFORMATOPTIONS['v'])
                if not v:
                    v = epoch_from_readable_date(V)
                update_after = v
                msg(f'Adding visits after {v} ({readable_date_from_epoch(v)})')

    con = sqlite3.connect(f"file:{args.places_sqlite}?mode=ro", uri=True)
    con.text_factory = lambda data: str(data, errors="surrogateescape")
    con.row_factory = sqlite3.Row
    cur = con.cursor()

    query = "SELECT * FROM moz_places"
    query += f' WHERE last_visit_date > {update_after}' if update_after else ''
    res = cur.execute(query)
    entries = [dict(s) for s in res]

    # Get visits for each entry
    visits = []
    for entry in entries:
        query = "SELECT * FROM moz_historyvisits WHERE place_id = ?"
        query += f' AND visit_date > {update_after}' if update_after else ''
        cross = cur.execute(query, (entry['id'],))
        for s in cross:
            visits.append(get_visit_information(entry, dict(s), fmt))

    con.close()

    if not visits:
        msg("No new visits")

    visits = hmerge(existing, visits, fmt) if update_after else visits

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
        with open(save_to, open_as) as f:
            f.write(dump)
            msg(f'Wrote to {save_to}')
    else:
        print(dump)


def cmd_b(args):
    save_to = args.save_to or DEFAULTSAVENAME
    fmt = args.format or DEFAULTBFORMATOPTIONS

    update_after = None
    existing = None

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
            M = one.get(BFORMATOPTIONS['M'])
            m = one.get(BFORMATOPTIONS['m'])
            id = one.get(BFORMATOPTIONS['i'])
            # Check we have dates
            if not m and not M:
                msg('Existing data has no modification dates. Without dates '
                    'in the data there is no reference point, so everything '
                    'would have to be queried from scratch.'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            # Check we have IDs
            if not id:
                msg('Existing data has no IDs. Without IDs in the data'
                    'there is no way to know which bookmarks were modified.'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            # If keywords in format options, check we have URLs or fk
            if 'k' in fmt and not ('url' in one or 'fk' in one):
                msg('Updating keywords requires fk or URLs in the data.')
                exit(1)
            # Check format options are all there
            if not bhasfields(one, fmt):
                msg('Existing data does not contain all format '
                    'options, so everything would have to be queried from '
                    f'scratch. {one} | {fmt}'
                    + S_UNDESIREDOVERWRITE)
                exit(1)
            else:               # Proceed with the update
                # Sort data in order to get the most recent date in it
                if m:
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['m']])
                else:
                    existing.sort(key=lambda r: r[BFORMATOPTIONS['M']])
                m = existing[-1].get(BFORMATOPTIONS['m'])
                if not m:
                    M = existing[-1].get(BFORMATOPTIONS['M'])
                    m = epoch_from_readable_date(M)
                update_after = m
                msg('Updating bookmarks after '
                    f'{m} ({readable_date_from_epoch(m)})')

    con = sqlite3.connect(f"file:{args.places_sqlite}?mode=ro", uri=True)
    con.text_factory = lambda data: str(data, errors="surrogateescape")
    con.row_factory = sqlite3.Row
    cur = con.cursor()

    keywords = {}
    if 'k' in fmt:
        keywords = get_keyword_alist(cur)
        update_keywords(cur, existing or {}, keywords)

    query = "SELECT * FROM moz_bookmarks"
    query += f' WHERE lastModified > {update_after}' if update_after else ''
    res = cur.execute(query)
    entries = [dict(s) for s in res]

    # Get place for each bookmark
    bookmarks = []
    for entry in entries:
        if not entry['fk']:
            continue
        cross = cur.execute("SELECT * FROM moz_places WHERE id = ?",
                            (entry['fk'],))
        for s in cross:
            bookmarks.append(
                get_bookmark_information(cur, entry, dict(s), fmt, keywords))

    con.close()

    if not bookmarks:
        msg("No new bookmarks")

    bookmarks = bmerge(existing, bookmarks, fmt) if update_after else bookmarks

    def sort_key(item):
        k = BFORMATOPTIONS[args.sort[0]]
        data_to_sort_by = item.get(k, None)
        if data_to_sort_by is None:
            msg(f"{k} not in entry data")
            data_to_sort_by = 0 if k in INTFIELDS else ''
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
    if len(descs):
        descs[-1] = f"or {descs[-1]}"
    parser.description = f"A utility script to \
{' '.join(descs)} from a places.sqlite file."
    # Parse args / print help and quit if no args
    # (Primer https://stackoverflow.com/a/47440202/18396947)
    return parser.parse_args(sys.argv[1:] or ['--help'])


COMMANDS['h']['func'] = cmd_h
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

I will put it and future updates in [dotfiles/scripts/places2csv](https://github.com/plu5/dotfiles/blob/main/pm/scripts/places2csv).

{% include fin.html %}
