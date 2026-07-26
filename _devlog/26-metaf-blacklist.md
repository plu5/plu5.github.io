---
layout: post
title: 26 — metaf blacklist, breadth, depth
date: 2026-07-20 07:19
modified_date: 2026-07-22 05:36
categories: metaf python
lang: en
redirect_from: /devlog/26
---

## What
[metaf](https://github.com/plu5/dotfiles/blob/main/pm/scripts/metaf.py) is a script to get metadata of files in a location on disk. If the tree contains git repositories, there can be thousands of git object files which slows and pollutes the results. It really needs blacklisting functionality to omit certain things from the result.

I'm thinking of adding two different options, blacklist and recursion blacklist. Blacklist will exclude an entry that matches from the results, recursion blacklist will prevent recursion into a directory that matches.

Last time I worked on metaf: 2026-04-21 04:56. 3 months ago.

## CLI option
First let's add a blacklist option to the CLI. It needs to be an option to which you can pass one or more arguments. With argparse this can be achieved with [nargs](https://docs.python.org/3/library/argparse.html#nargs) `+`.
```python
    parser.add_argument(
        '--blacklist', nargs='+',
        help='Regexes matching names to exclude from the output.')
```

But should we be matching file names or paths? If it's paths, maybe it avoids having two different options. But I think it would make more sense to make it names because otherwise matching just the name may be painful / error-prone for the user which path separator to use in the regex.

I think we can still have just the one argument, as we can match it against both files and folders, and if a folder matches avoid recursing into it.

## Modify the loop
This is what our get-files-information loop looks like currently:
```python
    for root, subdirs, files in os.walk(parent_path):
        if include_subdirs:
            for f in subdirs:
                process_file_or_subdir(root, f, is_subdir=True)
        for f in files:
            process_file_or_subdir(root, f)
```

Where `process_file_or_subdir`:
```python
    def process_file_or_subdir(root, name, is_subdir=False):
        # type: (str, str, bool) -> None
        p = os.path.join(root, name)
        rel_p = os.path.relpath(p, parent_path)
        e = existing.get(rel_p) if existing else None
        if existing and not e:
            msg(f"File {p} is new")
        try:
            files_info_dict[rel_p] = get_file_information(p, fmt, e)
        except FileNotFoundError as e:
            if is_subdir:
                raise e
            else:
                msg(f'{p} not found - broken symlink? Exception: {e}'
                    '\nSkipping.')
```

We can't actually stop recursion with this for subdirs that match, can we?

I remember with `os.walk`, there is this pattern to avoid recursing:
```python
for root, subdirs, files in os.walk(path):
    for f in files:
        do_something_with_the_file(f)
    break  # do not recurse
```
so is there a way to avoid recursing but only for certain subdirs?

Before worrying about that, let's first implement the basic blacklist, where we just exclude names that match. Pass args.blacklist into the function. Even though it is a no-no in Python, I am defaulting it to an empty list, to avoid having to check its value before iterating over it. I think since we're not going to be modifying the list, there's not the problems we normally seek to avoid by defaulting objects to None. Then we can just add to the beginning of `process_file_or_subdir`:
```python
        for b in blacklist:
            if re.fullmatch(b, name):
                return
```

## Prevent recursion into matching subdirs
Although what we've done in the previous section works, I think we should move the blacklist handling outside of `process_file_or_subdir` and have it be in the loop. ~~Then we could avoid recursion by doing `continue` at the start of the `os.walk` loop if subdir matches.~~ I think that's false it's a loop per subdir is it? It's subdirs.

Each iteration we have root, subdirs, files. The subdirs and files are the directories and files present in the path `root`.

For example for a tree like this:
```asciiart
 ./
 │
 ├── resources/
 │   └── mecha.png
 ├── .gitignore
 ├── game.c
 ├── Makefile
 └── minshell.html
```
The `os.walk` iterations are:
```asciiart
 iteration | root        | subdirs       | files
 ----------+-------------+---------------+--------------
 0         | .           | ['resources'] | ['.gitignore', 'game.c',
           |             |               |  'Makefile', 'minshell.html']
 1         | ./resources | []            | ['mecha.png']
```
We could check the last part of root, and `continue` without doing anything if it matches a balcklist.

We could write a function like this:
```python
    def blacklisted(name, blacklist=[]):
        # type: (str, list[str]) -> bool
        for b in blacklist:
            if re.fullmatch(b, name):
                return True
        return False
```
and change the loop to:
```python
    for root, subdirs, files in os.walk(parent_path):
        if blacklisted(os.path.basename(root), blacklist):
            msg(f'Skipping blacklisted path {root}')
            continue
        if include_subdirs:
            for f in subdirs:
                if blacklisted(f, blacklist):
                    msg(f'Skipping blacklisted subdir {f}')
                    continue
                process_file_or_subdir(root, f, True)
        for f in files:
            if blacklisted(f, blacklist):
                msg(f'Skipping blacklisted file {f}')
                continue
            process_file_or_subdir(root, f, False)
```

But would this actually present recursing into this folder? What if resources had a subfolder?

```asciiart
 ./
 │
 └── resources/
     ├── test/
     │   └── hi
     └── mecha.png
```

Test:
```sh
$ metaf.py . -ax csv --blacklist resources
metaf: Skipping blacklisted subdir resources
metaf: Skipping blacklisted path ./resources
key,creation,creation epoch,modification,modification epoch
<<metaf:generated,2026-07-21 12:56:40
<<metaf:generated_epoch,1784627800.699831
resources/test/hi,2026-07-21 12:54:15,1784627655.3569984,2026-07-21 12:54:18,1784627658.979562
```

It's not very elegant, but I guess we have to check every folder in the path? :-/

Maybe we could be storing the paths we're skipping to a list, and use `os.path.commonpath` to skip also other paths that begin with the skipped paths. But then we would be adding and comparing against more paths than necessary.

Maybe in the subdirs loop, when encountering a blacklisted subdir add `os.path.join(root, subdir)` to a list of roots to skip. We're currently not entering that loop if `include_subdirs` is not True, so it's a little bit nonoptimal, but maybe this is negligeable.
```python
    roots_to_skip = []

    for root, subdirs, files in os.walk(parent_path):
        if root in roots_to_skip:
            msg(f'Skipping blacklisted path {root}')
            continue
        for f in subdirs:
            if blacklisted(f, blacklist):
                roots_to_skip.append(os.path.join(root, f))
                msg(f'Skipping blacklisted subdir {f}')
                continue
            if include_subdirs:
                process_file_or_subdir(root, f, True)
        for f in files:
            if blacklisted(f, blacklist):
                msg(f'Skipping blacklisted file {f}')
                continue
            process_file_or_subdir(root, f, False)
```

That doesn't work, I still get `resources/test/hi`, which is obvious in retrospect, as the root of it will be `./resources/test`, whereas our `roots_to_skip` will be just `['./resources']`.

Name it `prefixes_to_skip` instead of `roots_to_skip`, and use `os.path.commonpath` with the root against them
```python
        for prefix in prefixes_to_skip:
            if os.path.commonpath([root, prefix]) == prefix:
                msg(f'Skipping blacklisted path {root}')
                continue
```

It doesn't work. `os.path.commonpath(['./resources/test', './resources'])` gives just `resources` instead of `./resources`.

There's also another flaw there: we're inside another loop. `continue` will not continue the outer loop there, so it will not actually skip.

Don't know how to make `os.path.commonpath` work here. Keep in mind we could be given both relative and absolute paths, `root` is not necessarily going to be a relative path.

Instead of `os.path.commonpath`, we could literally just check the first n characters. I know there can be different path separators, but on the same machine it should be consistent.
```python
    blacklistedroots = []

    def startswith_blacklistedroot(root):
        # type: (str) -> bool
        for r in blacklistedroots:
            if root.startswith(r):
                return True
        return False

    for root, subdirs, files in os.walk(parent_path):
        if blacklisted(os.path.basename(root), blacklist):
            msg(f'Skipping blacklisted path {root}')
            blacklistedroots.append(root)
            continue
        if startswith_blacklistedroot(root):
            msg(f'Skipping blacklisted nested path {root}')
            continue
        if include_subdirs:
            for f in subdirs:
                if blacklisted(f, blacklist):
                    msg(f'Skipping blacklisted subdir {f}')
                    continue
                process_file_or_subdir(root, f, True)
        for f in files:
            if blacklisted(f, blacklist):
                msg(f'Skipping blacklisted file {f}')
                continue
            process_file_or_subdir(root, f, False)
```
To avoid polluting what used to be a simple loop, should maybe further excise the blacklist handling into functions. But in the case of subdirs and files I don't see how we can, or it will only save one line, because we need to have an if and continue in there at minimum.

And the problem with comparing strings:
```sh
metaf: Skipping blacklisted nested path ./reps/plover/.github/workflows
```
When the blacklist had just `.git`. `.github` would not be skipped, but a path that starts with `./reps/plover/.git` will be, due to the string comparison we're doing. We need to at least add a path separator at the end during the comparison. Is there a way to know which path separator is in use?

> `os.path.join(path, '')` will add the trailing slash if it's not already there.  
[Steven T. Snyder, 2013](https://stackoverflow.com/a/15010678/18396947)

```python
    def startswith_blacklistedroot(root):
        # type: (str) -> bool
        for r in blacklistedroots:
            if root.startswith(os.path.join(r, '')):
                return True
        return False
```

## Standard things to blacklist
- `\.git`
- `\.cache`
- `__pycache__`
- `__astcache`
- `shader_cache`
- `\.venv`
- `\.tox`
- `\.pytest_cache`
- `\.mypy_cache`
- `node_modules`

(and maybe `build`, but not sure about that one)

Maybe I should add a default blacklist? I don't know if anyone would ever want these things. Downside is adding some running time for having to check against all the items in the blacklist, but it doesn't increase time complexity.

## Still want a separate option
I forgot that the reason I wanted a separate blacklist and recursion blacklist options, as confusing as that would be interface-wise, is because it's useful to have .git folder in the output to see when the git repository was created for example, without wanting the things inside it.

Is there a way to do this without an ugly and confusing interface?

Maybe a flag "show excluded" or something which will make the blacklist only be used to prevent recursion, with the files/subdirs that match themselves still included. But this doesn't allow a lot of control.

I can't think of a better way than the two separate options I wanted at the start, `--blacklist` and `--blacklist-recursion`, despite the potential confusion (expectation by the user that an item in blacklist will not be recursed into).

```python
    parser.add_argument(
        '--blacklist-recursion', nargs='+', default=[],
        help='Regexes matching names of folders to not enter.')
```

It's easy to add, just take `args.blacklist_recursion` in an argument (which I called `rblacklist`), and pass that as the blacklist to the `blacklisted` function when checking the root.
```python
        if blacklisted(os.path.basename(root), rblacklist):
            msg(f'Skipping blacklisted path {root}')
            blacklistedroots.append(root)
            continue
```

I'm seeing some .git folders in the output but in most folders where I know I have .git it doesn't show up. Ah oups, I forgot to run with `-a` in my test (metaf doesn't include folders by default, unless you run with `--all`/`-a`).

[Permalink to current state of the script after the changes so far](https://github.com/plu5/dotfiles/blob/3382523db17f70854ffebcf1c589c7a34b348dfe/pm/scripts/metaf.py).

## Breadth first
Something that I don't like about metaf output is it's depth first instead of breadth first, and none of our sorting options can help with that. I would like to see first the top-level items before going deeper.

[Not possible with `os.walk`](https://stackoverflow.com/questions/49654234/is-there-a-breadth-first-search-option-available-in-os-walk-or-equivalent-py).

It does have a parameter called `topdown`.

> If optional argument topdown is True or not specified, the triple for a directory is generated before the triples for any of its subdirectories (directories are generated top-down). If topdown is False, the triple for a directory is generated after the triples for all of its subdirectories (directories are generated bottom-up). No matter the value of topdown, the list of subdirectories is retrieved before the tuples for the directory and its subdirectories are generated.

> When topdown is True, the caller can modify the dirnames list in-place (perhaps using del or slice assignment), and walk() will only recurse into the subdirectories whose names remain in dirnames; this can be used to prune the search, impose a specific order of visiting, or even to inform walk() about directories the caller creates or renames before it resumes walk() again. Modifying dirnames when topdown is False has no effect on the behavior of the walk, because in bottom-up mode the directories in dirnames are generated before dirpath itself is generated.  
—[*docs.python os.walk*](https://docs.python.org/3/library/os.html#os.walk)

I am confused by that description, nor do I understand the distinction between this and breadth-first, but it does have the effect that top-level folders get shown first.

`topdown=False`:
- resources/test/hi
- resources/test
- resources/mecha.png
- resources
- .gitignore
- game.c
- Makefile
- minshell.html

`topdown=True`:
- resources
- .gitignore
- game.c
- Makefile
- minshell.html
- resources/test
- resources/mecha.png
- resources/test/hi

I will add a topdown option for it and pass True/False to `os.walk` based on it. True by default.

```python
    parser.add_argument(
        '--topdown', action='store_true', default=True,
        help='Topdown argument passed to os.walk. '
        'True is passed by default, resulting in top-level files shown first '
        'before going into each (if no sort).')
```

Wait, it doesn't make sense to have a `store_true` argument with default true, no way to make it false. I guess it should be `--bottomup`?

```python
    parser.add_argument(
        '--bottomup', action='store_true', default=False,
        help='Pass topdown=False argument to os.walk. '
        'topdown=True is passed by default, resulting in top-level files '
        'shown first before going into each (if no sort).')
```
then:
```python
topdown = not args.bottomup
```

## Depth limit
I would like also the ability to limit the depth to recurse. [This SE answer by nosklo, 2008](https://stackoverflow.com/a/234329/18396947) does it by counting the separators in root and deletes `subdirs` when it's exceeded, which prevents them from being recursed into, which we could have also done earlier to avoid recursing into blacklisted folders instead of the startswith solution. But it only works when topdown is True, as was indicated in the documentation excerpt above ("Modifying dirnames when topdown is False has no effect on the behavior of the walk").

I'm going to take this approach anyway, because I'd like to keep using `os.walk` instead of writing my own function. Let's just say in the option help string that it only works when topdown is True, which is the default.

```python
    parser.add_argument(
        '--depth-limit', type=int,
        help='Avoid going deeper than a given level into nested subfolders. '
        'Only works when topdown is True (which is the default).')
```

It involves doing this before the loop:
```python
nsep = parent_path.count(os.path.sep)
```
then at the end of each iteration:
```python
        nsep_cur = root.count(os.path.sep)
        if limit is not None and nsep + limit <= nsep_cur:
            del subdirs[:]
```

What happens with negative values?
Seems to be same as 0, showing only top-level items.

[Permalink to state of metaf.py now](https://github.com/plu5/dotfiles/blob/a8ea4ad209e336fd9fb8869bc2a40c62ad11c536/pm/scripts/metaf.py)

[History](https://github.com/plu5/dotfiles/commits/main/pm/scripts/metaf.py)

{% include fin.html %}
