---
layout: post
title:  "On Emacs font-lock"
date:   2021-03-12
modified_date: 2022-05-08 22:21
categories: emacs
redirect_from:
  - /on-emacs-font-lock
---

In Emacs, syntax highlighting is handled by [Font Lock mode](https://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html), a minor mode which is enabled by default in major modes that support it, which define highlighting patterns to fontify.

You may want to add your own patterns to draw your attention to certain elements in the text, like todo comments; or the opposite, make elements stand out less, such as debug lines.

Additional patterns for a given major mode can be added to `font-lock-keywords-alist`. You can see the current value of it by doing `C-h v font-lock-keywords-alist`. 

{% include note.html content="Fontification of basic elements like comments and strings is done in a different way in Emacs; see [Syntax Tables](https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Tables.html)." %}

## Basic form
The keywords can take several forms. The most common form is

```
(matcher (subexp facespec))
```

Where you can have multiple `(subexp facespec)` (this is called a subexp-highlighter). If you have only one, the parentheses aren’t required.

- `matcher` can be a regexp or a function (more details on that in [Example 4: Function matcher](#example-4-function-matcher))
- `subexp` is an integer specifying which subexpression of the match to fontify (0 means the entire matching text)
- `facespec` is an expression whose value specifies the face (could be the face, a variable containing the face, or a function that returns the face)

More detailed information is on the manual [Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html), and `C-h v font-lock-keywords`.

It is easier to understand by looking at examples.

## Example 1: Regexp matcher

```elisp
("\\<\\(FIXME\\):" 1 'font-lock-warning-face)
```

- `"\\<\\(FIXME\\):"` is the `matcher`; in this case, a regexp pattern. `\<` signifies the beginning of a word, `\(` and `\)` deliminate the capture group.
- `1` is the capture group (subexp) to fontify. Here, `FIXME` is capture group 1 so only that will be fontified, without the `:` that follows. If put here `0` instead, `:` would be fontified as well.
- `font-lock-warning-face` is the face to fontify the matches with. You can see what it looks like, and other faces, in `M-x list-faces-display`. You can define your own with [`defface`](https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html).

To add your own keywords you can use `font-lock-add-keywords`. For example, to add this to Org mode:

```elisp
(font-lock-add-keywords
 'org-mode
 '(("\\<\\(FIXME\\):" 1 'font-lock-warning-face)))
```

## Your regexp friend: RE Builder
Emacs regexp is confusing, especially with all the escaping that has to take place; in elisp strings every backslash has to be escaped, so for special backslash you need `"\\"`, and for a literal backslash you need `"\\\\"`, and to make matters worse, backslashes in regexp are used way more often than other syntaxes of regular expressions due to its many [backslash constructs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Backslash.html).

Thankfully, since GNU Emacs 21[^emacswiki-reb] we have RE Builder: `M-x re-builder`. You type your regexp string in there and it highlights matches on the buffer you opened it from. If you want to find matches in another buffer, either call re-builder again from it, or press `C-c C-b` in the RE Builder buffer to change target buffer.

See also: [Emacs manual: Regular Expressions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Regular-Expressions.html).

## A note on precedence
You’ll notice a simple keyword may not apply in all contexts, such as in a string or comment.

> Ordinarily, once part of the text has been fontified already, this cannot be overridden by a subsequent match in the same text; but you can specify different behavior using the _override_ element of a _subexp-highlighter_.   
—[Emacs manual: Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html)

The behaviour can be specified by providing one of these flags after the facespec:

- `keep`: do not fontify if already fontified,
- `t`: override existing fontifications,
- `prepend`: add to the beginning; it is a sort of merging of previous and current fontification,
- `append`: add to the end; also a sort of merging, but if your face and the applied one modify the same attributes, the applied one takes precedence.

This optional argument is called the `override`.

You will see this in use in examples below.

## Example 2: Multiple capture groups
The following is an example of fontifying multiple capture groups in a regex matcher.

```elisp
(font-lock-add-keywords
 'org-mode
 '(("src_[^{]+{\\([^}]*\\)\\(}\\)"
    (0 'org-block-begin-line prepend)
    (1 'font-lock-constant-face prepend)
    (2 'org-block-end-line prepend))))
```

The purpose of this is to fontify Org mode single-line code snippets, which look like the following:

```python
src_python{print("hi")}
```

1. The '0' capture group refers to the whole match, but in practice if you fontify other capture groups differently, then this can be used to fontify be the bits that are outside any capture groups; the `src_[^{]+{` part in this case, which will match `src_python{`.
2. The '1' capture group is the part between the first set of `\( \)`: `[^}]*`. This matches anything subsequent that is not }; in the example `print("hi")`.
3. The '2' capture group is the part between the second set to `\( \)`: `}`. This matches the ending bracket.

{% include aside.html content="A note that is quite unrelated to font-lock: These Org one-line code snippets do not have syntax highlighting in the Org mode buffer sadly, but they do in the exported document, making them superior to using Org mode monospace, i.e. `~print(\"hi\")~`." %}

## Optional capture groups
Following [the precedence-determining optional argument discussed above](#a-note-on-precedence), there is another optional argument that can be given: `laxmatch`[^se-laxmatch].

```
(subexp facespec [override [laxmatch]])
```

If you are fontifying multiple capture groups, but one of them is optional and will not always be present in the construct you are fontifying, you should pass `t` after override or font-lock will raise an error when it can’t find this group.

> If _laxmatch_ is non-`nil`, it means there should be no error if there is no subexpression numbered _subexp_ in _matcher_. Obviously, fontification of the subexpression numbered _subexp_ will not occur. However, fontification of other subexpressions (and other regexps) will continue. If _laxmatch_ is nil, and the specified subexpression is missing, then an error is signaled which terminates search-based fontification.  
—[Emacs manual: Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html)

Here is an example for fontifying python debug logging lines in a subtle colour, except for the strings which are fontified the same as comments, but may not always be present.

```elisp
(font-lock-add-keywords
 'python-mode
 '(("\\<\\(logging\\|logger\\).debug(\\([\"'][^\"']*[\"']\\)?[^)]*)"
    (0 'file-name-shadow prepend)
    (2 'font-lock-comment-face prepend t) ; optional
    )))
```

If the `t` isn’t given, it will raise this error:

> No match 2 in highlight (2 'font-lock-comment-face prepend)

{% include todo.html content="This is not the best example, because it’d probably be better to do the string matcher as an anchor matcher, especially since there can be multiple strings in one logging line (strings side by side get combined in Python, and this is often used to break up a long string into multiple lines)." %}

## Example 3: Anchors
In a font-lock keyword specification, following the matcher and facespec, you can have anchored matchers; matchers that will only execute if the previous one matched, and will begin their search where the last one left off. They take the following form:

```
(anchored-matcher pre-form post-form
                        subexp-highlighters…)
```

- `anchored-matcher` is like `matcher` of the [basic form](#basic-form); it can be a regexp or a function.
- `pre-form` is run before `anchored-matcher`. If you return a number lesser than (before) where the point is at the time, it will move it to that position before matching. If you return a number greater than the point (after), it will use this as the limit of the search. So this can be used to move back the starting point, or change the limit of the search that is about to be performed.
- `post-form` is run after `anchored-matcher`.
- `subexp-highlighters` are as before, constructs where you provide the face to fontify different capture groups; each taking the form `(subexp facespec)`

Lindydancer describes anchored matchers as a “search within a search.”[^se-dfraw]

The following example is taken from a StackExchange question by Ptharien's Flame.[^se-dfraw] The game Dwarf Fortress comes with [raw files](http://dwarffortresswiki.org/index.php/DF2014:Raw_file) wherein are properties and parameters of things like items and creatures in the game.

within there are lines like this one:

```
[ATTACK:EDGE:5:1000:stab:stabs:NO_SUB:1000]
```

and the requirement is to fontify differently:

1. The opening bracket
2. The token, `ATTACK` in this case
3. Each colon
4. Each subsequent parameter in between the colons
5. The ending bracket

3 and 4 make it difficult to do this in a single regular expression. So instead, they can be done as an anchored matcher.

```elisp
(defconst dfraw-font-lock-basic
  '(("\\(\\[\\)\\([A-Z0-9_]+\\)"
     (1 dfraw-bracket-face t)           ; 1
     (2 dfraw-token-face t)             ; 2
     ("\\(:\\)\\([^][:]*\\)"            ; anchor matcher
      (save-excursion                   ; pre-form {
	(if (re-search-forward "\\]" (line-end-position) t)
            (point)
          nil))                         ; }
      nil                               ; post-form
      (1 dfraw-colon-face t)            ; 3
      (2 dfraw-unknown-face t)))        ; 4
    ("\\]" (0 dfraw-bracket-face t)))   ; 5
  "Basic font-lock matchers for Dwarf Fortress \"raw\" files.")
```

- The first matcher, `"\\(\\[\\)\\([A-Z0-9_]+\\)"` matches the opening bracket and token (1, 2) and encapsulates each within its own capture group so that they can be fontified separately.
- The second matcher, `"\\(:\\)\\([^][:]*\\)"`, you will notice is inside the parentheses of the same keyword still, so it is an anchored matcher. The pre-form is used here to move the limit of the search to the ending bracket. The regexp matches a colon, and the parameter up to the next colon, each encapsulated in their own capture group so that they can be fontified separately (3, 4). Font-lock will match all matching expressions from where the previous matcher left off until the limit of the search.
- The third matcher, `"\\]"`, is also an anchored matcher, used to fontify the ending bracket (5).

Check out Ptharien's Flame’s finished package of a major mode for highlighting Dwarf Fortress raw files: [dfraw-mode](https://github.com/pthariensflame/dfraw-mode). The code snippet above can be found on [L143](https://github.com/pthariensflame/dfraw-mode/blob/master/dfraw-mode.el#L143).

## Example 4: Function matcher
There are some use-cases where a simple regex will not do, for example if you only want to apply fontifications in particular contexts or conditionally. Instead of using a regular expression you can provide a function that performs the search and returns the match data.

> When _function_ is called, it receives one argument, the limit of the search; it should begin searching at point, and not search beyond the limit. It should return non-`nil` if it succeeds, and set the match data to describe the match that was found. Returning `nil` indicates failure of the search.

> Fontification will call _function_ repeatedly with the same limit, and with point where the previous invocation left it, until _function_ fails.

> —[Emacs manual: Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html)

Consider variables in bash strings:

```bash
echo "Current Directory: $PWD"
echo 'Current Directory: $PWD'
```

The `$PWD` in the double-quoted string should be fontified differently from the string to give “a clear visual indication that the variable will be expanded”[^se-sh]. In the single-quoted string, however, it will be treated as literal text, so it should be fontified like the rest of the string.

```elisp
(defun point-in-double-quoted-string-p ()
  "Non-nil if point is inside a double-quoted string."
  (let ((state (syntax-ppss)))
    (eq (nth 3 state) ?\")))

(defun match-sh-var-in-string (&optional limit)
  "Search for sh variables in LIMIT.
Keep searching if match is not in a double-quoted string.
Set `match-data' and return non-nil if found."
  (let (res)
    (while
        (and
         (setq res
               (re-search-forward
                "[^\\]\\(\\$\\)\\({?[[:alnum:]_]+}?\\|[-#?@!]\\)"
                limit t))
         (not (point-in-double-quoted-string-p))))
    res))

(font-lock-add-keywords
 'sh-mode
 '((match-sh-var-in-string (2 font-lock-variable-name-face prepend))))
```

When you call `re-search-forward`, it sets `match-data`. If your matcher function returns nil, nothing will be fontified regardless and _it skips the rest of the line_. If your function returns non-nil, what’s in `match-data` will be fontified, and the function gets called again if point is not at the end of the line.

The reason for the while loop, then, is to not return yet if it encounters a var that is not in a double-quoted string, as if it returns nil there it will not keep searching the rest of the line, and something like:

```bash
'$NOTVAR' "$VAR"
```

won’t get fontified properly, because it’ll move on to the next line after it sees the first var is not in a double-quoted string.

And the reason for saving the return value of `re-search-forward` in a temporary variable `res` is so that we can return it, so that we do not return nil if there is a match.

> The value of a ‘while’ form is always nil.

<small>Credit to [Lindydancer](https://emacs.stackexchange.com/users/897/lindydancer)[^se-sh] for coming up with this matcher function.</small>

If you want the `$` to be fontified too, fontify capture group 1:

```elisp
(font-lock-add-keywords
 'sh-mode
 '((match-sh-var-in-string (1 font-lock-variable-name-face prepend)
                           (2 font-lock-variable-name-face prepend))))
```

## Example 5: Multiline auto-identification
When a buffer is opened, font-lock considers the whole buffer, and so any multiline regexp matchers you have will be correctly fontified.

{% include attention.html content="Just remember that `.` in regexp does not match newline characters, so you may have to do something like `[.\n]` instead." %}

However, when the buffer is modified, font-lock tends to only consider the current line. You can see this visually with Lindydancer’s [highlight-refontification-mode](https://github.com/Lindydancer/highlight-refontification).

{% include note.html content="Also notice that if you stop editing for a while, font-lock takes the opportunity to refontify a larger section of the buffer." %}

Therefore, with the default behaviour, your multiline constructs will not update in realtime.

There are several ways to get around this, which I will outline in this and the following examples.

The first and laziest way is turning on the global variable `font-lock-multiline`:

> If the `font-lock-multiline` variable is set to t, Font Lock will try to add the `font-lock-multiline` property automatically on multiline constructs. This is not a universal solution, however, since it slows down Font Lock somewhat. It can miss some multiline constructs, or make the property larger or smaller than necessary.     
—[Emacs manual: Font Lock Multiline](https://www.gnu.org/software/emacs/manual/html_node/elisp/Font-Lock-Multiline.html)

Note that this variable is buffer-local, so you need to attach the setq to a hook, e.g.:

```elisp
(add-hook 'python-mode-hook
          (lambda () (setq font-lock-multiline t)))
```

Here is an example for fontifying python debug lines:

```elisp
(font-lock-add-keywords
 'python-mode
 '(("\\<\\(logging\\|logger\\).debug([^)]*)"
    (0 'file-name-shadow prepend))))

(add-hook 'python-mode-hook
          (lambda () (setq font-lock-multiline t)))
```

Regexp explanation: At the start of a word, match `logging` or `logger` followed by `.debug(`, then all subsequent characters (including newlines) that are not `)`, followed by `)`.

If you try this without the hook, you will notice that without it if you enter newlines in your construct interactively they do not get fontified unless you trigger a whole-buffer refontification by idling, re-enabling the mode, or calling `font-lock-fontify-buffer`.

With it, font-lock tries to figure out which lines belong to your construct and does a good job of that in this case most of the time. Not always, though: for example, try entering `logging.debug(`, enter one or a few newlines, then `)`. It doesn’t work, but again if you wait for a whole-buffer refontification it will. What happens is if font-lock does not apply the `font-lock-multiline` property to the whole region we are concerned with, it does not get considered in full when we edit it, so it cannot match.

## Example 6: Multiline facespec trick
Rather than having `font-lock-multiline` try to guess which lines are part of the construct, we could apply the property ourselves.

I have come across a clever way to do it which is used in `css-mode` for its selectors (see [L956](https://github.com/emacs-mirror/emacs/blob/3af9e84ff59811734dcbb5d55e04e1fdb7051e77/lisp/textmodes/css-mode.el#L956)).

To remind you, a subexp-highlighter has the following form:

```
(subexp facespec)
```

where “_facespec_ is an expression whose value specifies the face to use for highlighting.”[^manual-sbf]

Therefore, we could pass a function and be able to execute arbitrary code in the fontification phase. In this case, logic to apply `font-lock-multiline` text property where we want it. font-lock will run it and apply the return value as the face. 

```elisp
("^[ \t]*{" (0 (save-excursion
                 (goto-char (match-beginning 0))
                 (skip-chars-backward " \n\t")
                 (put-text-property (point) (match-end 0)
                                    'font-lock-multiline t)
                 ;; No face.
                 nil)))
```

{% include note.html content="In this example they pass ``nil`` to apply no face. You could put a face symbol here instead -- same as you would when giving the facespec normally -- and the keyword will be fontified with that face." %}

[//]: # (i want to be able to find an example of my own. was thinking of doing the log example from earlier in this way but not sure how to do that really because we wouldn’t know what chars to skip backwards. i guess you’d have to keep going backwards checking that you don’t meet an open parenthesis each time? sounds bad performance-wise. i guess you could just search backwards instead for the open paren or something? it would be easier if i could find or think of an example more like the css one)

This facespec trick is useful beyond just multiline stuff, to any sort of logic you might want to do in the fontification phase. For example, out "Approach 3" in [this](https://emacs.stackexchange.com/a/71561/15886) SE answer I wrote:

```elisp
(defvar custom-cpp-keywords-record nil
  "Transient list of positions of custom cpp keywords in the buffer.")

(font-lock-add-keywords
 'c++-mode
 '(("^\\(trn\\|def\\|mem\\)\\>" 0       ; regexp, subexp
    (progn                              ; facespec {
      (add-to-list 'custom-cpp-keywords-record
                   (cons (match-beginning 0) (match-end 0)))
      'font-lock-keyword-face)          ; }
    prepend)))                          ; override (optional)
```

In the facespec here the positions of the keywords are saved into a variable, so in addition to fontifying our keywords we also get a list of positions we could use from another function.

## One step behind/ahead problem
Font-lock removes the multiline property each time after fontification, so it must be reinstalled. Applying it in the keyword facespec is sort of ‘one step behind’; since the multiline property is applied at the fontification step, it is too late to fontify it until the next time fontification is done. That means if you write the construct that is supposed to be fontified, it does not get fontified immediately, but only after you type one other character or do something else that triggers refontification.

Instead of doing it in the facespec you could hook into something that gets called before fontification and add the property there, but then you run into the opposite problem -- being ‘one step ahead’ -- which means that when the multiline construct is no longer there it does not get unfontified; that part of the buffer does not get reconsidered, because by then the multiline property is no longer there.

You can see this visually with Lindydancer’s [highlight-refontification](https://github.com/Lindydancer/highlight-refontification).

[//]: # (Also could have a section about the lunaryorn complicated method for applying properties.)

## Example 7: Multiline pre-form trick

[//]: # (this section is still a bit messy because it’s old and in copyediting i sort of left it as is despite what was previously before and after having changed. so what the code is trying to accomplish for example has not been explained. i.e. what it is attempting to fontify.)

Instead of several groups in one regexp, you can have several matchers, one after the other, each one acting as anchor to the next; font-lock searches for matches where it left off from the previous one (see [Example 3: Anchors](#example-3-anchors)).

```elisp
(font-lock-add-keywords
 'python-mode
 '(("logger.debug(" (0 'file-name-shadow prepend)
    ("\"[^\"]*\"" nil nil (0 'font-lock-comment-face prepend))
    ("[^)]*)" nil nil (0 'file-name-shadow prepend)))))
```

Notice that unlike in the single regex approach, all subsequent matchers become optionals, so that if it does not find the second it goes to the third.

The `nil nil` following the second and third matchers are the _pre-form_ and _post-form_.

> The forms _pre-form_ and _post-form_ can be used to initialize before, and cleanup after, _anchored-matcher_ is used. Typically, _pre-form_ is used to move point to some position relative to the match of _matcher_, before starting with _anchored-matcher_. _post-form_ might be used to move back, before resuming with _matcher_.

> After Font Lock evaluates _pre-form_, it does not search for _anchored-matcher_ beyond the end of the line. However, if _pre-form_ returns a buffer position that is greater than the position of point after _pre-form_ is evaluated, then the position returned by _pre-form_ is used as the limit of the search instead. It is generally a bad idea to return a position greater than the end of the line; in other words, the _anchored-matcher_ search should not span lines.

> —[Emacs manual: Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html)

Never mind that last line; we’re going to do exactly that.

We can use a function as the pre-form that will give the position of the next line that ends with `)`. For this to work, we also need to set `font-lock-multiline` to `t`.

```elisp
(defun end-of-parens-position ()
  "Return position of the next line that ends with ), unless current line ends with ) in which case return position of end of line."
  (save-excursion
    (goto-char (point-at-eol))
    (while (not (or (eobp)              ; end of buffer
                    (and (bolp) (eolp)) ; end of line
                    (char-equal (char-before) ?\))))
      (forward-line)
      (goto-char (point-at-eol)))
    (point)))

(font-lock-add-keywords
 'python-mode
 '(("\\<logger\\.debug(" (0 'file-name-shadow t)
    ("\".*\"" nil nil (0 'font-lock-comment-face t))
    (".*)" (end-of-parens-position) nil (0 'file-name-shadow t)))))

(add-hook 'python-mode-hook
          (lambda () (setq font-lock-multiline t)))
```

Now the example works, though it would not if you have linebreaks in the `""`. If you want to go further, you can add another function to find the closing quote:

```elisp
(defun end-of-quotes-position ()
  "Return position of the next \" or '."
  (save-excursion
    (if (not (or (char-equal (char-before) ?\")
                  (char-equal (char-before) ?\')))
        (re-search-forward "[\"']" nil t))))

(font-lock-add-keywords
 'python-mode
 '(("\\<logger\\.debug(" (0 'file-name-shadow t)
    ("[\"'][^\"']*[\"']?" nil nil (0 'font-lock-comment-face t))
    ("[^\"']*[\"']" (end-of-quotes-position) nil (0 'font-lock-comment-face t))
    ("[^)]*)" (end-of-parens-position) nil (0 'file-name-shadow t)))))
```

This could be rewritten to be more robust, to make sure we found the proper end to the string, allow escaping inside the string, etc.

Some notes about writing pre-form functions:

- Unlike matcher functions, here you do not want to move the point (wrap in `save-excursion` if you need to). This will mess up font-lock fontifying.
- Be careful not to get stuck in an infinite loop. For example if I didn’t put the end of buffer check in my while loop exit conditions, it’d get into an infinite loop in some situations and hang Emacs, which can be really annoying and difficult to diagnose if you’re not using a Unix-like system.

## Example 8: Multiline by extend region

[//]: # (this also needs some more explanation as it was just taken directly from an SE answer draft I wrote a long time ago and not edited much further.)

Here I wrote an extend region function that extends the beginning of it to the beginning of the line with the open parenthesis. That way if a closing parenthesis is inserted in a later line, font-lock does not only look at that line but back to the line with the open parenthesis (if it exists), so that our matcher can make an informed decision whether it needs to fontify the log.

{% include note.html content="Or you can just ignore the ending parenthesis and only match the starting ones. Plenty of modes do that, e.g. CSS Mode for selectors." %}

```elisp
(font-lock-add-keywords 'python-mode
                            '(("\\<\\(log\\)([^)]*)" 1 font-lock-builtin-face)))

(defun font-lock-extend-parens-region ()
  "Extend font-lock region for parentheses on different lines."
  (save-excursion
    (let (res)
      ;; go to start of region
      (goto-char font-lock-beg)
          ;; if inside parentheses
      (when (> (elt (syntax-ppss) 0) 0)
        ;; go to the starting parenthesis
        (ignore-errors (up-list -1))
        (beginning-of-line)
        ;; if point is before the region
        (when (setq res (< (point) font-lock-beg))
          ;; extend the beginning of the region
          (setq font-lock-beg (point))))
      ;; go to end of region
      (goto-char font-lock-end)
      ;; if inside parentheses
      (when (> (elt (syntax-ppss) 0) 0)
        ;; go to the ending parenthesis
        (ignore-errors (up-list))
        ;; if point is after the region
        (when (> (point) font-lock-end)
          ;; extend the end of the region
          (setq font-lock-end (point))
          (setq res t)))
      res)))

(defun add-font-lock-extend-parens-region-to-hook ()
  "Add `font-lock-extend-parens-region' function to `font-lock-extend-region-functions' hook."
  (add-hook 'font-lock-extend-region-functions
            #'font-lock-extend-parens-region))

(add-hook 'python-mode-hook #'add-font-lock-extend-parens-region-to-hook)
```

`font-lock-extend-region-functions` is a hook that is run before a region is fontified. Functions added to it get no arguments, but can still access the bounds of the region with the dynamically-bound variables `font-lock-beg` and `font-lock-end`. It is buffer-local, hence adding it in the python-mode-hook.

It expects you to return non-nil if you modified the bounds, nil otherwise. If you fail to return nil, you can end up in an infinite loop (it is kind of similar to font-lock-keywords pre-form functions in that respect), so you should write this function carefully, with the knowledge that after you modify the bounds of the region the function will be called again, and provided there is nothing further to do, it should return nil.

There is another, easier way to do the same thing, but it is more destructive, so it makes more sense if you are writing your own mode rather than user customisation: making your own `font-lock-fontify-region-function`. In this way you can modify the bounds of the region, received as arguments, and then call the default fontify region function at the end with the new bounds.

```elisp
(font-lock-add-keywords 'python-mode
                        '(("\\<\\(log\\)([^)]*)" 1 font-lock-builtin-face)))
                        
(defun font-lock-extend-parens-region (beg end loudly)
  "Extend font-lock region for parentheses on different lines."
  (save-excursion
    ;; go to start of region
    (goto-char beg)
    ;; if inside parentheses
    (when (> (elt (syntax-ppss) 0) 0)
      ;; go to the starting parenthesis
      (ignore-errors (up-list -1))
      (beginning-of-line)
      ;; if point is before the region
      (when (< (point) beg)
        ;; extend the beginning of the region
        (setq beg (point))))
    ;; go to end of region
    (goto-char end)
    ;; if inside parentheses
    (when (> (elt (syntax-ppss) 0) 0)
      ;; go to the ending parenthesis
      (ignore-errors (up-list))
      ;; if point is after the region
      (when (> (point) end)
        ;; extend the end of the region
        (setq end (point)))))
  ;; call the default fontify region function with the new bounds
  (font-lock-default-fontify-region beg end loudly))

(defun set-font-lock-extend-parens-region-as-fontify-region-function ()
  "Set `font-lock-extend-parens-region' as `font-lock-fontify-region-function'."
  (set (make-local-variable 'font-lock-fontify-region-function)
       #'font-lock-extend-parens-region))

(add-hook 'python-mode-hook
          #'set-font-lock-extend-parens-region-as-fontify-region-function)
```
 [cperl-mode](https://git.savannah.gnu.org/cgit/emacs.git/plain/lisp/progmodes/cperl-mode.el) takes this approach; see `cperl-font-lock-fontify-region-function`.

It may look about the same as the other approach, but when developing your own extend region function, this way is much easier to get working, since it is so easy to mess up the return values in the other way and get unintended results, whereas here, you can modify the bounds of the region to your heart’s content without having to worry about anything else. I think it is a good idea then to start with this method and convert to the other once you’ve got it working.

## Performance considerations

Adding font-lock keywords is necessarily sacrificing performance for looks. You can mitigate the performance impact by benchmarking, finding less costly ways to do the same thing, and avoiding unnecessary complexity where possible.

Poorly written matcher and pre-form functions can lead to poor performance or hangs, and ``C-h v font-lock-keywords`` seems to suggest regexp patterns could cause major problems too:

> Be careful when composing these regular expressions; a poorly written pattern can dramatically slow things down! The function regexp-opt (see [Regexp Functions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Regexp-Functions.html)) is useful for calculating optimal regular expressions to match several keywords.     
—[Emacs manual: Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html)

All of the packages by Lindydancer are really excellent for font-lock debugging:

- [font-lock-studio](https://github.com/Lindydancer/font-lock-studio) is a step-by-step debugger for font-lock. This is very helpful to see what font-lock is actually doing, which may be something other than you expect. Run it in a buffer with your fontified text: `M-x font-lock-studio`, and press space to step through. Watch the echo area for information. For more options, look at the Font Lock Studio menu entries, or press `?`.
- [font-lock-profiler](https://github.com/Lindydancer/font-lock-profiler).
- [highlight-refontification](https://github.com/Lindydancer/highlight-refontification) highlights the parts of the region that are being considered by font-lock in each fontification. Very useful for debugging multiline stuff.

They are all on MELPA so you can easily install them with `M-x package-install`. You may need to first do `M-x package-refresh-contents` to update the list of packages your Emacs knows about.

Check out the rest of his [GitHub repositories](https://github.com/Lindydancer?tab=repositories); beyond useful tools he’s also made major modes with syntax highlighting for various languages and there is a lot to learn from reading his code.

There is also the good old [Emacs profiler](https://www.gnu.org/software/emacs/manual/html_node/elisp/Profiling.html), which you can use to profile the performance of your matcher and pre-form functions.

## Further reading

There’s a lot about multiline that I still don’t understand. Some stuff to read:

- [StackOverflow: is there a clear example of multi-line font-locking?](https://stackoverflow.com/questions/9452615/emacs-is-there-a-clear-example-of-multi-line-font-locking)
- [Emacs manual: Multiline Font Lock Constructs](https://www.gnu.org/software/emacs/manual/html_node/elisp/Multiline-Font-Lock.html) and child pages

{% include todo.html content="Add more to this in future!" %}

I think it’s super useful if you are not sure how to tackle something to read the code of existing modes for ideas. For instance you can read the source code for progmodes included with emacs [here](https://github.com/emacs-mirror/emacs/tree/master/lisp/progmodes).

You can also post on [Emacs StackExchange](https://emacs.stackexchange.com/) with the `font-lock` tag. I monitor this tag and try to help when I can. (<https://emacs.stackexchange.com/questions/tagged/font-lock>)


[^emacswiki-reb]: [EmacsWiki: Re Builder](https://www.emacswiki.org/emacs/ReBuilder)
[^se-sh]: [Emacs SE: Highlighting shell variables within quotes](https://emacs.stackexchange.com/q/13128/15886)
[^se-laxmatch]: [StackOverflow: Emacs syntax highlighting of optional parts](https://stackoverflow.com/a/13229626)
[^se-dfraw]: [Emacs SE: How to match more than one instance of a single subexpression?](https://emacs.stackexchange.com/q/20168/15886)
[^manual-sbf]: [Emacs manual: Search-based Fontification](https://www.gnu.org/software/emacs/manual/html_node/elisp/Search_002dbased-Fontification.html)

{% include fin.html %}
